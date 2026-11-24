{-# LANGUAGE OverloadedStrings #-}

-- | Performance benchmarks for the paths a user exercises: streaming a model
-- reply through the real Markdown/ANSI presenter, capturing and streaming
-- large shell output, and the editor and prompt reducers.
--
-- Run with @cabal bench@ or through the flake's @benchmark@ check. The @llm@
-- group starts a local mock server, so its numbers include only Hutian's
-- request, rendering, and output handling, not a real model.
module Main (main) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forever, unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (toLower)
import qualified Data.IntMap.Strict as IntMap
import Data.List (intercalate)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Text as Text
import Hutian.Context (TurnContext (MainContext))
import Hutian.Editor
  ( InputMode (..)
  , Prompt
  , activePrompt
  , applyKey
  , cursorPosition
  , initialPrompt
  , initialPromptBuffers
  , physicalRows
  , promptText
  , setPromptText
  )
import Hutian.Highlight
  ( codeStyle
  , highlightJsonText
  , highlightLines
  , highlightShellLine
  , sanitizeOutput
  , sanitizeOutputBytes
  )
import Hutian.History (History (History))
import Hutian.Interrupt (InterruptFlag, newInterruptFlag)
import Hutian.Key (Key (..))
import Hutian.Llm
  ( LlmOutput (..)
  , LlmSettings (..)
  , LlmUrl (..)
  , ModelName (..)
  , SystemPrompt (..)
  , apiFor
  , runLlm
  )
import Hutian.Llm.Stream (ToolCallDelta (..), mergeToolCallDeltas, parseStreamLine)
import Hutian.Markdown
  ( MarkdownFlavor (..)
  , initialMarkdownState
  , renderMarkdownRows
  , renderMarkdownWith
  )
import Hutian.Prompt
  ( PromptState
  , PromptStep (..)
  , initialPromptState
  , promptBuffers
  , stepPrompt
  )
import Hutian.Render (grayOutputText, trailingNewline)
import Hutian.Shell
  ( CommandResult (..)
  , commandResultText
  , executableCommands
  , runShellCommandOutput
  , runShellCommandStreaming
  )
import Hutian.Tool.Catalog (toolCatalog)
import Hutian.Tool.Config (defaultToolConfig)
import Hutian.Verbosity (Verbosity (..))
import Network.Socket
  ( Family (AF_INET)
  , SockAddr (SockAddrInet)
  , Socket
  , SocketType (Stream)
  , accept
  , bind
  , close
  , defaultProtocol
  , listen
  , socket
  , socketPort
  , tupleToHostAddress
  )
import Network.Socket.ByteString (recv, sendAll)
import System.IO
  ( Handle
  , IOMode (WriteMode)
  , hFlush
  , hPutStr
  , hPutStrLn
  , openFile
  , stderr
  )
import Test.Tasty.Bench (Benchmark, bench, bgroup, defaultMain, nf, nfIO, whnfIO)

main :: IO ()
main = do
  ttftPort <- startMockServer (sseBody ["hello"])
  fastPort <- startMockServer fastReplyBody
  flag <- newInterruptFlag
  -- The sink stands in for the terminal: every write is encoded and flushed,
  -- so the rendered text and the per-piece output cost are both measured.
  sink <- openFile "/dev/null" WriteMode
  defaultMain
    [ pureBenchmarks
    , startupBenchmarks
    , shellBenchmarks sink
    , llmBenchmarks sink ttftPort fastPort flag
    ]

pureBenchmarks :: Benchmark
pureBenchmarks =
  bgroup
    "pure"
    [ markdownBenchmarks
    , highlightBenchmarks
    , streamBenchmarks
    , editorBenchmarks
    ]

markdownBenchmarks :: Benchmark
markdownBenchmarks =
  bgroup
    "markdown"
    [ bench "render multi-KiB reply" $
        nf (snd . renderMarkdownWith NormalMarkdown initialMarkdownState) markdownText
    , bench "render rows with a fenced block" $
        nf renderMarkdownRows markdownRows
    ]

highlightBenchmarks :: Benchmark
highlightBenchmarks =
  bgroup
    "highlight"
    [ bench "haskell source" $
        nf (highlightLines codeStyle "haskell") haskellLines
    , bench "shell command" $
        nf highlightShellLine shellCommand
    , bench "json arguments" $
        nf highlightJsonText jsonText
    ]

streamBenchmarks :: Benchmark
streamBenchmarks =
  bgroup
    "stream"
    [ bench "parse SSE lines" $
        nf (length . mapMaybe parseStreamLine) streamLines
    , bench "merge tool-call deltas" $
        nf (IntMap.size . mergeToolCallDeltas IntMap.empty) toolDeltas
    ]

editorBenchmarks :: Benchmark
editorBenchmarks =
  bgroup
    "editor"
    [ bench "editor keystrokes" $
        nf
          (length . promptText . foldl' (flip applyKey) (setPromptText "" initialPrompt))
          editorKeys
    , bench "prompt keystrokes" $
        nf
          ( length
              . promptText
              . activePrompt LlmInput
              . promptBuffers
              . foldl' advancePrompt (initialPromptState LlmInput initialPromptBuffers)
          )
          promptKeys
    , bench "layout a 500-line prompt" $
        nf layoutCost largePrompt
    ]

-- | Feed one key through the prompt reducer and keep the resulting state.
advancePrompt :: PromptState -> Key -> PromptState
advancePrompt state key =
  case stepPrompt (History [] []) key state of
    StepContinue next -> next
    StepRedraw next -> next
    StepMoveCursor _ next -> next
    StepComplete next -> next
    StepFinish _ _ next -> next

-- | The layout work a redraw does: wrapping every row plus locating the cursor.
layoutCost :: Prompt -> Int
layoutCost prompt =
  let (row, column) = cursorPosition layoutWidth prefix prompt
   in length (physicalRows layoutWidth prefix prompt) + row + column
  where
    layoutWidth = 80
    prefix = "> "

largePrompt :: Prompt
largePrompt =
  setPromptText
    ( unlines
        [ "line " ++ show number ++ ": the quick brown fox jumps over the lazy dog"
        | number <- [1 :: Int .. 500]
        ]
    )
    initialPrompt

-- | Startup work outside the sandbox: the completion catalog scans every
-- directory on PATH. A one-shot @hutian -- command@ bypasses this.
startupBenchmarks :: Benchmark
startupBenchmarks =
  bgroup
    "startup"
    [ bench "scan PATH for completions" $
        nfIO executableCommands
    ]

shellBenchmarks :: Handle -> Benchmark
shellBenchmarks sink =
  bgroup
    "shell"
    [ bench "capture 64 KiB tool output" $
        whnfIO (captureToolOutput sink 65536)
    , bench "capture 256 KiB tool output" $
        whnfIO (captureToolOutput sink 262144)
    , bench "capture 1 MiB tool output" $
        whnfIO (captureToolOutput sink 1048576)
    , bench "stream 1 MiB tool output" $
        whnfIO (streamToolOutput sink 1048576)
    ]

-- | The model shell tool captures output, sends it to the model, and formats it
-- for display when output is visible.
captureToolOutput :: Handle -> Int -> IO ()
captureToolOutput sink bytes = do
  result <- runShellCommandOutput "/" (largeOutputCommand bytes)
  case result of
    Left message -> hPutStrLn stderr message
    Right output -> do
      hPutStr sink (commandResultText output)
      hPutStr
        sink
        (grayOutputText True (trailingNewline (sanitizeOutput (commandStdout output))))

-- | Term mode streams output through the sanitizer to the terminal.
streamToolOutput :: Handle -> Int -> IO ()
streamToolOutput sink bytes = do
  result <-
    runShellCommandStreaming sanitizeOutputBytes sink sink "/" (largeOutputCommand bytes)
  either (hPutStrLn stderr) pure result

largeOutputCommand :: Int -> String
largeOutputCommand bytes = "yes x | head -c " ++ show bytes

llmBenchmarks :: Handle -> Int -> Int -> InterruptFlag -> Benchmark
llmBenchmarks sink ttftPort fastPort flag =
  bgroup
    "llm"
    [ bench "first token (mock server)" $
        whnfIO (runLlm output flag (settingsFor ttftPort) [] "hello")
    , bench "stream 2000 chunks with rendering" $
        whnfIO (runLlm output flag (settingsFor fastPort) [] "hello")
    ]
  where
    output = sinkOutput sink

-- | A sink with the same shape as the console writer: every piece is encoded
-- and flushed, and an error would still be reported.
sinkOutput :: Handle -> LlmOutput
sinkOutput sink =
  LlmOutput
    { llmOutputWrite = \text -> unless (null text) (hPutStr sink text >> hFlush sink)
    , llmOutputErrorLine = hPutStrLn stderr
    , llmOutputContext = MainContext
    , llmOutputVerbosity = VerbosityHigh
    }

settingsFor :: Int -> LlmSettings
settingsFor port =
  LlmSettings
    { settingUrl = url
    , settingApi = apiFor url
    , settingModel = ModelName "bench-model"
    , settingSystemPrompt = SystemPrompt "bench system prompt"
    , settingSocket = Nothing
    , settingVerbosity = VerbosityHigh
    , settingTools = toolCatalog defaultToolConfig
    }
  where
    url = LlmUrl ("http://127.0.0.1:" ++ show port ++ "/v1/chat/completions")

-- | A short reply for the first-token measurement.
sseBody :: [String] -> BS.ByteString
sseBody chunks = BS8.pack (concatMap frame chunks ++ "data: [DONE]\n\n")
  where
    frame chunk =
      "data: {\"choices\":[{\"delta\":{\"content\":\""
        ++ concatMap escapeChar chunk
        ++ "\"}}]}\n\n"

-- | 2000 small pieces, about what a fast local model emits token by token.
-- Lines, emphasis, links, and a fenced block make the presenter do real work.
fastReplyBody :: BS.ByteString
fastReplyBody = sseBody (take 2000 (cycle pieces))
  where
    pieces =
      [ "The "
      , "quick "
      , "brown "
      , "**fox** "
      , "jumps "
      , "over "
      , "the "
      , "lazy "
      , "`dog` "
      , "and "
      , "then "
      , "runs.\n"
      , "A "
      , "second "
      , "line "
      , "with "
      , "a "
      , "[link](https://example.test).\n"
      , "```haskell\n"
      , "main "
      , "= "
      , "putStrLn "
      , "\"hello\"\n"
      , "```\n"
      ]

escapeChar :: Char -> String
escapeChar '"' = "\\\""
escapeChar '\\' = "\\\\"
escapeChar '\n' = "\\n"
escapeChar '\r' = "\\r"
escapeChar character = [character]

-- | A mock OpenAI-compatible SSE server. It serves every request with the same
-- body, so tasty-bench can run a benchmark repeatedly.
mockResponse :: BS.ByteString -> BS.ByteString
mockResponse body =
  BS8.pack
    ( "HTTP/1.1 200 OK\r\n"
        ++ "Content-Type: text/event-stream\r\n"
        ++ "Content-Length: "
        ++ show (BS.length body)
        ++ "\r\n"
        ++ "Connection: close\r\n\r\n"
    )
    <> body

startMockServer :: BS.ByteString -> IO Int
startMockServer body = do
  listener <- socket AF_INET Stream defaultProtocol
  bind listener (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  listen listener 16
  port <- fromIntegral <$> socketPort listener
  _ <- forkIO (acceptLoop body listener)
  pure port

acceptLoop :: BS.ByteString -> Socket -> IO ()
acceptLoop body listener =
  forever $ do
    (connection, _) <- accept listener
    _ <- forkIO (serveConnection body connection)
    pure ()

serveConnection :: BS.ByteString -> Socket -> IO ()
serveConnection body connection = do
  _ <- try (receiveRequest connection) :: IO (Either SomeException ())
  _ <- try (sendAll connection (mockResponse body)) :: IO (Either SomeException ())
  close connection

-- | Read the complete request so closing the socket does not reset the
-- connection before the response reaches the client.
receiveRequest :: Socket -> IO ()
receiveRequest connection = do
  headers <- receiveHeaders connection BS.empty
  let (_, afterHeaders) = BS.breakSubstring "\r\n\r\n" headers
      bodySoFar = BS.drop 4 afterHeaders
  drainBody connection (contentLength headers - BS.length bodySoFar)

receiveHeaders :: Socket -> BS.ByteString -> IO BS.ByteString
receiveHeaders connection accumulated
  | "\r\n\r\n" `BS.isInfixOf` accumulated = pure accumulated
  | otherwise = do
      chunk <- recv connection 4096
      if BS.null chunk
        then pure accumulated
        else receiveHeaders connection (accumulated <> chunk)

drainBody :: Socket -> Int -> IO ()
drainBody _ remaining | remaining <= 0 = pure ()
drainBody connection remaining = do
  chunk <- recv connection 4096
  if BS.null chunk
    then pure ()
    else drainBody connection (remaining - BS.length chunk)

contentLength :: BS.ByteString -> Int
contentLength headers =
  fromMaybe 0 (listToMaybe (mapMaybe parseLine (BS8.lines headers)))
  where
    parseLine line =
      case BS8.break (== ':') line of
        (name, value)
          | BS8.map toLower name == "content-length" ->
              readInt (BS8.dropWhile (== ' ') (BS8.drop 1 value))
        _ -> Nothing
    readInt :: BS.ByteString -> Maybe Int
    readInt value =
      case reads (BS8.unpack value) of
        [(number, "")] -> Just number
        _ -> Nothing

markdownRows :: [String]
markdownRows =
  [ "# Benchmark"
  , ""
  , "A paragraph with **bold**, *italic*, `code`, and a [link](https://example.test)."
  , ""
  ]
    ++ concat
      [ [ "- item " ++ show i ++ " with `inline code` and **emphasis**"
        , "  continued text for item " ++ show i
        ]
      | i <- [1 :: Int .. 40]
      ]
    ++ [ "```haskell"
       , "main :: IO ()"
       , "main = putStrLn \"hello\""
       , "```"
       ]

markdownText :: Text.Text
markdownText = Text.pack (unlines markdownRows)

haskellLines :: [String]
haskellLines =
  [ "module Bench where"
  , ""
  , "import Data.List (foldl')"
  , ""
  ]
    ++ concat
      [ [ "f" ++ show i ++ " :: Int -> Int"
        , "f" ++ show i ++ " x = foldl' (+) x [" ++ show i ++ " .. " ++ show (i + 10) ++ "]"
        , ""
        ]
      | i <- [1 :: Int .. 20]
      ]

shellCommand :: String
shellCommand =
  "if true; then echo \"hello\" | grep -q hello && printf '%s\\n' done; else exit 1; fi"

jsonText :: String
jsonText =
  "{\n"
    ++ intercalate
      ",\n"
      [ "  \"field" ++ show i ++ "\": \"value " ++ show i ++ "\""
      | i <- [1 :: Int .. 50]
      ]
    ++ "\n}"

streamLines :: [BS.ByteString]
streamLines =
  [ BS8.pack
      ( "data: {\"choices\":[{\"delta\":{\"content\":\"token "
          ++ show i
          ++ "\"}}]}"
      )
  | i <- [1 :: Int .. 100]
  ]
    ++ [ "data: {\"choices\":[{\"delta\":{\"reasoning_content\":\"thinking\"}}]}"
       , "data: {\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"call-1\",\"function\":{\"name\":\"shell\",\"arguments\":\"{}\"}}]}}]}"
       , "data: [DONE]"
       ]

toolDeltas :: [ToolCallDelta]
toolDeltas =
  concat
    [ [ ToolCallDelta i (Just (Text.pack ("call-" ++ show i))) (Just "shell") Nothing
      , ToolCallDelta i Nothing Nothing (Just "{\"command\":")
      , ToolCallDelta i Nothing Nothing (Just "\"ls\"}")
      ]
    | i <- [0 :: Int .. 9]
    ]

editorKeys :: [Key]
editorKeys =
  take
    400
    (cycle [KeyChar 'a', KeyChar 'b', KeyChar 'c', KeyEnter, KeyBackspace, KeyLeft, KeyRight])

promptKeys :: [Key]
promptKeys =
  take
    400
    (cycle [KeyChar 'a', KeyChar 'b', KeyEnter, KeyBackspace, KeyLeft, KeyRight, KeyHome, KeyEnd])
