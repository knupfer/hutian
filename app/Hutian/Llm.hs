{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Minimal streaming client for llama.cpp.
module Hutian.Llm
  ( runLlm
  , runLlmWithTools
  , runLlmContinuation
  , LlmSettings (..)
  , parseLlmSettings
  , LlmResult (..)
  , emptyLlmResult
  , Api (..)
  , apiFor
  , requestBodyFor
  , continuationBodyFor
  , toolDefinitions
  , toolCommand
  , StreamEvent (..)
  , ToolCallDelta (..)
  , parseStreamLine
  , splitLines
  , extractContent
  , extractReasoning
  , extractError
  , extractToolCalls
  , mergeToolCallDeltas
  , isStop
  , decode
  ) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe, isNothing, listToMaybe, mapMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncodingError
import qualified Data.Vector as Vector
import Data.Word (Word8)
import Hutian.Conversation
  ( ChatMessage (..)
  , Conversation
  , ToolCall (..)
  , completionContinuationPrompt
  , completionPrompt
  , continuationMessages
  , requestMessages
  , roleName
  )
import Hutian.Interrupt (InterruptFlag, isInterrupted, withInterruptFlag)
import Hutian.Markdown
  ( MarkdownState
  , finishMarkdown
  , finishMarkdownWith
  , initialMarkdownState
  , renderMarkdown
  , renderMarkdownWith
  )
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import System.IO (hFlush, hPutStrLn, stderr, stdout)

-- | The result of one streaming LLM request.
data LlmResult = LlmResult
  { llmResponseText :: Maybe String
  , llmToolCalls :: [ToolCall]
  }
  deriving stock (Eq, Show)

emptyLlmResult :: LlmResult
emptyLlmResult = LlmResult Nothing []

-- | Run one LLM request for a user prompt, returning any tool calls that the
-- model requested. Kept as a convenience wrapper for callers that only need
-- the assistant text.
runLlm :: InterruptFlag -> LlmSettings -> Conversation -> String -> IO (Maybe String)
runLlm interruptFlag settings conversation prompt =
  llmResponseText <$> runLlmWithTools interruptFlag settings conversation prompt

runLlmWithTools :: InterruptFlag -> LlmSettings -> Conversation -> String -> IO LlmResult
runLlmWithTools interruptFlag settings conversation prompt
  | all isSpace prompt = pure emptyLlmResult
  | otherwise =
      runRequest interruptFlag settings $
        requestBodyFor
          (apiFor (settingUrl settings))
          (settingModel settings)
          (settingSystemPrompt settings)
          conversation
          prompt

-- | Continue a conversation after tool results have been appended.
runLlmContinuation :: InterruptFlag -> LlmSettings -> Conversation -> IO LlmResult
runLlmContinuation interruptFlag settings conversation =
  runRequest interruptFlag settings $
    continuationBodyFor
      (apiFor (settingUrl settings))
      (settingModel settings)
      (settingSystemPrompt settings)
      conversation

data LlmSettings = LlmSettings
  { settingUrl :: String
  , settingModel :: String
  , settingSystemPrompt :: String
  }
  deriving stock (Eq, Show)

-- | Parse the launcher-provided connection settings. All three options are
-- required so the shell never falls back to ambient environment variables.
parseLlmSettings :: [String] -> Either String LlmSettings
parseLlmSettings = go Nothing Nothing Nothing
  where
    go (Just url) (Just model) (Just systemPrompt) [] =
      Right (LlmSettings url model systemPrompt)
    go url model systemPrompt [] =
      Left
        ( "missing required argument"
            ++ plural
            ++ ": "
            ++ unwords (missingArguments url model systemPrompt)
            ++ "\n"
            ++ llmUsage
        )
      where
        plural =
          if length (missingArguments url model systemPrompt) > 1
            then "s"
            else ""
    go _ _ _ ["--llama-url"] = missingValue "--llama-url"
    go _ _ _ ["--model"] = missingValue "--model"
    go _ _ _ ["--system-prompt"] = missingValue "--system-prompt"
    go _ model systemPrompt ("--llama-url" : value : rest) =
      go (Just value) model systemPrompt rest
    go url _ systemPrompt ("--model" : value : rest) =
      go url (Just value) systemPrompt rest
    go url model _ ("--system-prompt" : value : rest) =
      go url model (Just value) rest
    go _ _ _ (argument : _) =
      Left ("unknown argument: " ++ argument ++ "\n" ++ llmUsage)

    missingValue flag = Left ("missing value for " ++ flag ++ "\n" ++ llmUsage)

missingArguments :: Maybe String -> Maybe String -> Maybe String -> [String]
missingArguments url model systemPrompt =
  concat
    [ [ "--llama-url" | isNothing url ]
    , [ "--model" | isNothing model ]
    , [ "--system-prompt" | isNothing systemPrompt ]
    ]

llmUsage :: String
llmUsage = "usage: hutian-shell --llama-url URL --model MODEL --system-prompt PROMPT"

runRequest :: InterruptFlag -> LlmSettings -> Aeson.Value -> IO LlmResult
runRequest interruptFlag settings body = do
  result <-
    try
      ( withInterruptFlag interruptFlag $
          streamCompletion interruptFlag (settingUrl settings) body
      )
      :: IO (Either SomeException LlmResult)
  case result of
    Left exception -> do
      hPutStrLn stderr ("llm: " ++ show exception)
      pure emptyLlmResult
    Right response -> pure response

data Api
  = ChatApi
  | CompletionApi
  deriving stock (Eq, Show)

apiFor :: String -> Api
apiFor url
  | "/v1/chat/completions" `isInfixOf` url = ChatApi
  | otherwise = CompletionApi

requestBodyFor :: Api -> String -> String -> Conversation -> String -> Aeson.Value
requestBodyFor api model systemPrompt conversation prompt =
  case api of
    ChatApi -> chatBody model (requestMessages systemPrompt conversation prompt)
    CompletionApi ->
      object
        [ "model" .= model
        , "prompt" .= completionPrompt systemPrompt conversation prompt
        , "stream" .= True
        ]

-- | Build a follow-up request after tool results, without a new user message.
continuationBodyFor :: Api -> String -> String -> Conversation -> Aeson.Value
continuationBodyFor api model systemPrompt conversation =
  case api of
    ChatApi -> chatBody model (continuationMessages systemPrompt conversation)
    CompletionApi ->
      object
        [ "model" .= model
        , "prompt" .= completionContinuationPrompt systemPrompt conversation
        , "stream" .= True
        ]

chatBody :: String -> [ChatMessage] -> Aeson.Value
chatBody model messages =
  object
    [ "model" .= model
    , "messages" .= map messageValue messages
    , "stream" .= True
    , "tools" .= toolDefinitions
    , "tool_choice" .= ("auto" :: String)
    ]

messageValue :: ChatMessage -> Aeson.Value
messageValue message =
  object $
    [ "role" .= roleName (chatRole message)
    , "content" .= chatContent message
    ]
      ++ [ "tool_calls" .= map toolCallValue calls
         | calls <- [chatToolCalls message]
         , not (null calls)
         ]
      ++ [ "tool_call_id" .= callId
         | callId <- maybeToList (chatToolCallId message)
         ]

toolCallValue :: ToolCall -> Aeson.Value
toolCallValue call =
  object
    [ "id" .= toolCallId call
    , "type" .= ("function" :: String)
    , "function" .=
        object
          [ "name" .= toolCallName call
          , "arguments" .= toolCallArguments call
          ]
    ]

-- | The single tool exposed to the model.
toolDefinitions :: [Aeson.Value]
toolDefinitions =
  [ object
      [ "type" .= ("function" :: String)
      , "function" .=
          object
            [ "name" .= (shellToolName :: String)
            , "description"
                .= ( "Run a shell command with /bin/sh -c in the sandbox. "
                       ++ "Use it to inspect files, run builds and tests, and modify "
                       ++ "the workspace. Each command starts in the sandbox working "
                       ++ "directory in a fresh shell, so cd and exported variables do "
                       ++ "not persist between calls; use absolute paths or combine "
                       ++ "steps such as cd dir && cmd in one command."
                   )
            , "parameters" .=
                object
                  [ "type" .= ("object" :: String)
                  , "properties" .=
                      object
                        [ "command" .=
                            object
                              [ "type" .= ("string" :: String)
                              , "description" .= ("The shell command to run." :: String)
                              ]
                        ]
                  , "required" .= ["command" :: String]
                  ]
            ]
      ]
  ]

shellToolName :: String
shellToolName = "run_shell_command"

-- | Extract the shell command from a tool call. Unknown tools and malformed
-- arguments become a result string that is sent back to the model.
toolCommand :: ToolCall -> Either String String
toolCommand call
  | toolCallName call /= shellToolName =
      Left ("unknown tool: " ++ toolCallName call)
  | otherwise =
      case Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 (Text.pack (toolCallArguments call))) of
        Right (Object arguments) ->
          case KeyMap.lookup "command" arguments of
            Just (String command) -> Right (Text.unpack command)
            _ -> Left "missing string argument: command"
        _ -> Left "tool arguments are not a JSON object"

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just value) = [value]

streamCompletion :: InterruptFlag -> String -> Aeson.Value -> IO LlmResult
streamCompletion interruptFlag url body = do
  manager <- newManager defaultManagerSettings
  initialRequest <- parseRequest url
  let request =
        initialRequest
          { method = "POST"
          , requestHeaders = [("Content-Type", "application/json")]
          , requestBody = RequestBodyLBS (Aeson.encode body)
          , responseTimeout = responseTimeoutNone
          }
  withResponse request manager $ \response -> do
    let code = statusCode (responseStatus response)
    if code < 200 || code >= 300
      then do
        responseBody <- readBody (Network.HTTP.Client.responseBody response) []
        let message = Text.unpack (Text.strip (decode responseBody))
        hPutStrLn stderr ("llm: llama.cpp returned HTTP " ++ show code ++ errorSuffix message)
        pure emptyLlmResult
      else do
        finalState <-
          consumeResponse interruptFlag (Network.HTTP.Client.responseBody response) BS.empty initialState
        endState <- flushReasoning finalState
        let trailing = finishMarkdown (streamMarkdown endState)
        putStr trailing
        unless (null trailing) (putStrLn "")
        unless (streamSawData endState) $
          hPutStrLn stderr "llm: no response content received"
        pure (resultFromState endState)

resultFromState :: StreamState -> LlmResult
resultFromState state =
  LlmResult
    { llmResponseText = responseText
    , llmToolCalls = streamToolCalls state
    }
  where
    calls = streamToolCalls state
    answer = streamAnswerText state
    responseText
      | not (streamSawData state) = Nothing
      | Text.null answer && not (null calls) = Nothing
      | otherwise = Just (Text.unpack answer)

data StreamState = StreamState
  { streamMarkdown :: MarkdownState
  , streamReasoning :: MarkdownState
  , streamReasoningStarted :: Bool
  , streamAnswerStarted :: Bool
  , streamAnswerText :: Text.Text
  , streamToolCalls :: [ToolCall]
  , streamSawData :: Bool
  , streamDone :: Bool
  }

initialState :: StreamState
initialState = StreamState
  { streamMarkdown = initialMarkdownState
  , streamReasoning = initialMarkdownState
  , streamReasoningStarted = False
  , streamAnswerStarted = False
  , streamAnswerText = ""
  , streamToolCalls = []
  , streamSawData = False
  , streamDone = False
  }

thinkingGrey :: String
thinkingGrey = "\ESC[38;5;245m"

flushReasoning :: StreamState -> IO StreamState
flushReasoning state
  | not (streamReasoningStarted state) = pure state
  | otherwise = do
      let trailing = finishMarkdownWith thinkingGrey (streamReasoning state)
      unless (null trailing) $ do
        putStr trailing
        putStrLn ""
      putStr "\ESC[0m"
      hFlush stdout
      pure state { streamReasoning = initialMarkdownState }

emit :: String -> IO ()
emit output =
  unless (null output) $ do
    putStr output
    hFlush stdout

readBody :: BodyReader -> [BS.ByteString] -> IO BS.ByteString
readBody reader chunks = do
  chunk <- brRead reader
  if BS.null chunk
    then pure (BS.concat (reverse chunks))
    else readBody reader (chunk : chunks)

consumeResponse :: InterruptFlag -> BodyReader -> BS.ByteString -> StreamState -> IO StreamState
consumeResponse interruptFlag reader buffered state = do
  interrupted <- isInterrupted interruptFlag
  if interrupted
    then pure state
    else do
      chunk <- brRead reader
      if BS.null chunk
        then processLines (fst (splitLines buffered)) state
        else do
          let (lineChunks, rest) = splitLines (buffered <> chunk)
          state' <- processLines lineChunks state
          if streamDone state'
            then pure state'
            else consumeResponse interruptFlag reader rest state'

splitLines :: BS.ByteString -> ([BS.ByteString], BS.ByteString)
splitLines = go []
  where
    go acc input =
      case BS.elemIndex 10 input of
        Nothing -> (reverse acc, input)
        Just index ->
          let (line, rest) = BS.splitAt index input
           in go (line : acc) (BS.drop 1 rest)

processLines :: [BS.ByteString] -> StreamState -> IO StreamState
processLines [] state = pure state
processLines (line : lines') state
  | streamDone state = pure state
  | otherwise = do
      state' <- processLine line state
      processLines lines' state'

-- | One streaming fragment of a tool call. The first fragment usually carries
-- the id and function name; later fragments append to the arguments.
data ToolCallDelta = ToolCallDelta
  { toolCallDeltaIndex :: Int
  , toolCallDeltaId :: Maybe Text.Text
  , toolCallDeltaName :: Maybe Text.Text
  , toolCallDeltaArguments :: Maybe Text.Text
  }
  deriving stock (Eq, Show)

-- | A parsed server-sent event from a streaming response.
data StreamEvent = StreamEvent
  { eventReasoning :: Maybe Text.Text
  , eventContent :: Maybe Text.Text
  , eventError :: Maybe Text.Text
  , eventToolCalls :: [ToolCallDelta]
  , eventStop :: Bool
  }
  deriving stock (Eq, Show)

processLine :: BS.ByteString -> StreamState -> IO StreamState
processLine rawLine state =
  case parseStreamLine rawLine of
    Nothing -> pure state
    Just event -> do
      reasoningState <- renderReasoning (eventReasoning event) state
      contentState <- renderContent (eventContent event) reasoningState
      case eventError event of
        Just message -> hPutStrLn stderr ("llm: " ++ Text.unpack message)
        Nothing -> pure ()
      let toolState =
            contentState
              { streamToolCalls =
                  mergeToolCallDeltas (streamToolCalls contentState) (eventToolCalls event)
              }
      pure
        toolState
          { streamSawData = True
          , streamDone = streamDone toolState || eventStop event
          }

-- | Parse one SSE line, or a bare JSON line, into a stream event.
parseStreamLine :: BS.ByteString -> Maybe StreamEvent
parseStreamLine rawLine
  | BS.null line = Nothing
  | "data:" `BS.isPrefixOf` line = parsePayload (Text.strip (decode (BS.drop 5 line)))
  | otherwise = parsePayload (Text.strip (decode line))
  where
    line = BS.dropWhileEnd isCarriageReturn rawLine

parsePayload :: Text.Text -> Maybe StreamEvent
parsePayload payload
  | payload == "[DONE]" = Just (StreamEvent Nothing Nothing Nothing [] True)
  | otherwise =
      case Aeson.decodeStrict' (TextEncoding.encodeUtf8 payload) of
        Just (Object value) ->
          Just
            StreamEvent
              { eventReasoning = extractReasoning value
              , eventContent = extractContent value
              , eventError = extractError value
              , eventToolCalls = extractToolCalls value
              , eventStop = isStop value
              }
        _ -> Nothing

isCarriageReturn :: Word8 -> Bool
isCarriageReturn = (== 13)

renderReasoning :: Maybe Text.Text -> StreamState -> IO StreamState
renderReasoning Nothing state = pure state
renderReasoning (Just text) state
  | Text.null text = pure state
  | otherwise = do
      let (markdown', output) =
            renderMarkdownWith thinkingGrey (streamReasoning state) text
      emit output
      pure
        state
          { streamReasoning = markdown'
          , streamReasoningStarted = True
          }

renderContent :: Maybe Text.Text -> StreamState -> IO StreamState
renderContent Nothing state = pure state
renderContent (Just text) state
  | Text.null text = pure state
  | otherwise = do
      startingAnswer <- transitionFromReasoning state
      let (markdown', output) =
            renderMarkdown (streamMarkdown startingAnswer) text
      emit output
      pure
        startingAnswer
          { streamMarkdown = markdown'
          , streamAnswerStarted = True
          , streamAnswerText = streamAnswerText startingAnswer <> text
          }

transitionFromReasoning :: StreamState -> IO StreamState
transitionFromReasoning state
  | streamAnswerStarted state = pure state
  | otherwise = flushReasoning state

extractContent :: KeyMap.KeyMap Value -> Maybe Text.Text
extractContent value =
  lookupString "content" value
    <|> listToMaybe (mapMaybe (lookupString "content") (deltas value))

extractReasoning :: KeyMap.KeyMap Value -> Maybe Text.Text
extractReasoning value =
  firstString reasoningKeys value
    <|> listToMaybe (mapMaybe (firstString reasoningKeys) (deltas value))
  where
    reasoningKeys = ["reasoning_content", "reasoning", "thinking"]

extractError :: KeyMap.KeyMap Value -> Maybe Text.Text
extractError value =
  case KeyMap.lookup "error" value of
    Just (String message) -> Just message
    Just (Object nested) ->
      lookupString "message" nested
        <|> Just (Text.pack (show (Object nested)))
    _ -> Nothing

-- | Extract tool-call fragments from either a bare delta object or the deltas
-- nested under @choices@.
extractToolCalls :: KeyMap.KeyMap Value -> [ToolCallDelta]
extractToolCalls value =
  indexedToolCalls 0 (toolCallObjects value)
    ++ concatMap (indexedToolCalls 0 . toolCallObjects) (deltas value)

indexedToolCalls :: Int -> [KeyMap.KeyMap Value] -> [ToolCallDelta]
indexedToolCalls start objects =
  [ ToolCallDelta
      { toolCallDeltaIndex = fromMaybe (start + offset) (lookupIndex call)
      , toolCallDeltaId = lookupString "id" call
      , toolCallDeltaName = lookupFunctionString "name" call
      , toolCallDeltaArguments = lookupFunctionString "arguments" call
      }
  | (offset, call) <- zip [0 ..] objects
  ]

toolCallObjects :: KeyMap.KeyMap Value -> [KeyMap.KeyMap Value]
toolCallObjects fields =
  case KeyMap.lookup "tool_calls" fields of
    Just (Array items) -> [item | Object item <- Vector.toList items]
    _ -> []

lookupIndex :: KeyMap.KeyMap Value -> Maybe Int
lookupIndex fields =
  case KeyMap.lookup "index" fields of
    Just (Number number) -> Just (round number)
    _ -> Nothing

lookupFunctionString :: Key -> KeyMap.KeyMap Value -> Maybe Text.Text
lookupFunctionString key fields =
  case KeyMap.lookup "function" fields of
    Just (Object function) -> lookupString key function
    _ -> Nothing

-- | Merge streamed tool-call fragments by their index.
mergeToolCallDeltas :: [ToolCall] -> [ToolCallDelta] -> [ToolCall]
mergeToolCallDeltas = foldl' mergeOne
  where
    mergeOne calls delta =
      let index = toolCallDeltaIndex delta
          (before, rest) = splitAt index calls
          existing =
            case rest of
              call : _ -> call
              [] -> ToolCall "" "" ""
          updated =
            existing
              { toolCallId = preferText (toolCallId existing) (toolCallDeltaId delta)
              , toolCallName = preferText (toolCallName existing) (toolCallDeltaName delta)
              , toolCallArguments =
                  toolCallArguments existing
                    ++ maybe "" Text.unpack (toolCallDeltaArguments delta)
              }
       in before ++ [updated] ++ drop 1 rest

preferText :: String -> Maybe Text.Text -> String
preferText current Nothing = current
preferText current (Just text)
  | Text.null text = current
  | otherwise = Text.unpack text

isStop :: KeyMap.KeyMap Value -> Bool
isStop value =
  case KeyMap.lookup "stop" value of
    Just (Bool True) -> True
    _ -> any choiceFinished (choices value)
  where
    choiceFinished choice =
      case KeyMap.lookup "finish_reason" choice of
        Nothing -> False
        Just Null -> False
        Just _ -> True

-- | The choice objects of a streaming response.
choices :: KeyMap.KeyMap Value -> [KeyMap.KeyMap Value]
choices value =
  case KeyMap.lookup "choices" value of
    Just (Array items) -> [choice | Object choice <- Vector.toList items]
    _ -> []

-- | The non-empty delta objects of a streaming response.
deltas :: KeyMap.KeyMap Value -> [KeyMap.KeyMap Value]
deltas = mapMaybe delta . choices
  where
    delta choice =
      case KeyMap.lookup "delta" choice of
        Just (Object fields) -> Just fields
        _ -> Nothing

lookupString :: Key -> KeyMap.KeyMap Value -> Maybe Text.Text
lookupString key fields =
  case KeyMap.lookup key fields of
    Just (String text) -> Just text
    _ -> Nothing

firstString :: [Key] -> KeyMap.KeyMap Value -> Maybe Text.Text
firstString keys fields =
  listToMaybe
    [ text
    | key <- keys
    , Just (String text) <- [KeyMap.lookup key fields]
    ]

decode :: BS.ByteString -> Text.Text
decode = TextEncoding.decodeUtf8With TextEncodingError.lenientDecode

errorSuffix :: String -> String
errorSuffix "" = ""
errorSuffix message = ": " ++ message
