{-# LANGUAGE OverloadedStrings #-}

module LlmIntegrationSpec (tests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Hutian.Conversation (ConversationEntry (..), ToolCall (..))
import Hutian.Interrupt (newInterruptFlag)
import Hutian.Llm
  ( LlmResult (..)
  , LlmSettings (..)
  , LlmUrl (..)
  , ModelName (..)
  , SystemPrompt (..)
  , runLlm
  , runLlmWithTools
  )
import Hutian.Policy (ToolPolicy (..))
import Hutian.Tool.Catalog (toolCatalog)
import Hutian.Verbosity (Verbosity (..))
import Hutian.Tool.Config (ToolConfig (..), defaultToolConfig)
import Hutian.Render (dimTealForeground, grayForeground)
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
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit
import TestSupport (captureStderr, captureStdout, stdoutLlmOutput)

tests :: TestTree
tests =
  testGroup
    "llm-integration"
    [ testCase "runLlm streams reasoning, highlighted answer, and returns raw text" $ do
        (port, requestVar) <- startFakeServer streamingChunks
        flag <- newInterruptFlag
        captured <-
          timeout 7000000 $
            captureStdout $
              runLlm
                stdoutLlmOutput
                flag
                (settingsFor port)
                [UserEntry "prior", AssistantEntry "previous"]
                "current"
        (result, output) <-
          maybe (assertFailure "runLlm timed out" >> pure (Nothing, "")) pure captured
        result @?= Just "# Answer\n**bold**\n"
        assertBool "thinking should be labelled" ((dimTealForeground ++ "think:") `isInfixOf` output)
        assertBool "answer should be labelled" ("\ESC[1;38;2;0;255;200manswer:" `isInfixOf` output)
        assertBool "thinking should be grey" (grayForeground `isInfixOf` output)
        assertBool "heading should be highlighted" ("\ESC[1;38;2;137;180;250m" `isInfixOf` output)
        assertBool "bold should be highlighted" ("\ESC[1;38;2;249;226;175m" `isInfixOf` output)
        assertBool "reasoning should not be part of the answer" (not ("thinking" `isInfixOf` fromMaybe "" result))
        request <- fromMaybe "" <$> timeout 2000000 (takeMVar requestVar)
        assertBool "fake server did not receive a request" (not (null request))
        assertBool "prior user history missing from HTTP request" ("prior" `isInfixOf` request)
        assertBool "prior assistant history missing from HTTP request" ("previous" `isInfixOf` request)
        assertBool "current prompt missing from HTTP request" ("current" `isInfixOf` request)
    , testCase "runLlmWithTools returns streamed tool calls" $ do
        (port, _) <- startFakeServer toolCallChunks
        flag <- newInterruptFlag
        captured <-
          timeout 7000000 $
            captureStdout $
              runLlmWithTools stdoutLlmOutput flag (settingsFor port) [UserEntry "prior"] "run ls"
        (result, _) <-
          maybe (assertFailure "runLlmWithTools timed out" >> pure (emptyResult, "")) pure captured
        llmToolCalls result
          @?= [ToolCall "call-1" "shell" "{\"command\":\"ls\"}"]
        llmResponseText result @?= Nothing
    , testCase "runLlm processes a final stream line without a newline" $ do
        let chunk =
              Aeson.object
                [ "choices"
                    .= [ Aeson.object
                           [ "delta" .= Aeson.object ["content" .= ("last" :: String)]
                           ]
                       ]
                ]
        (port, _) <-
          startFakeServerWith
            (streamingResponse ("data: " <> BL.toStrict (Aeson.encode chunk)))
        flag <- newInterruptFlag
        captured <-
          timeout 7000000 $
            captureStdout $
              runLlm stdoutLlmOutput flag (settingsFor port) [] "hello"
        (result, _) <-
          maybe
            (assertFailure "unterminated stream test timed out" >> pure (Nothing, ""))
            pure
            captured
        result @?= Just "last"
    , testCase "non-success responses are reported without throwing" $ do
        (port, _) <- startFakeServerWith (httpResponse 500 "{\"error\":\"boom\"}")
        flag <- newInterruptFlag
        captured <-
          timeout 7000000 $
            captureStderr $
              runLlm stdoutLlmOutput flag (settingsFor port) [] "hello"
        (result, output) <-
          maybe (assertFailure "error response test timed out" >> pure (Nothing, "")) pure captured
        result @?= Nothing
        assertBool "status code should be reported" ("HTTP 500" `isInfixOf` output)
        assertBool "error body should be reported" ("boom" `isInfixOf` output)
    ]

settingsFor :: Int -> LlmSettings
settingsFor port =
  LlmSettings
    { settingUrl = LlmUrl ("http://127.0.0.1:" ++ show port ++ "/v1/chat/completions")
    , settingModel = ModelName "test-model"
    , settingSystemPrompt = SystemPrompt "system prompt"
    , settingSocket = Nothing
    , settingVerbosity = VerbosityHigh
    , settingTools = toolCatalog (defaultToolConfig {toolShell = ToolAllow})
    }

emptyResult :: LlmResult
emptyResult = LlmResult Nothing []

streamingChunks :: [Aeson.Value]
streamingChunks =
  [ Aeson.object
      [ "choices"
          .= [ Aeson.object
                 [ "delta" .= Aeson.object ["reasoning_content" .= ("thinking\n" :: String)]
                 ]
             ]
      ]
  , Aeson.object
      [ "choices"
          .= [ Aeson.object
                 [ "delta" .= Aeson.object ["content" .= ("# Answer\n" :: String)]
                 ]
             ]
      ]
  , Aeson.object
      [ "choices"
          .= [ Aeson.object
                 [ "delta" .= Aeson.object ["content" .= ("**bold**\n" :: String)]
                 ]
             ]
      ]
  ]

toolCallChunks :: [Aeson.Value]
toolCallChunks =
  [ Aeson.object
      [ "choices"
          .= [ Aeson.object
                 [ "delta"
                     .= Aeson.object
                       [ "tool_calls"
                           .= [ Aeson.object
                                  [ "index" .= (0 :: Int)
                                  , "id" .= ("call-1" :: String)
                                  , "type" .= ("function" :: String)
                                  , "function"
                                      .= Aeson.object
                                        [ "name" .= ("shell" :: String)
                                        , "arguments" .= ("{\"command\":" :: String)
                                        ]
                                  ]
                              ]
                       ]
                 ]
             ]
      ]
  , Aeson.object
      [ "choices"
          .= [ Aeson.object
                 [ "delta"
                     .= Aeson.object
                       [ "tool_calls"
                           .= [ Aeson.object
                                  [ "index" .= (0 :: Int)
                                  , "function"
                                      .= Aeson.object
                                        [ "arguments" .= ("\"ls\"}" :: String)
                                        ]
                                  ]
                              ]
                       ]
                 ]
             ]
      ]
  , Aeson.object
      [ "choices"
          .= [ Aeson.object
                 [ "finish_reason" .= ("tool_calls" :: String)
                 ]
             ]
      ]
  ]

startFakeServer :: [Aeson.Value] -> IO (Int, MVar String)
startFakeServer = startFakeServerWith . responseFor

startFakeServerWith :: BS.ByteString -> IO (Int, MVar String)
startFakeServerWith response = do
  listener <- socket AF_INET Stream defaultProtocol
  bind listener (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  listen listener 1
  port <- fromIntegral <$> socketPort listener
  requestVar <- newEmptyMVar
  _ <- forkIO $ do
    (connection, _) <- accept listener
    request <- receiveRequest connection
    putMVar requestVar (BS8.unpack request)
    sendAll connection response
    close connection
    close listener
  pure (port, requestVar)

receiveRequest :: Socket -> IO BS.ByteString
receiveRequest connection = go BS.empty
  where
    go accumulated
      | "\r\n\r\n" `BS.isInfixOf` accumulated = pure accumulated
      | otherwise = do
          chunk <- recv connection 4096
          if BS.null chunk
            then pure accumulated
            else go (accumulated <> chunk)

responseFor :: [Aeson.Value] -> BS.ByteString
responseFor chunks =
  streamingResponse $
    BS.concat
      [ "data: " <> BL.toStrict (Aeson.encode chunk) <> "\n\n"
      | chunk <- chunks
      ]
      <> "data: [DONE]\n\n"

streamingResponse :: BS.ByteString -> BS.ByteString
streamingResponse body =
  BS8.pack
    ( "HTTP/1.1 200 OK\r\n"
        ++ "Content-Type: text/event-stream\r\n"
        ++ "Content-Length: "
        ++ show (BS.length body)
        ++ "\r\n"
        ++ "Connection: close\r\n\r\n"
    )
    <> body

httpResponse :: Int -> BS.ByteString -> BS.ByteString
httpResponse status body =
  BS8.pack
    ( "HTTP/1.1 "
        ++ show status
        ++ " Error\r\n"
        ++ "Content-Type: application/json\r\n"
        ++ "Content-Length: "
        ++ show (BS.length body)
        ++ "\r\n"
        ++ "Connection: close\r\n\r\n"
    )
    <> body
