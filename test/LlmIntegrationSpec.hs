{-# LANGUAGE OverloadedStrings #-}

module LlmIntegrationSpec (tests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Hutian.Conversation (ConversationEntry (..))
import Hutian.Interrupt (newInterruptFlag)
import Hutian.Llm (runLlm)
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
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO (hClose, hFlush, stdout)
import System.Posix.IO
  ( closeFd
  , createPipe
  , dup
  , dupTo
  , fdToHandle
  , stdOutput
  )
import System.Posix.Types (Fd)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "llm-integration"
    [ testCase "runLlm streams reasoning, highlighted answer, and returns raw text" $ do
        (port, requestVar) <- startFakeServer streamingChunks
        withLlmEnvironment port $ do
          flag <- newInterruptFlag
          captured <-
            timeout 7000000 $
              captureStdout $
                runLlm flag [UserEntry "prior", AssistantEntry "previous"] "current"
          (result, output) <-
            maybe (assertFailure "runLlm timed out" >> pure (Nothing, "")) pure captured
          result @?= Just "# Answer\n**bold**\n"
          assertBool "thinking should be grey" ("\ESC[38;5;245m" `isInfixOf` output)
          assertBool "heading should be highlighted" ("\ESC[38;5;81m" `isInfixOf` output)
          assertBool "bold should be highlighted" ("\ESC[1;38;5;222m" `isInfixOf` output)
          assertBool "reasoning should not be part of the answer" (not ("thinking" `isInfixOf` fromMaybe "" result))
        request <- fromMaybe "" <$> timeout 2000000 (takeMVar requestVar)
        assertBool "fake server did not receive a request" (not (null request))
        assertBool "prior user history missing from HTTP request" ("prior" `isInfixOf` request)
        assertBool "prior assistant history missing from HTTP request" ("previous" `isInfixOf` request)
        assertBool "current prompt missing from HTTP request" ("current" `isInfixOf` request)
    ]

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

startFakeServer :: [Aeson.Value] -> IO (Int, MVar String)
startFakeServer chunks = do
  listener <- socket AF_INET Stream defaultProtocol
  bind listener (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  listen listener 1
  port <- fromIntegral <$> socketPort listener
  requestVar <- newEmptyMVar
  _ <- forkIO $ do
    (connection, _) <- accept listener
    request <- receiveRequest connection
    putMVar requestVar (BS8.unpack request)
    sendAll connection (responseFor chunks)
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
  let body =
        BS.concat
          [ "data: " <> BL.toStrict (Aeson.encode chunk) <> "\n\n"
          | chunk <- chunks
          ]
          <> "data: [DONE]\n\n"
      headers =
        BS8.pack
          ( "HTTP/1.1 200 OK\r\n"
              ++ "Content-Type: text/event-stream\r\n"
              ++ "Content-Length: "
              ++ show (BS.length body)
              ++ "\r\n"
              ++ "Connection: close\r\n\r\n"
          )
   in headers <> body

withLlmEnvironment :: Int -> IO a -> IO a
withLlmEnvironment port action = do
  withEnv "HUTIAN_LLAMA_URL" ("http://127.0.0.1:" ++ show port ++ "/v1/chat/completions") $
    withEnv "HUTIAN_LLAMA_MODEL" "test-model" $
      withEnv "HUTIAN_SYSTEM_PROMPT" "system prompt" action

withEnv :: String -> String -> IO a -> IO a
withEnv name value action = do
  original <- lookupEnv name
  setEnv name value
  action `finally` maybe (unsetEnv name) (setEnv name) original

captureStdout :: IO a -> IO (a, String)
captureStdout action = do
  (readFd, writeFd) <- createPipe
  savedStdout <- dup stdOutput
  _ <- dupTo writeFd stdOutput
  closeFd writeFd
  hFlush stdout
  result <-
    action
      `finally` do
        hFlush stdout
        _ <- dupTo savedStdout stdOutput
        closeFd savedStdout
  hFlush stdout
  output <- readPipe readFd
  pure (result, output)

readPipe :: Fd -> IO String
readPipe readFd = do
  handle <- fdToHandle readFd
  contents <- BS.hGetContents handle
  hClose handle
  pure (BS8.unpack contents)