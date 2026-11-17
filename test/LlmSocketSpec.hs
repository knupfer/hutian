{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LlmSocketSpec (tests) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, catch, finally)
import qualified Data.ByteString as BS
import Data.List (isInfixOf)
import Hutian.Conversation (ConversationEntry (..))
import Hutian.Http.Wire (recvAll)
import Hutian.Interrupt
  ( clearInterrupt
  , installInterruptHandler
  , newInterruptFlag
  )
import Hutian.Llm
  ( LlmResult (..)
  , LlmSettings (..)
  , LlmUrl (..)
  , ModelName (..)
  , SystemPrompt (..)
  , runLlm
  , runLlmWithTools
  )
import Hutian.Llm.UnixSocket (postUnixStream)
import Hutian.Policy (ModelShell (..))
import Hutian.Relay.Protocol (decodeStatus, encodeStatus)
import Network.Socket
  ( Family (AF_UNIX)
  , SockAddr (SockAddrUnix)
  , SocketType (Stream)
  , accept
  , bind
  , close
  , defaultProtocol
  , listen
  , socket
  )
import Network.Socket.ByteString (sendAll)
import System.Posix.Signals (raiseSignal, sigINT)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit
import TestSupport
  ( assertInfix
  , assertNotInfix
  , awaitMVar
  , captureStderr
  , captureStdout
  , stdoutLlmOutput
  , withTemporaryDirectory
  )

tests :: TestTree
tests =
  testGroup
    "llm-socket"
    [ unitTests
    , socketTests
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "unit"
    [ testCase "decodeStatus accepts only four-byte HTTP status frames" $ do
        decodeStatus (encodeStatus 200) @?= Right 200
        decodeStatus (encodeStatus 599) @?= Right 599
        decodeStatus (encodeStatus 99) @?= Left "relay: invalid status code"
        decodeStatus (encodeStatus 600) @?= Left "relay: invalid status code"
        decodeStatus "\0\0\0" @?= Left "relay: invalid status frame"
        decodeStatus (BS.replicate 5 0) @?= Left "relay: invalid status frame"
    , testCase "encodeStatus writes big-endian bytes" $
        encodeStatus 200 @?= BS.pack [0, 0, 0, 200]
    ]

socketTests :: TestTree
socketTests =
  testGroup
    "socket"
    [ testCase "postUnixStream sends only the body and reads a streamed response" $
        withTemporaryDirectory "hutian-unix-client" $ \directory -> do
          let socketPath = directory ++ "/llm.sock"
          requests <- startUnixServer socketPath streamingResponse
          result <-
            timeout 3000000 $
              postUnixStream socketPath "{\"model\":\"m\"}" $ \status reader -> do
                body <- readAll reader
                pure (status, body)
          (status, body) <- maybe (assertFailure "socket request timed out" >> pure (0, "")) pure result
          status @?= 200
          assertInfix "answer" body
          request <- awaitMVar "Unix server did not receive a request" requests
          request @?= "{\"model\":\"m\"}"
    , testCase "postUnixStream surfaces non-success status codes" $
        withTemporaryDirectory "hutian-unix-client" $ \directory -> do
          let socketPath = directory ++ "/llm.sock"
          _ <- startUnixServer socketPath errorResponse
          result <-
            timeout 3000000 $
              postUnixStream socketPath "{}" $ \status reader -> do
                body <- readAll reader
                pure (status, body)
          (status, body) <- maybe (assertFailure "socket request timed out" >> pure (0, "")) pure result
          status @?= 500
          assertInfix "upstream exploded" body
    , testCase "runLlm streams through a Unix socket" $
        withTemporaryDirectory "hutian-unix-client" $ \directory -> do
          let socketPath = directory ++ "/llm.sock"
              settings = settingsForSocket socketPath
          requests <- startUnixServer socketPath streamingResponse
          flag <- newInterruptFlag
          captured <-
            timeout 7000000 $
              captureStdout $
                runLlm stdoutLlmOutput flag settings [UserEntry "prior"] "current"
          (result, output) <-
            maybe (assertFailure "runLlm timed out" >> pure (Nothing, "")) pure captured
          result @?= Just "answer\n"
          assertBool ("answer label missing: " ++ output) ("answer:" `isInfixOf` output)
          request <- awaitMVar "Unix server did not receive a request" requests
          assertInfix "current" request
          assertInfix "prior" request
          assertNotInfix "Authorization" request
    , testCase "runLlmWithTools stops a blocked stream when interrupted" $
        withTemporaryDirectory "hutian-unix-client" $ \directory -> do
          let socketPath = directory ++ "/llm.sock"
          ready <- startStallingUnixServer socketPath
          flag <- newInterruptFlag
          installInterruptHandler flag
          _ <- forkIO $ do
            takeMVar ready
            threadDelay 100000
            raiseSignal sigINT
          result <-
            timeout 3000000 $
              captureStderr $
                runLlmWithTools
                  stdoutLlmOutput
                  flag
                  (settingsForSocket socketPath)
                  [UserEntry "prior"]
                  "current"
          clearInterrupt flag
          case result of
            Nothing -> assertFailure "interrupt did not stop a blocked stream"
            Just (response, errors) -> do
              llmToolCalls response @?= []
              llmResponseText response @?= Nothing
              assertBool
                "cancelling the worker must not be reported as a request error"
                (not ("thread killed" `isInfixOf` errors))
    , testCase "runLlm reports errors without crashing when the socket is missing" $ do
        flag <- newInterruptFlag
        captured <-
          timeout 3000000 $
            captureStderr $
              runLlm stdoutLlmOutput flag (settingsForSocket "/nonexistent/hutian/llm.sock") [] "hello"
        (result, errors) <-
          maybe (assertFailure "missing socket test timed out" >> pure (Nothing, "")) pure captured
        result @?= Nothing
        assertBool "the connection failure should be reported" ("llm:" `isInfixOf` errors)
    ]

settingsForSocket :: FilePath -> LlmSettings
settingsForSocket socketPath =
  LlmSettings
    { settingUrl = LlmUrl "http://hutian-relay/v1/chat/completions"
    , settingModel = ModelName "test-model"
    , settingSystemPrompt = SystemPrompt "system prompt"
    , settingSocket = Just socketPath
    , settingModelShell = ModelShellAllowed
    }

streamingResponse :: BS.ByteString
streamingResponse =
  encodeStatus 200
    <> BS.concat
      [ "data: {\"choices\":[{\"delta\":{\"content\":\"answer\\n\"}}]}\n\n"
      , "data: [DONE]\n\n"
      ]

errorResponse :: BS.ByteString
errorResponse =
  encodeStatus 500
    <> "{\"error\":\"upstream exploded\"}"

startUnixServer :: FilePath -> BS.ByteString -> IO (MVar BS.ByteString)
startUnixServer path response = do
  listener <- socket AF_UNIX Stream defaultProtocol
  bind listener (SockAddrUnix path)
  listen listener 1
  requests <- newEmptyMVar
  _ <- forkIO $
    ( do
        (connection, _) <- accept listener
        request <- recvAll connection
        putMVar requests request
        sendAll connection response
        close connection
        close listener
    )
      `catch` \(_ :: SomeException) -> pure ()
  pure requests

-- | Accept one request, send a successful status, then leave the body open and
-- silent. The returned 'MVar' is filled once the status is on the wire, so a
-- test can raise SIGINT only after the client is known to be waiting for data.
startStallingUnixServer :: FilePath -> IO (MVar ())
startStallingUnixServer path = do
  listener <- socket AF_UNIX Stream defaultProtocol
  bind listener (SockAddrUnix path)
  listen listener 1
  ready <- newEmptyMVar
  _ <- forkIO $
    ( ( do
          (connection, _) <- accept listener
          _ <- recvAll connection
          sendAll connection (encodeStatus 200)
          putMVar ready ()
          threadDelay 5000000
          close connection
      )
        `finally` close listener
    )
      `catch` \(_ :: SomeException) -> pure ()
  pure ready

readAll :: IO BS.ByteString -> IO BS.ByteString
readAll reader = go []
  where
    go chunks = do
      chunk <- reader
      if BS.null chunk
        then pure (BS.concat (reverse chunks))
        else go (chunk : chunks)
