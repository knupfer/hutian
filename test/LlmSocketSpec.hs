{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LlmSocketSpec (tests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar)
import Control.Exception (SomeException, catch, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Either (fromRight)
import Data.List (isInfixOf)
import Hutian.Conversation (ConversationEntry (..))
import Hutian.Http.Wire (recvExactly, recvUntil)
import Hutian.Interrupt (newInterruptFlag)
import Hutian.Llm
  ( LlmSettings (..)
  , LlmUrl (..)
  , ModelName (..)
  , SystemPrompt (..)
  , runLlm
  )
import Hutian.Llm.UnixSocket
  ( postUnixStream
  , readStatusFromHead
  , requestBytes
  )
import Hutian.Relay.Http (headerValue, parseRequestHead)
import Network.Socket
  ( Family (AF_UNIX)
  , SockAddr (SockAddrUnix)
  , Socket
  , SocketType (Stream)
  , accept
  , bind
  , close
  , defaultProtocol
  , listen
  , socket
  )
import Network.Socket.ByteString (sendAll)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Property, counterexample, property, (.&&.))
import TestSupport
  ( assertInfix
  , assertNotInfix
  , awaitMVar
  , captureStdout
  , withTemporaryDirectory
  )
import Text.Read (readMaybe)

tests :: TestTree
tests =
  testGroup
    "llm-socket"
    [ unitTests
    , properties
    , socketTests
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "unit"
    [ testCase "requestBytes builds a relay request without credentials" $ do
        let body = "{\"model\":\"m\"}"
            request = requestBytes "/v1/chat/completions" body
        assertBool "missing request line" ("POST /v1/chat/completions HTTP/1.1\r\n" `BS.isPrefixOf` request)
        assertInfix "Content-Length: " request
        assertInfix (BS8.pack (show (BS.length body))) request
        assertInfix body request
        assertNotInfix "Authorization" request
    , testCase "readStatusFromHead parses and validates status lines" $ do
        readStatusFromHead "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n"
          @?= Right 200
        readStatusFromHead "HTTP/1.0 404 Not Found\r\n"
          @?= Right 404
        readStatusFromHead "not a status\r\n"
          @?= Left "llm: invalid relay response status"
        readStatusFromHead "HTTP/1.1 nope\r\n"
          @?= Left "llm: invalid relay response status"
        readStatusFromHead "BOGUS 200 OK\r\n"
          @?= Left "llm: invalid relay response status"
        readStatusFromHead "HTTP/1.1 99 Too Early\r\n"
          @?= Left "llm: invalid relay response status"
    ]

properties :: TestTree
properties =
  testGroup
    "properties"
    [ testProperty "requestBytes preserves the body and length" prop_requestBytes
    ]

prop_requestBytes :: String -> Property
prop_requestBytes input =
  let body = BS8.pack input
      request = requestBytes "/v1/chat/completions" body
      (headPart, _) = BS.breakSubstring "\r\n\r\n" request
      lengthHeader = "Content-Length: " <> BS8.pack (show (BS.length body))
   in counterexample (show request) $
        property (lengthHeader `BS.isInfixOf` headPart)
          .&&. property (body `BS.isInfixOf` request)
          .&&. property (not ("Authorization" `BS.isInfixOf` headPart))

socketTests :: TestTree
socketTests =
  testGroup
    "socket"
    [ testCase "postUnixStream reads a streamed response over a Unix socket" $
        withTemporaryDirectory "hutian-unix-client" $ \directory -> do
          let socketPath = directory ++ "/llm.sock"
          requests <- startUnixServer socketPath streamingResponse
          result <-
            timeout 3000000 $
              postUnixStream socketPath "/v1/chat/completions" "{}" $ \status reader -> do
                body <- readAll reader
                pure (status, body)
          (status, body) <- maybe (assertFailure "socket request timed out" >> pure (0, "")) pure result
          status @?= 200
          assertInfix "answer" body
          request <- awaitMVar "Unix server did not receive a request" requests
          assertInfix "POST /v1/chat/completions HTTP/1.1" request
          assertInfix "{}" request
          assertNotInfix "Authorization" request
    , testCase "postUnixStream surfaces non-success status codes" $
        withTemporaryDirectory "hutian-unix-client" $ \directory -> do
          let socketPath = directory ++ "/llm.sock"
          _ <- startUnixServer socketPath errorResponse
          result <-
            timeout 3000000 $
              postUnixStream socketPath "/v1/chat/completions" "{}" $ \status reader -> do
                body <- readAll reader
                pure (status, body)
          (status, body) <- maybe (assertFailure "socket request timed out" >> pure (0, "")) pure result
          status @?= 500
          assertInfix "upstream exploded" body
    , testCase "postUnixStream rejects an oversized response head" $
        withTemporaryDirectory "hutian-unix-client" $ \directory -> do
          let socketPath = directory ++ "/llm.sock"
          _ <- startUnixServer socketPath (BS.replicate 70000 97)
          result <-
            try
              ( postUnixStream socketPath "/v1/chat/completions" "{}" (\_ _ -> pure ())
                  :: IO ()
              )
              :: IO (Either SomeException ())
          case result of
            Left _ -> pure ()
            Right () -> assertFailure "an oversized response head should fail"
    , testCase "runLlm streams through a Unix socket" $
        withTemporaryDirectory "hutian-unix-client" $ \directory -> do
          let socketPath = directory ++ "/llm.sock"
              settings = settingsForSocket socketPath
          requests <- startUnixServer socketPath streamingResponse
          flag <- newInterruptFlag
          captured <-
            timeout 7000000 $
              captureStdout $
                runLlm flag settings [UserEntry "prior"] "current"
          (result, output) <-
            maybe (assertFailure "runLlm timed out" >> pure (Nothing, "")) pure captured
          result @?= Just "answer\n"
          assertBool ("answer label missing: " ++ output) ("answer:" `isInfixOf` output)
          request <- awaitMVar "Unix server did not receive a request" requests
          assertInfix "current" request
          assertInfix "prior" request
          assertNotInfix "Authorization" request
    , testCase "runLlm reports errors without crashing when the socket is missing" $ do
        flag <- newInterruptFlag
        captured <-
          timeout 3000000 $
            captureStdout $
              runLlm flag (settingsForSocket "/nonexistent/hutian/llm.sock") [] "hello"
        (result, _) <-
          maybe (assertFailure "missing socket test timed out" >> pure (Nothing, "")) pure captured
        result @?= Nothing
    ]

settingsForSocket :: FilePath -> LlmSettings
settingsForSocket socketPath =
  LlmSettings
    { settingUrl = LlmUrl "http://hutian-relay/v1/chat/completions"
    , settingModel = ModelName "test-model"
    , settingSystemPrompt = SystemPrompt "system prompt"
    , settingSocket = Just socketPath
    }

streamingResponse :: BS.ByteString
streamingResponse =
  BS.concat
    [ "HTTP/1.1 200 OK\r\n"
    , "Content-Type: text/event-stream\r\n"
    , "Connection: close\r\n\r\n"
    , "data: {\"choices\":[{\"delta\":{\"content\":\"answer\\n\"}}]}\n\n"
    , "data: [DONE]\n\n"
    ]

errorResponse :: BS.ByteString
errorResponse =
  BS.concat
    [ "HTTP/1.1 500 Internal Server Error\r\n"
    , "Content-Type: application/json\r\n"
    , "Connection: close\r\n\r\n"
    , "{\"error\":\"upstream exploded\"}"
    ]

startUnixServer :: FilePath -> BS.ByteString -> IO (MVar BS.ByteString)
startUnixServer path response = do
  listener <- socket AF_UNIX Stream defaultProtocol
  bind listener (SockAddrUnix path)
  listen listener 1
  requests <- newEmptyMVar
  _ <- forkIO $
    ( do
        (connection, _) <- accept listener
        request <- receiveRequest connection
        putMVar requests request
        sendAll connection response
        close connection
        close listener
    )
      `catch` \(_ :: SomeException) -> pure ()
  pure requests

receiveRequest :: Socket -> IO BS.ByteString
receiveRequest connection = do
  headResult <- recvUntil connection "\r\n\r\n" maxRequestHead
  case headResult of
    Left _ -> pure BS.empty
    Right (rawHead, leftover) ->
      case parseRequestHead rawHead of
        Left _ -> pure rawHead
        Right request ->
          case headerValue "content-length" request >>= readMaybe . BS8.unpack of
            Nothing -> pure rawHead
            Just bodyLength -> do
              bodyResult <- recvExactly connection bodyLength leftover
              pure (rawHead <> fromRight BS.empty bodyResult)

maxRequestHead :: Int
maxRequestHead = 64 * 1024

readAll :: IO BS.ByteString -> IO BS.ByteString
readAll reader = go []
  where
    go chunks = do
      chunk <- reader
      if BS.null chunk
        then pure (BS.concat (reverse chunks))
        else go (chunk : chunks)
