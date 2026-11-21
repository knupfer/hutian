{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RelaySpec (tests) where

import Control.Concurrent (forkFinally, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryTakeMVar)
import Control.Exception (SomeException, bracket, catch, finally)
import Control.Monad (void)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (toLower)
import Data.Either (fromRight)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Maybe (listToMaybe)
import Data.Word (Word8)
import Hutian.Conversation (ConversationEntry (..))
import Hutian.Http.Wire (recvAll, recvExactly, recvUntil)
import Hutian.Interrupt (newInterruptFlag)
import Hutian.Llm (LlmSettings (..), LlmUrl (..), ModelName (..), SystemPrompt (..), runLlm)
import Hutian.Policy (ToolPolicy (..))
import Hutian.Tool.Config (ToolConfig (..), defaultToolConfig)
import Hutian.Relay
  ( RelayAuth (..)
  , RelayAuthPolicy (..)
  , RelayRuntime
  , RelaySettings (..)
  , defaultRelayMaxBody
  , handleRelayConnection
  , loadRelayAuth
  , maxRelayKeyBytes
  , newRelayRuntime
  , parseRelaySettings
  , redactKey
  , runRelay
  , trimKey
  )
import Hutian.Relay.Protocol (decodeStatus, statusFrameLength)
import Network.Socket
  ( Family (AF_INET, AF_UNIX)
  , SockAddr (SockAddrInet)
  , ShutdownCmd (ShutdownSend)
  , Socket
  , SocketOption (ReuseAddr)
  , SocketType (Stream)
  , accept
  , bind
  , close
  , defaultProtocol
  , listen
  , setSocketOption
  , shutdown
  , socket
  , socketPair
  , socketPort
  , tupleToHostAddress
  )
import Network.Socket.ByteString (sendAll)
import System.Directory (doesPathExist)
import System.IO (IOMode (ReadMode), withFile)
import System.Posix.Files (fileMode, getFileStatus, setFileCreationMask)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
  ( Positive (..)
  , Property
  , counterexample
  , property
  , (.&&.)
  , (===)
  )
import TestSupport
  ( assertInfix
  , assertNotInfix
  , awaitMVar
  , captureStdout
  , stdoutLlmOutput
  , withTemporaryDirectory
  )
import Text.Read (readMaybe)

tests :: TestTree
tests =
  testGroup
    "relay"
    [ settingsTests
    , properties
    , wireTests
    , upstreamTests
    , endToEndTests
    ]

-- * Settings, key loading, and pure helpers

settingsTests :: TestTree
settingsTests =
  testGroup
    "settings"
    [ testCase "parseRelaySettings accepts listen, upstream, and a body limit" $
        parseRelaySettings
          [ "--upstream"
          , "https://example.test/v1/chat/completions"
          , "--listen"
          , "/tmp/llm.sock"
          , "--max-body"
          , "1024"
          ]
          @?= Right
            ( RelaySettings
                "/tmp/llm.sock"
                "https://example.test/v1/chat/completions"
                1024
                RelayAuthRequired
            )
    , testCase "parseRelaySettings uses the default body limit" $
        parseRelaySettings
          [ "--listen"
          , "/run/hutian/llm.sock"
          , "--upstream"
          , "http://127.0.0.1:8080/v1/chat/completions"
          ]
          @?= Right
            ( RelaySettings
                "/run/hutian/llm.sock"
                "http://127.0.0.1:8080/v1/chat/completions"
                defaultRelayMaxBody
                RelayAuthRequired
            )
    , testCase "parseRelaySettings rejects missing, empty, and invalid options" $ do
        assertLeftNames "--listen" (parseRelaySettings ["--upstream", "http://x"])
        assertLeftNames "--upstream" (parseRelaySettings ["--listen", "/tmp/s"])
        assertLeftNames
          "empty value"
          (parseRelaySettings ["--listen", "", "--upstream", "http://x"])
        assertLeftNames
          "empty value"
          (parseRelaySettings ["--listen", "/tmp/s", "--upstream", ""])
        assertLeftNames "--listen" (parseRelaySettings ["--listen"])
        assertLeftNames
          "--max-body"
          (parseRelaySettings ["--listen", "/tmp/s", "--upstream", "http://x", "--max-body", "0"])
        assertLeftNames
          "--max-body"
          (parseRelaySettings ["--listen", "/tmp/s", "--upstream", "http://x", "--max-body", "nope"])
        assertLeftNames
          "duplicate argument"
          (parseRelaySettings ["--listen", "/tmp/s", "--listen", "/tmp/t", "--upstream", "http://x"])
        assertLeftNames
          "duplicate argument"
          (parseRelaySettings ["--listen", "/tmp/s", "--upstream", "http://x", "--no-auth", "--no-auth"])
        assertLeftNames "unknown" (parseRelaySettings ["--unknown"])
    , testCase "parseRelaySettings accepts --no-auth without a value" $
        parseRelaySettings
          [ "--listen"
          , "/run/hutian/llm.sock"
          , "--upstream"
          , "http://127.0.0.1:8080/v1/chat/completions"
          , "--no-auth"
          ]
          @?= Right
            ( RelaySettings
                "/run/hutian/llm.sock"
                "http://127.0.0.1:8080/v1/chat/completions"
                defaultRelayMaxBody
                RelayAuthDisabled
            )
    , testCase "loadRelayAuth reads, trims, and rejects empty input" $
        withTemporaryDirectory "hutian-relay-key" $ \directory -> do
          let path = directory ++ "/key"
          BS.writeFile path "  stdin-key\r\n"
          withFile path ReadMode $ \handle -> do
            key <- loadRelayAuth handle
            key @?= Right (RelayAuthBearer "stdin-key")
          BS.writeFile path " \n\t"
          withFile path ReadMode $ \handle -> do
            key <- loadRelayAuth handle
            key @?= Left "hutian-llm-relay: the API key is empty"
          BS.writeFile path "abc\ndef"
          withFile path ReadMode $ \handle -> do
            key <- loadRelayAuth handle
            key @?= Left "hutian-llm-relay: the API key contains control characters"
          BS.writeFile path (BS.replicate (maxRelayKeyBytes + 1) 97)
          withFile path ReadMode $ \handle -> do
            key <- loadRelayAuth handle
            key @?= Left "hutian-llm-relay: the API key is too long"
    , testCase "trimKey removes surrounding whitespace but not interior bytes" $ do
        trimKey "  secret\r\n" @?= "secret"
        trimKey "\tsecret\n" @?= "secret"
        trimKey "sec ret" @?= "sec ret"
        trimKey "" @?= ""
    , testCase "redactKey removes the secret from messages" $ do
        let message = "request to upstream failed with Bearer s3cret-key"
        assertBool "redacted message still contains the key" (not ("s3cret-key" `isInfixOf` redactKey "s3cret-key" message))
        assertBool "redacted message should mark the secret" ("<redacted>" `isInfixOf` redactKey "s3cret-key" message)
        redactKey "" message @?= message
    ]

properties :: TestTree
properties =
  testGroup
    "properties"
    [ testProperty "trimKey removes only outer whitespace" prop_trimKey
    , testProperty "redactKey never leaves the key in a message" prop_redactKey
    ]

prop_trimKey :: String -> Property
prop_trimKey input =
  counterexample (show trimmed) $
    property (not (hasOuterSpace trimmed))
      .&&. trimKey trimmed === trimmed
  where
    trimmed = trimKey (BS8.pack input)

hasOuterSpace :: ByteString -> Bool
hasOuterSpace bytes =
  maybe False (isKeySpace . fst) (BS.uncons bytes)
    || maybe False (isKeySpace . fst) (BS.uncons (BS.reverse bytes))

isKeySpace :: Word8 -> Bool
isKeySpace byte = byte == 32 || (byte >= 9 && byte <= 13)

prop_redactKey :: Positive Int -> String -> Property
prop_redactKey (Positive number) message =
  let key = BS8.pack (show number)
      redacted = redactKey key message
   in counterexample redacted $
        property (not (BS8.unpack key `isInfixOf` redacted))

-- * Wire-level relay tests over a socketpair

wireTests :: TestTree
wireTests =
  testGroup
    "wire"
    [ testCase "relay forwards the body, injects the key, and streams the response" $
        withUpstream streamingResponse $ \port requests -> do
          runtime <- runtimeFor port defaultRelayMaxBody
          let body = "{\"model\":\"m\",\"stream\":true}"
          (status, response) <- withRelaySession runtime (sendBody body)
          status @?= 200
          assertInfix "answer" response
          assertInfix "data: [DONE]" response
          assertNotInfix "s3cret-key" response
          upstream <- awaitMVar "upstream did not receive a request" requests
          assertInfix "POST /v1/chat/completions HTTP/1.1" upstream
          assertInfix "Authorization: Bearer s3cret-key" upstream
          assertInfix body upstream
    , testCase "relay sends only its own fixed headers upstream" $
        withUpstream streamingResponse $ \port requests -> do
          runtime <- runtimeFor port defaultRelayMaxBody
          _ <- withRelaySession runtime (sendBody "{\"model\":\"m\"}")
          upstream <- awaitMVar "upstream did not receive a request" requests
          let (headPart, _) = BS.breakSubstring "\r\n\r\n" upstream
          assertInfix "Content-Type: application/json" headPart
          assertInfix "Accept: text/event-stream" headPart
          assertInfix "Connection: close" headPart
          assertInfix "Authorization: Bearer s3cret-key" headPart
          assertNotInfix "Cookie:" headPart
          assertNotInfix "X-Evil" headPart
          assertNotInfix "client-key" upstream
    , testCase "relay omits Authorization when no key is configured" $
        withUpstream streamingResponse $ \port requests -> do
          runtime <- runtimeForWithoutKey port defaultRelayMaxBody
          (status, _) <- withRelaySession runtime (sendBody "{}")
          status @?= 200
          upstream <- awaitMVar "upstream did not receive a request" requests
          let (headPart, _) = BS.breakSubstring "\r\n\r\n" upstream
          assertInfix "Content-Type: application/json" headPart
          assertInfix "Accept: text/event-stream" headPart
          assertInfix "Connection: close" headPart
          assertNotInfix "Authorization:" headPart
    , testCase "relay rejects a body larger than the limit" $
        withUpstream streamingResponse $ \port requests -> do
          runtime <- runtimeFor port 8
          (status, _) <- withRelaySession runtime (sendBody "123456789")
          status @?= 413
          assertNothingWithin "upstream must not be contacted" requests
    , testCase "relay forwards upstream errors without leaking the key" $
        withUpstream unauthorizedResponse $ \port _ -> do
          runtime <- runtimeFor port defaultRelayMaxBody
          (status, response) <- withRelaySession runtime (sendBody "{}")
          status @?= 401
          assertInfix "bad key" response
          assertNotInfix "s3cret-key" response
    , testCase "relay sends 502 when the upstream cannot be reached" $ do
        runtime <- runtimeFor 1 defaultRelayMaxBody
        (status, response) <- withRelaySession runtime (sendBody "{}")
        status @?= 502
        assertNotInfix "s3cret-key" response
    , testCase "relay retries once when the upstream closes before headers" $
        withFlakyUpstream streamingResponse $ \port requests -> do
          runtime <- runtimeFor port defaultRelayMaxBody
          (status, response) <- withRelaySession runtime (sendBody "{}")
          status @?= 200
          assertInfix "data: [DONE]" response
          upstream <- awaitMVar "upstream did not receive the retried request" requests
          assertInfix "POST /v1/chat/completions HTTP/1.1" upstream
    , testCase "relay treats a client disconnect as normal" $
        withUpstream largeStreamingResponse $ \port _requests -> do
          logs <- newIORef []
          runtime <-
            newRelayRuntime
              (\message -> modifyIORef' logs (message :))
              ( RelaySettings
                  "/unused.sock"
                  ("http://127.0.0.1:" ++ show port ++ "/v1/chat/completions")
                  defaultRelayMaxBody
                  RelayAuthRequired
              )
              (RelayAuthBearer "s3cret-key")
          (client, server) <- socketPair AF_UNIX Stream defaultProtocol
          done <- newEmptyMVar
          _ <-
            forkFinally
              (handleRelayConnection runtime server)
              (\_ -> close server >> putMVar done ())
          sendAll client "{}"
          shutdown client ShutdownSend
          headResult <- recvExactly client statusFrameLength BS.empty
          case headResult of
            Left _ -> assertFailure "relay did not send a status frame"
            Right _ -> pure ()
          close client
          awaitMVar "relay did not stop after the client disconnected" done
          messages <- readIORef logs
          assertBool
            ("client disconnect was logged as an upstream failure: " ++ show messages)
            (not (any ("upstream request failed" `isInfixOf`) messages))
    ]

-- * Upstream-facing tests

upstreamTests :: TestTree
upstreamTests =
  testGroup
    "upstream"
    [ testCase "relay targets the path and query from the configured URL" $
        withUpstream streamingResponse $ \port requests -> do
          runtime <-
            newRelayRuntime
              silentLogger
              ( RelaySettings
                  "/unused.sock"
                  ("http://127.0.0.1:" ++ show port ++ "/v1/chat/completions?stream=true")
                  defaultRelayMaxBody
                  RelayAuthRequired
              )
              (RelayAuthBearer "s3cret-key")
          _ <- withRelaySession runtime (sendBody "{}")
          upstream <- awaitMVar "upstream did not receive a request" requests
          assertInfix "POST /v1/chat/completions?stream=true HTTP/1.1" upstream
    , testCase "relay does not forward response headers such as Set-Cookie" $
        withUpstream cookieResponse $ \port _ -> do
          runtime <- runtimeFor port defaultRelayMaxBody
          (status, response) <- withRelaySession runtime (sendBody "{}")
          status @?= 200
          assertNotInfix "Set-Cookie" response
          assertInfix "data: ok" response
    , testCase "relay does not follow upstream redirects" $
        withUpstream streamingResponse $ \otherPort otherRequests ->
          withUpstream (redirectResponse otherPort) $ \port _ -> do
            runtime <- runtimeFor port defaultRelayMaxBody
            (status, response) <- withRelaySession runtime (sendBody "{}")
            status @?= 302
            assertInfix "redirected" response
            assertNothingWithin "the relay must not follow redirects" otherRequests
    ]

-- * Full path: shell client -> relay -> upstream

endToEndTests :: TestTree
endToEndTests =
  testGroup
    "end-to-end"
    [ testCase "the shell streams through the relay socket without seeing the key" $
        withTemporaryDirectory "hutian-relay-socket" $ \directory -> do
          withUpstream streamingResponse $ \port requests -> do
            let socketPath = directory ++ "/llm.sock"
                settings =
                  LlmSettings
                    (LlmUrl "http://hutian-relay/v1/chat/completions")
                    (ModelName "test-model")
                    (SystemPrompt "system prompt")
                    (Just socketPath)
                    (defaultToolConfig {toolShell = ToolAllow})
            runtime <-
              newRelayRuntime
                silentLogger
                ( RelaySettings
                    socketPath
                    ("http://127.0.0.1:" ++ show port ++ "/v1/chat/completions")
                    defaultRelayMaxBody
                    RelayAuthRequired
                )
                (RelayAuthBearer "s3cret-key")
            withRelay runtime socketPath $ do
              permissions <- fileMode <$> getFileStatus socketPath
              permissions .&. 0o777 @?= 0o600
              flag <- newInterruptFlag
              captured <-
                timeout 7000000 $
                  captureStdout $
                    runLlm stdoutLlmOutput flag settings [UserEntry "prior"] "current prompt"
              (result, output) <-
                maybe (assertFailure "relay request timed out" >> pure (Nothing, "")) pure captured
              result @?= Just "answer\n"
              assertBool ("answer label missing: " ++ output) ("answer:" `isInfixOf` output)
              assertBool ("relay leaked the key: " ++ output) (not ("s3cret-key" `isInfixOf` output))
              upstream <- awaitMVar "upstream did not receive a request" requests
              assertInfix "Authorization: Bearer s3cret-key" upstream
              assertInfix "current prompt" upstream
    , testCase "the relay socket is private even under a permissive umask" $
        withTemporaryDirectory "hutian-relay-socket" $ \directory -> do
          let socketPath = directory ++ "/llm.sock"
          runtime <-
            newRelayRuntime
              silentLogger
              ( RelaySettings
                  socketPath
                  "http://127.0.0.1:1/v1/chat/completions"
                  defaultRelayMaxBody
                  RelayAuthRequired
              )
              (RelayAuthBearer "s3cret-key")
          previousMask <- setFileCreationMask 0
          withRelay runtime socketPath
            ( do
                permissions <- fileMode <$> getFileStatus socketPath
                permissions .&. 0o077 @?= 0
            )
            `finally` void (setFileCreationMask previousMask)
    , testCase "the relay removes its socket when stopped" $
        withTemporaryDirectory "hutian-relay-socket" $ \directory -> do
          let socketPath = directory ++ "/llm.sock"
          runtime <-
            newRelayRuntime
              silentLogger
              ( RelaySettings
                  socketPath
                  "http://127.0.0.1:1/v1/chat/completions"
                  defaultRelayMaxBody
                  RelayAuthRequired
              )
              (RelayAuthBearer "s3cret-key")
          withRelay runtime socketPath (pure ())
          exists <- doesPathExist socketPath
          assertBool "the relay socket should be removed" (not exists)
    ]

-- * Fixtures

streamingResponse :: ByteString
streamingResponse =
  BS.concat
    [ "HTTP/1.1 200 OK\r\n"
    , "Content-Type: text/event-stream\r\n"
    , "Set-Cookie: leaked=1\r\n"
    , "Connection: close\r\n\r\n"
    , "data: {\"choices\":[{\"delta\":{\"content\":\"answer\\n\"}}]}\n\n"
    , "data: [DONE]\n\n"
    ]

cookieResponse :: ByteString
cookieResponse =
  BS.concat
    [ "HTTP/1.1 200 OK\r\n"
    , "Content-Type: text/event-stream\r\n"
    , "Set-Cookie: leaked=1\r\n"
    , "Connection: close\r\n\r\n"
    , "data: ok\n\n"
    ]

unauthorizedResponse :: ByteString
unauthorizedResponse =
  BS.concat
    [ "HTTP/1.1 401 Unauthorized\r\n"
    , "Content-Type: application/json\r\n"
    , "Connection: close\r\n\r\n"
    , "{\"error\":\"bad key\"}"
    ]

largeStreamingResponse :: ByteString
largeStreamingResponse =
  BS.concat
    [ "HTTP/1.1 200 OK\r\n"
    , "Content-Type: text/event-stream\r\n"
    , "Connection: close\r\n\r\n"
    , BS.replicate (1024 * 1024) 97
    ]

redirectResponse :: Int -> ByteString
redirectResponse port =
  BS.concat
    [ "HTTP/1.1 302 Found\r\n"
    , "Location: http://127.0.0.1:"
    , BS8.pack (show port)
    , "/v1/chat/completions\r\n"
    , "Content-Type: text/plain\r\n"
    , "Connection: close\r\n\r\n"
    , "redirected"
    ]

runtimeFor :: Int -> Int -> IO RelayRuntime
runtimeFor port maxBody =
  newRelayRuntime
    silentLogger
    ( RelaySettings
        "/unused.sock"
        ("http://127.0.0.1:" ++ show port ++ "/v1/chat/completions")
        maxBody
        RelayAuthRequired
    )
    (RelayAuthBearer "s3cret-key")

runtimeForWithoutKey :: Int -> Int -> IO RelayRuntime
runtimeForWithoutKey port maxBody =
  newRelayRuntime
    silentLogger
    ( RelaySettings
        "/unused.sock"
        ("http://127.0.0.1:" ++ show port ++ "/v1/chat/completions")
        maxBody
        RelayAuthDisabled
    )
    RelayAuthNone

silentLogger :: String -> IO ()
silentLogger _ = pure ()

sendBody :: ByteString -> Socket -> IO (Int, ByteString)
sendBody body client = do
  sendAll client body
  shutdown client ShutdownSend
  receiveResponse client

receiveResponse :: Socket -> IO (Int, ByteString)
receiveResponse client = do
  result <- timeout 3000000 (readResponse client)
  maybe (assertFailure "relay response timed out" >> pure (0, "")) pure result

readResponse :: Socket -> IO (Int, ByteString)
readResponse client = do
  frame <- recvExactly client statusFrameLength BS.empty >>= either (fail . show) pure
  status <- either fail pure (decodeStatus frame)
  body <- recvAll client
  pure (status, body)

withRelaySession :: RelayRuntime -> (Socket -> IO a) -> IO a
withRelaySession runtime action = do
  (client, server) <- socketPair AF_UNIX Stream defaultProtocol
  _ <- forkFinally (handleRelayConnection runtime server) (\_ -> close server)
  action client `finally` close client

withRelay :: RelayRuntime -> FilePath -> IO a -> IO a
withRelay runtime socketPath action =
  bracket start stop $ \_ -> waitForSocket socketPath >> action
  where
    start = do
      stopped <- newEmptyMVar
      thread <- forkFinally (runRelay runtime) (\_ -> putMVar stopped ())
      pure (thread, stopped)
    stop (thread, stopped) = do
      killThread thread
      awaitMVar "the relay did not stop" stopped

withUpstream :: ByteString -> (Int -> MVar ByteString -> IO a) -> IO a
withUpstream response action =
  bracket start stop $ \(port, requests, _) -> action port requests
  where
    start = do
      listener <- socket AF_INET Stream defaultProtocol
      setSocketOption listener ReuseAddr 1
      bind listener (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
      listen listener 1
      port <- fromIntegral <$> socketPort listener
      requests <- newEmptyMVar
      _ <- forkIO $
        ( do
            (connection, _) <- accept listener
            request <- readHttpRequest connection
            putMVar requests request
            sendAll connection response
            close connection
        )
          `catch` \(_ :: SomeException) -> pure ()
      pure (port, requests, listener)
    stop (_, _, listener) = close listener `catch` \(_ :: SomeException) -> pure ()

-- | Accept one connection, close it without a response, then accept the retry
-- on a second connection and answer normally. This reproduces the early-close
-- that http-client reports as IncompleteHeaders or ConnectionClosed.
withFlakyUpstream :: ByteString -> (Int -> MVar ByteString -> IO a) -> IO a
withFlakyUpstream response action =
  bracket start stop $ \(port, requests, _) -> action port requests
  where
    start = do
      listener <- socket AF_INET Stream defaultProtocol
      setSocketOption listener ReuseAddr 1
      bind listener (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
      listen listener 2
      port <- fromIntegral <$> socketPort listener
      requests <- newEmptyMVar
      _ <- forkIO $
        ( do
            (first, _) <- accept listener
            _ <- readHttpRequest first
            close first
            (retried, _) <- accept listener
            request <- readHttpRequest retried
            putMVar requests request
            sendAll retried response
            close retried
        )
          `catch` \(_ :: SomeException) -> pure ()
      pure (port, requests, listener)
    stop (_, _, listener) = close listener `catch` \(_ :: SomeException) -> pure ()

readHttpRequest :: Socket -> IO ByteString
readHttpRequest connection = do
  headResult <- recvUntil connection "\r\n\r\n" maxUpstreamHead
  case headResult of
    Left _ -> pure BS.empty
    Right (rawHead, leftover) ->
      case contentLengthOf rawHead of
        Nothing -> pure rawHead
        Just bodyLength -> do
          bodyResult <- recvExactly connection bodyLength leftover
          pure (rawHead <> fromRight BS.empty bodyResult)

contentLengthOf :: ByteString -> Maybe Int
contentLengthOf rawHead =
  listToMaybe
    [ parsed
    | line <- BS8.lines rawHead
    , let (name, rest) = BS8.break (== ':') line
    , BS8.map toLower name == "content-length"
    , let value = BS8.dropWhile (== ' ') (BS8.drop 1 rest)
    , Just parsed <- [readMaybe (BS8.unpack value)]
    ]

maxUpstreamHead :: Int
maxUpstreamHead = 64 * 1024

assertNothingWithin :: String -> MVar ByteString -> IO ()
assertNothingWithin message requests = do
  result <- timeout 300000 (tryTakeMVar requests)
  case result of
    Just (Just _) -> assertFailure ("unexpected upstream request: " ++ message)
    _ -> pure ()

waitForSocket :: FilePath -> IO ()
waitForSocket path = go (0 :: Int)
  where
    go attempts
      | attempts > 200 = assertFailure ("relay socket did not appear: " ++ path)
      | otherwise = do
          exists <- doesPathExist path
          if exists
            then pure ()
            else threadDelay 10000 >> go (attempts + 1)

assertLeftNames :: String -> Either String a -> IO ()
assertLeftNames expected result =
  case result of
    Left message -> assertBool ("expected " ++ show expected ++ " in " ++ show message) (expected `isInfixOf` message)
    Right _ -> assertFailure ("expected failure mentioning " ++ show expected)
