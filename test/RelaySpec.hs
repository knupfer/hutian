{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RelaySpec (tests) where

import Control.Concurrent (forkFinally, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryTakeMVar)
import Control.Exception (SomeException, bracket, catch, finally)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Either (fromRight)
import Data.List (isInfixOf)
import Data.Word (Word8)
import Hutian.Conversation (ConversationEntry (..))
import Hutian.Http.Wire (recvAll, recvExactly, recvUntil)
import Hutian.Interrupt (newInterruptFlag)
import Hutian.Llm (LlmSettings (..), LlmUrl (..), ModelName (..), SystemPrompt (..), runLlm)
import Hutian.Relay
  ( KeySource (..)
  , RelayRuntime
  , RelaySettings (..)
  , defaultRelayMaxBody
  , handleRelayConnection
  , loadRelayKey
  , newRelayRuntime
  , parseRelaySettings
  , runRelay
  )
import Hutian.Relay.Http
  ( RequestHead (..)
  , headerValue
  , parseRequestHead
  , redactKey
  , trimKey
  )
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
import System.Posix.Files (fileMode, getFileStatus)
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
  , withStdinFile
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

-- * Settings, key loading, and pure wire helpers

settingsTests :: TestTree
settingsTests =
  testGroup
    "settings"
    [ testCase "parseRelaySettings accepts a key file" $
        parseRelaySettings
          [ "--listen"
          , "/run/hutian/llm.sock"
          , "--upstream"
          , "http://127.0.0.1:8080/v1/chat/completions"
          , "--key-file"
          , "/run/secrets/hutian-key"
          ]
          @?= Right
            ( RelaySettings
                "/run/hutian/llm.sock"
                "http://127.0.0.1:8080/v1/chat/completions"
                (KeyFile "/run/secrets/hutian-key")
                defaultRelayMaxBody
            )
    , testCase "parseRelaySettings accepts stdin and a body limit" $
        parseRelaySettings
          [ "--upstream"
          , "https://example.test/v1/chat/completions"
          , "--key-stdin"
          , "--listen"
          , "/tmp/llm.sock"
          , "--max-body"
          , "1024"
          ]
          @?= Right
            ( RelaySettings
                "/tmp/llm.sock"
                "https://example.test/v1/chat/completions"
                KeyStdin
                1024
            )
    , testCase "parseRelaySettings rejects missing, conflicting, and invalid options" $ do
        assertLeftNames "--listen" (parseRelaySettings ["--upstream", "http://x", "--key-stdin"])
        assertLeftNames "--upstream" (parseRelaySettings ["--listen", "/tmp/s", "--key-stdin"])
        assertLeftNames
          "key source"
          (parseRelaySettings ["--listen", "/tmp/s", "--upstream", "http://x"])
        assertLeftNames
          "mutually exclusive"
          (parseRelaySettings ["--listen", "/tmp/s", "--upstream", "http://x", "--key-file", "/k", "--key-stdin"])
        assertLeftNames
          "empty value"
          (parseRelaySettings ["--listen", "/tmp/s", "--upstream", "http://x", "--key-file", ""])
        assertLeftNames
          "empty value"
          (parseRelaySettings ["--listen", "", "--upstream", "http://x", "--key-stdin"])
        assertLeftNames
          "empty value"
          (parseRelaySettings ["--listen", "/tmp/s", "--upstream", "", "--key-stdin"])
        assertLeftNames "--listen" (parseRelaySettings ["--listen"])
        assertLeftNames "--max-body" (parseRelaySettings ["--listen", "/tmp/s", "--upstream", "http://x", "--key-stdin", "--max-body", "0"])
        assertLeftNames "--max-body" (parseRelaySettings ["--listen", "/tmp/s", "--upstream", "http://x", "--key-stdin", "--max-body", "nope"])
        assertLeftNames "unknown" (parseRelaySettings ["--unknown"])
    , testCase "loadRelayKey reads, trims, and rejects files" $
        withTemporaryDirectory "hutian-relay-key" $ \directory -> do
          let path = directory ++ "/key"
          BS.writeFile path "  file-key\r\n"
          loadRelayKey (KeyFile path) >>= (@?= Right "file-key")
          BS.writeFile path " \n\t"
          loadRelayKey (KeyFile path) >>= (@?= Left "hutian-llm-relay: the API key is empty")
          loadRelayKey (KeyFile (directory ++ "/missing")) >>= \result ->
            either
              ( \message ->
                  assertBool
                    "a missing key file should be reported by path"
                    ("cannot read key file" `isInfixOf` message)
              )
              (const (assertFailure "a missing key file should fail"))
              result
    , testCase "loadRelayKey reads standard input" $
        withTemporaryDirectory "hutian-relay-key" $ \directory -> do
          let path = directory ++ "/key"
          BS.writeFile path "  stdin-key\n"
          withStdinFile path $
            loadRelayKey KeyStdin >>= (@?= Right "stdin-key")
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
    , testCase "parseRequestHead keeps method, target, and lowercased headers" $
        case parseRequestHead "POST /v1/chat/completions HTTP/1.1\r\nHost: local\r\nX-Test: value\r\n\r\n" of
          Left message -> assertFailure message
          Right request -> do
            headMethod request @?= "POST"
            headTarget request @?= "/v1/chat/completions"
            headVersion request @?= "HTTP/1.1"
            headerValue "host" request @?= Just "local"
            headerValue "X-TEST" request @?= Just "value"
            headerValue "missing" request @?= Nothing
    , testCase "parseRequestHead rejects malformed input" $ do
        assertLeftNames "incomplete" (parseRequestHead "POST / HTTP/1.1")
        assertLeftNames "request line" (parseRequestHead "NOPE\r\n\r\n")
        assertLeftNames "header" (parseRequestHead "POST / HTTP/1.1\r\nbroken\r\n\r\n")
        assertLeftNames "header" (parseRequestHead "POST / HTTP/1.1\r\n\nX-Test: value\r\n\r\n")
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
    [ testCase "relay injects the key, strips the client key, and streams the body" $
        withUpstream streamingResponse $ \port requests -> do
          runtime <- runtimeFor port defaultRelayMaxBody
          let body = "{\"model\":\"m\",\"stream\":true}"
              request =
                postRequest
                  "/v1/chat/completions"
                  body
                  ["Authorization: Bearer client-key\r\n"]
          response <- withRelaySession runtime (requestOn request)
          assertBool "relay did not return 200" ("HTTP/1.1 200" `BS.isPrefixOf` response)
          assertInfix "answer" response
          assertInfix "data: [DONE]" response
          assertNotInfix "s3cret-key" response
          upstream <- awaitMVar "upstream did not receive a request" requests
          assertInfix "POST /v1/chat/completions HTTP/1.1" upstream
          assertInfix "Authorization: Bearer s3cret-key" upstream
          assertNotInfix "client-key" upstream
          assertInfix body upstream
    , testCase "relay rejects a target other than the configured upstream" $
        withUpstream streamingResponse $ \port requests -> do
          runtime <- runtimeFor port defaultRelayMaxBody
          response <-
            withRelaySession runtime $
              requestOn (postRequest "/v1/other" "{}" [])
          assertBool "unexpected target should be forbidden" ("HTTP/1.1 403" `BS.isPrefixOf` response)
          assertNothingWithin "upstream must not be contacted" requests
    , testCase "relay rejects methods and HTTP versions other than POST/1.1" $
        withUpstream streamingResponse $ \port requests -> do
          runtime <- runtimeFor port defaultRelayMaxBody
          getResponse <-
            withRelaySession runtime $
              requestOn
                "GET /v1/chat/completions HTTP/1.1\r\nHost: local\r\nContent-Length: 0\r\n\r\n"
          assertBool "unexpected method should be rejected" ("HTTP/1.1 405" `BS.isPrefixOf` getResponse)
          versionResponse <-
            withRelaySession runtime $
              requestOn
                "POST /v1/chat/completions HTTP/1.0\r\nHost: local\r\nContent-Length: 0\r\n\r\n"
          assertBool "unexpected version should be rejected" ("HTTP/1.1 505" `BS.isPrefixOf` versionResponse)
          assertNothingWithin "upstream must not be contacted" requests
    , testCase "relay requires exactly one valid Content-Length" $
        withUpstream streamingResponse $ \port requests -> do
          runtime <- runtimeFor port defaultRelayMaxBody
          missingLength <-
            withRelaySession runtime $
              requestOn "POST /v1/chat/completions HTTP/1.1\r\nHost: local\r\n\r\n"
          assertBool "missing length should fail" ("HTTP/1.1 400" `BS.isPrefixOf` missingLength)
          invalidLength <-
            withRelaySession runtime $
              requestOn "POST /v1/chat/completions HTTP/1.1\r\nHost: local\r\nContent-Length: nope\r\n\r\n"
          assertBool "invalid length should fail" ("HTTP/1.1 400" `BS.isPrefixOf` invalidLength)
          duplicateLength <-
            withRelaySession runtime $
              requestOn
                "POST /v1/chat/completions HTTP/1.1\r\nHost: local\r\nContent-Length: 0\r\nContent-Length: 0\r\n\r\n"
          assertBool "duplicate length should fail" ("HTTP/1.1 400" `BS.isPrefixOf` duplicateLength)
          assertNothingWithin "upstream must not be contacted" requests
    , testCase "relay rejects chunked, oversized, and truncated bodies" $
        withUpstream streamingResponse $ \port requests -> do
          runtime <- runtimeFor port 8
          chunked <-
            withRelaySession runtime $
              requestOn
                "POST /v1/chat/completions HTTP/1.1\r\nHost: local\r\nTransfer-Encoding: chunked\r\n\r\n"
          assertBool "chunked should fail" ("HTTP/1.1 400" `BS.isPrefixOf` chunked)
          oversized <-
            withRelaySession runtime $
              requestOn (postRequest "/v1/chat/completions" "123456789" [])
          assertBool "oversized body should fail" ("HTTP/1.1 413" `BS.isPrefixOf` oversized)
          truncated <-
            withRelaySession runtime $
              requestAndClose
                "POST /v1/chat/completions HTTP/1.1\r\nHost: local\r\nContent-Length: 5\r\n\r\nabc"
          assertBool
            ("truncated body should fail, got: " ++ show truncated)
            ("HTTP/1.1 400" `BS.isPrefixOf` truncated)
          assertNothingWithin "upstream must not be contacted" requests
    , testCase "relay rejects an oversized request head" $
        withUpstream streamingResponse $ \port requests -> do
          runtime <- runtimeFor port defaultRelayMaxBody
          let request =
                BS.concat
                  [ "POST /v1/chat/completions HTTP/1.1\r\nX-Pad: "
                  , BS.replicate 33000 97
                  , "\r\n\r\n"
                  ]
          response <- withRelaySession runtime (requestOn request)
          assertBool "an oversized head should fail" ("HTTP/1.1 400" `BS.isPrefixOf` response)
          assertNothingWithin "upstream must not be contacted" requests
    , testCase "relay forwards upstream errors without leaking the key" $
        withUpstream unauthorizedResponse $ \port _ -> do
          runtime <- runtimeFor port defaultRelayMaxBody
          response <-
            withRelaySession runtime $
              requestOn (postRequest "/v1/chat/completions" "{}" [])
          assertBool "upstream error should be forwarded" ("HTTP/1.1 401" `BS.isPrefixOf` response)
          assertInfix "bad key" response
          assertNotInfix "s3cret-key" response
    , testCase "relay sends 502 when the upstream cannot be reached" $ do
        runtime <- runtimeFor 1 defaultRelayMaxBody
        response <- withRelaySession runtime (requestOn (postRequest "/v1/chat/completions" "{}" []))
        assertBool "unreachable upstream should be a gateway error" ("HTTP/1.1 502" `BS.isPrefixOf` response)
        assertNotInfix "s3cret-key" response
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
              ( RelaySettings
                  "/unused.sock"
                  ("http://127.0.0.1:" ++ show port ++ "/v1/chat/completions?stream=true")
                  (KeyFile "/unused")
                  defaultRelayMaxBody
              )
              "s3cret-key"
          _ <-
            withRelaySession runtime $
              requestOn (postRequest "/v1/chat/completions?stream=true" "{}" [])
          upstream <- awaitMVar "upstream did not receive a request" requests
          assertInfix "POST /v1/chat/completions?stream=true HTTP/1.1" upstream
    , testCase "relay only sends its own headers upstream" $
        withUpstream streamingResponse $ \port requests -> do
          runtime <- runtimeFor port defaultRelayMaxBody
          _ <-
            withRelaySession runtime $
              requestOn
                ( postRequest
                    "/v1/chat/completions"
                    "{}"
                    [ "Cookie: session=abc\r\n"
                    , "X-Evil: yes\r\n"
                    , "Authorization: Bearer client-key\r\n"
                    ]
                )
          upstream <- awaitMVar "upstream did not receive a request" requests
          assertInfix "Content-Type: application/json" upstream
          assertNotInfix "Cookie:" upstream
          assertNotInfix "X-Evil" upstream
          assertNotInfix "client-key" upstream
    , testCase "relay does not forward response headers such as Set-Cookie" $
        withUpstream cookieResponse $ \port _ -> do
          runtime <- runtimeFor port defaultRelayMaxBody
          response <-
            withRelaySession runtime $
              requestOn (postRequest "/v1/chat/completions" "{}" [])
          assertNotInfix "Set-Cookie" response
          assertInfix "data: ok" response
    , testCase "relay does not follow upstream redirects" $
        withUpstream streamingResponse $ \otherPort otherRequests ->
          withUpstream (redirectResponse otherPort) $ \port _ -> do
            runtime <- runtimeFor port defaultRelayMaxBody
            response <-
              withRelaySession runtime $
                requestOn (postRequest "/v1/chat/completions" "{}" [])
            assertBool "the redirect status should be returned" ("HTTP/1.1 302" `BS.isPrefixOf` response)
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
            runtime <-
              newRelayRuntime
                ( RelaySettings
                    socketPath
                    ("http://127.0.0.1:" ++ show port ++ "/v1/chat/completions")
                    (KeyFile "/unused")
                    defaultRelayMaxBody
                )
                "s3cret-key"
            withRelay runtime socketPath $ do
              permissions <- fileMode <$> getFileStatus socketPath
              permissions .&. 0o777 @?= 0o600
              flag <- newInterruptFlag
              captured <-
                timeout 7000000 $
                  captureStdout $
                    runLlm flag settings [UserEntry "prior"] "current prompt"
              (result, output) <-
                maybe (assertFailure "relay request timed out" >> pure (Nothing, "")) pure captured
              result @?= Just "answer\n"
              assertBool ("answer label missing: " ++ output) ("answer:" `isInfixOf` output)
              assertBool ("relay leaked the key: " ++ output) (not ("s3cret-key" `isInfixOf` output))
              upstream <- awaitMVar "upstream did not receive a request" requests
              assertInfix "Authorization: Bearer s3cret-key" upstream
              assertInfix "current prompt" upstream
    , testCase "the relay removes its socket when stopped" $
        withTemporaryDirectory "hutian-relay-socket" $ \directory -> do
          let socketPath = directory ++ "/llm.sock"
          runtime <-
            newRelayRuntime
              ( RelaySettings
                  socketPath
                  "http://127.0.0.1:1/v1/chat/completions"
                  (KeyFile "/unused")
                  defaultRelayMaxBody
              )
              "s3cret-key"
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
    ( RelaySettings
        "/unused.sock"
        ("http://127.0.0.1:" ++ show port ++ "/v1/chat/completions")
        (KeyFile "/unused")
        maxBody
    )
    "s3cret-key"

postRequest :: ByteString -> ByteString -> [ByteString] -> ByteString
postRequest target body extraHeaders =
  BS.concat $
    [ "POST " <> target <> " HTTP/1.1\r\n"
    , "Host: localhost\r\n"
    , "Content-Type: application/json\r\n"
    ]
      ++ extraHeaders
      ++ [ "Content-Length: " <> BS8.pack (show (BS.length body)) <> "\r\n"
         , "Connection: close\r\n\r\n"
         , body
         ]

withRelaySession :: RelayRuntime -> (Socket -> IO a) -> IO a
withRelaySession runtime action = do
  (client, server) <- socketPair AF_UNIX Stream defaultProtocol
  _ <- forkFinally (handleRelayConnection runtime server) (\_ -> close server)
  action client `finally` close client

requestOn :: ByteString -> Socket -> IO ByteString
requestOn request client = do
  sendAll client request
  receiveResponse client

requestAndClose :: ByteString -> Socket -> IO ByteString
requestAndClose request client = do
  sendAll client request
  shutdown client ShutdownSend
  receiveResponse client

receiveResponse :: Socket -> IO ByteString
receiveResponse client = do
  result <- timeout 3000000 (recvAll client)
  maybe (assertFailure "relay response timed out") pure result

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

readHttpRequest :: Socket -> IO ByteString
readHttpRequest connection = do
  headResult <- recvUntil connection "\r\n\r\n" maxUpstreamHead
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
