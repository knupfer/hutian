{-# LANGUAGE OverloadedStrings #-}

module WireSpec (tests) where

import Control.Exception (bracket)
import qualified Data.ByteString as BS
import Hutian.Http.Wire
import Network.Socket
  ( Family (AF_UNIX)
  , ShutdownCmd (ShutdownSend)
  , Socket
  , SocketType (Stream)
  , close
  , defaultProtocol
  , shutdown
  , socketPair
  )
import Network.Socket.ByteString (sendAll)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "wire"
    [ testGroup
        "recvUntil"
        [ testCase "returns the marker-terminated head and leaves the tail readable" $
            withSocketPair $ \(reader, writer) -> do
              sendAll writer "abcMARKtail"
              shutdown writer ShutdownSend
              result <- await "recvUntil" (recvUntil reader "MARK" 16)
              (headPart, leftover) <- case result of
                Left err -> assertFailure ("recvUntil failed: " ++ show err)
                Right parts -> pure parts
              rest <- await "recvAll after recvUntil" (recvAll reader)
              headPart @?= "abcMARK"
              headPart <> leftover <> rest @?= "abcMARKtail"
        , testCase "fails when no marker fits inside the limit" $
            withSocketPair $ \(reader, writer) -> do
              sendAll writer "abcdef"
              shutdown writer ShutdownSend
              recvUntil reader "MARK" 3
                >>= (@?= Left WireTooLarge)
        , testCase "rejects a marker that appears beyond the limit" $
            withSocketPair $ \(reader, writer) -> do
              sendAll writer "abcdefMARK"
              shutdown writer ShutdownSend
              recvUntil reader "MARK" 3
                >>= (@?= Left WireTooLarge)
        , testCase "reports a closed connection without a marker" $
            withSocketPair $ \(reader, writer) -> do
              shutdown writer ShutdownSend
              recvUntil reader "MARK" 16
                >>= (@?= Left WireClosed)
        ]
    , testGroup
        "recvExactly"
        [ testCase "returns buffered bytes without reading the socket" $
            withSocketPair $ \(reader, _) ->
              recvExactly reader 3 "abcdef"
                >>= (@?= Right "abc")
        , testCase "reads the remaining bytes from the socket" $
            withSocketPair $ \(reader, writer) -> do
              sendAll writer "llo"
              shutdown writer ShutdownSend
              recvExactly reader 5 "he"
                >>= (@?= Right "hello")
        , testCase "reports a closed connection before the expected length" $
            withSocketPair $ \(reader, writer) -> do
              shutdown writer ShutdownSend
              recvExactly reader 5 BS.empty
                >>= (@?= Left WireClosed)
        , testCase "always satisfies a zero-length request" $
            withSocketPair $ \(reader, _) ->
              recvExactly reader 0 BS.empty
                >>= (@?= Right "")
        ]
    , testGroup
        "recvAll"
        [ testCase "reads until the peer closes" $
            withSocketPair $ \(reader, writer) -> do
              sendAll writer "hello"
              shutdown writer ShutdownSend
              await "recvAll" (recvAll reader)
                >>= (@?= "hello")
        , testCase "returns an empty string when the peer sends nothing" $
            withSocketPair $ \(reader, writer) -> do
              shutdown writer ShutdownSend
              await "recvAll" (recvAll reader)
                >>= (@?= "")
        , testCase "accepts a body within the limit" $
            withSocketPair $ \(reader, writer) -> do
              sendAll writer "hello"
              shutdown writer ShutdownSend
              await "recvAllLimited" (recvAllLimited reader 5)
                >>= (@?= Right "hello")
        , testCase "rejects a body beyond the limit" $
            withSocketPair $ \(reader, writer) -> do
              sendAll writer "hello"
              shutdown writer ShutdownSend
              await "recvAllLimited" (recvAllLimited reader 4)
                >>= (@?= Left WireTooLarge)
        , testCase "treats an empty connection as an empty body" $
            withSocketPair $ \(reader, writer) -> do
              shutdown writer ShutdownSend
              await "recvAllLimited" (recvAllLimited reader 4)
                >>= (@?= Right "")
        ]
    ]

withSocketPair :: ((Socket, Socket) -> IO a) -> IO a
withSocketPair =
  bracket
    (socketPair AF_UNIX Stream defaultProtocol)
    (\(left, right) -> close left >> close right)

await :: String -> IO a -> IO a
await label action = do
  result <- timeout 3000000 action
  maybe (assertFailure (label ++ " timed out") >> ioError (userError label)) pure result
