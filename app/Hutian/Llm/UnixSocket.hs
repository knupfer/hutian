{-# LANGUAGE OverloadedStrings #-}

-- | Minimal HTTP/1.1 client over a Unix domain socket. The sandbox never sees
-- the relay URL's host or credentials: it only speaks to the socket path the
-- launcher mounted into the sandbox.
module Hutian.Llm.UnixSocket
  ( postUnixStream
  , requestBytes
  , readStatusFromHead
  ) where

import Control.Exception (bracket, onException)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef (newIORef, readIORef, writeIORef)
import Hutian.Http.Wire (WireError (..), recvUntil)
import Network.Socket
  ( Family (AF_UNIX)
  , SockAddr (SockAddrUnix)
  , Socket
  , SocketType (Stream)
  , close
  , connect
  , defaultProtocol
  , socket
  )
import Network.Socket.ByteString (recv, sendAll)
import Text.Read (readMaybe)

maxResponseHead :: Int
maxResponseHead = 64 * 1024

-- | Send one POST and hand the status code and streaming body reader to the
-- continuation. The body is delimited by connection close, matching the relay.
postUnixStream
  :: FilePath
  -> ByteString
  -> ByteString
  -> (Int -> IO ByteString -> IO a)
  -> IO a
postUnixStream path target body action =
  bracket (connectUnix path) close $ \connection -> do
    sendAll connection (requestBytes target body)
    (status, leftover) <- readResponseHead connection
    reader <- newReader connection leftover
    action status reader

connectUnix :: FilePath -> IO Socket
connectUnix path = do
  connection <- socket AF_UNIX Stream defaultProtocol
  connect connection (SockAddrUnix path)
    `onException` close connection
  pure connection

-- | Build the request the relay expects. There is deliberately no
-- Authorization header: the relay adds the key on the host.
requestBytes :: ByteString -> ByteString -> ByteString
requestBytes target body =
  BS.concat
    [ "POST "
    , target
    , " HTTP/1.1\r\nHost: localhost\r\n"
    , "Content-Type: application/json\r\n"
    , "Accept: text/event-stream\r\n"
    , "Content-Length: "
    , BS8.pack (show (BS.length body))
    , "\r\nConnection: close\r\n\r\n"
    , body
    ]

newReader :: Socket -> ByteString -> IO (IO ByteString)
newReader connection initial = do
  buffered <- newIORef initial
  pure $ do
    pending <- readIORef buffered
    if BS.null pending
      then recv connection 32768
      else do
        writeIORef buffered BS.empty
        pure pending

readResponseHead :: Socket -> IO (Int, ByteString)
readResponseHead connection = do
  result <- recvUntil connection "\r\n\r\n" maxResponseHead
  case result of
    Left WireTooLarge -> ioError (userError "llm: relay response head is too large")
    Left WireClosed -> ioError (userError "llm: relay closed before sending a response")
    Right (rawHead, leftover) ->
      case readStatusFromHead rawHead of
        Left message -> ioError (userError message)
        Right status -> pure (status, leftover)

-- | Read the status code from a response head. A malformed status line is an
-- error rather than a silently accepted response.
readStatusFromHead :: ByteString -> Either String Int
readStatusFromHead rawHead =
  case BS8.words (BS8.takeWhile (/= '\r') rawHead) of
    (version : codeText : _)
      | "HTTP/" `BS8.isPrefixOf` version
      , Just code <- readMaybe (BS8.unpack codeText)
      , code >= 100 && code <= 599 ->
          Right code
    _ -> Left "llm: invalid relay response status"
