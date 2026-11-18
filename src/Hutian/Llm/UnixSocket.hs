{-# LANGUAGE OverloadedStrings #-}

-- | Minimal request/response client for the host relay's Unix socket. The only
-- bytes the sandbox sends are the request body; the relay adds the target,
-- headers, and credentials on the host.
module Hutian.Llm.UnixSocket
  ( postUnixStream
  ) where

import Control.Exception (bracket, onException)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Hutian.Http.Wire (WireError (..), recvExactly)
import Hutian.Relay.Protocol (decodeStatus, statusFrameLength)
import Network.Socket
  ( Family (AF_UNIX)
  , ShutdownCmd (ShutdownSend)
  , SockAddr (SockAddrUnix)
  , Socket
  , SocketType (Stream)
  , close
  , connect
  , defaultProtocol
  , shutdown
  , socket
  )
import Network.Socket.ByteString (recv, sendAll)

-- | Send one request body, half-close the socket, then hand the upstream status
-- code and streaming body reader to the continuation. The body is delimited by
-- connection close.
postUnixStream
  :: FilePath
  -> ByteString
  -> (Int -> IO ByteString -> IO a)
  -> IO a
postUnixStream path body action =
  bracket (connectUnix path) close $ \connection -> do
    sendAll connection body
    shutdown connection ShutdownSend
    status <- readStatus connection
    action status (recv connection 32768)

connectUnix :: FilePath -> IO Socket
connectUnix path = do
  connection <- socket AF_UNIX Stream defaultProtocol
  connect connection (SockAddrUnix path)
    `onException` close connection
  pure connection

readStatus :: Socket -> IO Int
readStatus connection = do
  frameResult <- recvExactly connection statusFrameLength BS.empty
  frame <-
    case frameResult of
      Left WireTooLarge -> ioError (userError "llm: relay status frame is too large")
      Left WireClosed -> ioError (userError "llm: relay closed before sending a status")
      Right bytes -> pure bytes
  either (ioError . userError) pure (decodeStatus frame)
