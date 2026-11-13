{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Host-side LLM relay. The relay owns the API key and listens on a private
-- Unix socket that is mounted into the sandbox.
--
-- The security property is deliberately small enough to check by reading this
-- module:
--
-- * the relay reads one request body and never interprets it as text;
-- * it sends that body to exactly one upstream 'Request', built once from the
--   configured URL;
-- * the only headers it sends are the fixed ones in 'relayRequest', including
--   the Bearer key when one is configured;
-- * it never follows a redirect, and it returns only a status code plus the
--   upstream body.
--
-- A sandboxed client can therefore spend tokens, but it cannot choose the
-- host, path, headers, or credentials of the upstream request.
module Hutian.Relay
  ( RelaySettings (..)
  , parseRelaySettings
  , defaultRelayMaxBody
  , RelayLogger
  , stderrLogger
  , RelayRuntime
  , newRelayRuntime
  , loadRelayKey
  , trimKey
  , redactKey
  , runRelay
  , handleRelayConnection
  ) where

import Control.Concurrent (forkFinally)
import Control.Exception (IOException, SomeException, bracket, catch, displayException, try)
import Control.Monad (forever, unless, void, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncodingError
import Data.Text.Read (decimal)
import Hutian.Http.Wire (WireError (..), recvAllLimited)
import Hutian.Relay.Protocol (encodeStatus)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
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
import System.Directory (doesPathExist, removeFile)
import System.IO (Handle, hPutStrLn, stderr)
import System.Posix.Files (getFileStatus, isSocket, setFileMode)

-- | Everything the relay needs at startup. The key is deliberately not part of
-- the settings: it is read from standard input so it never appears in argv.
data RelaySettings = RelaySettings
  { relayListenPath :: FilePath
  , relayUpstream :: String
  , relayMaxBody :: Int
  , relayNoAuth :: Bool
  }
  deriving stock (Eq, Show)

-- | 64 MiB is far more than a chat completion request needs while still
-- bounding what one sandboxed client can make the host buffer.
defaultRelayMaxBody :: Int
defaultRelayMaxBody = 64 * 1024 * 1024

-- | Parse the relay command line. The key is never an argument. With
-- @--no-auth@ the relay does not read standard input and forwards without an
-- @Authorization@ header.
parseRelaySettings :: [String] -> Either String RelaySettings
parseRelaySettings = go Nothing Nothing defaultRelayMaxBody False
  where
    go listenPath upstream maxBody noAuth [] =
      RelaySettings
        <$> required "--listen" listenPath
        <*> required "--upstream" upstream
        <*> pure maxBody
        <*> pure noAuth
    go _ _ _ _ ["--listen"] = missing "--listen"
    go _ _ _ _ ["--upstream"] = missing "--upstream"
    go _ _ _ _ ["--max-body"] = missing "--max-body"
    go _ upstream maxBody noAuth ("--listen" : value : rest) =
      go (Just value) upstream maxBody noAuth rest
    go listenPath _ maxBody noAuth ("--upstream" : value : rest) =
      go listenPath (Just value) maxBody noAuth rest
    go listenPath upstream _ noAuth ("--max-body" : value : rest) = do
      parsed <- parsePositive "--max-body" value
      go listenPath upstream parsed noAuth rest
    go listenPath upstream maxBody _ ("--no-auth" : rest) =
      go listenPath upstream maxBody True rest
    go _ _ _ _ (argument : _) =
      Left ("unknown argument: " ++ argument ++ "\n" ++ relayUsage)

    required name value =
      case value of
        Nothing -> Left ("missing required argument: " ++ name ++ "\n" ++ relayUsage)
        Just "" -> Left ("empty value for " ++ name ++ "\n" ++ relayUsage)
        Just found -> Right found
    missing name = Left ("missing value for " ++ name ++ "\n" ++ relayUsage)

parsePositive :: String -> String -> Either String Int
parsePositive flag value =
  case decimal (Text.pack value) of
    Right (number, rest)
      | Text.null rest && number > 0 -> Right number
    _ -> Left ("invalid value for " ++ flag ++ ": " ++ value ++ "\n" ++ relayUsage)

relayUsage :: String
relayUsage =
  "usage: hutian-llm-relay --listen PATH --upstream URL [--max-body BYTES] [--no-auth]\n"
    ++ "the API key is read from standard input unless --no-auth is given"

-- | Read the key from the given handle. The launcher pipes it from the
-- configured file or command into standard input, so the key never appears in
-- an argument list or environment.
loadRelayKey :: Handle -> IO (Either String BS.ByteString)
loadRelayKey handle = validateKey <$> BS.hGetContents handle

validateKey :: BS.ByteString -> Either String BS.ByteString
validateKey raw
  | BS.null key = Left "hutian-llm-relay: the API key is empty"
  | BS.any isControl key =
      Left "hutian-llm-relay: the API key contains control characters"
  | otherwise = Right key
  where
    key = trimKey raw
    isControl byte = byte < 0x20 || byte == 0x7f

-- | Remove surrounding ASCII whitespace from a key read from a file or stdin.
trimKey :: BS.ByteString -> BS.ByteString
trimKey = BS.dropWhile isKeySpace . BS.dropWhileEnd isKeySpace
  where
    isKeySpace byte = byte == 32 || (byte >= 9 && byte <= 13)

-- | Remove a secret from a log line so relay errors never echo the key.
redactKey :: BS.ByteString -> String -> String
redactKey key message
  | BS.null key = message
  | otherwise =
      Text.unpack $
        Text.replace
          (TextEncoding.decodeUtf8With TextEncodingError.lenientDecode key)
          "<redacted>"
          (Text.pack message)

-- | A prepared relay: the key, a shared HTTP manager, and the single allowed
-- upstream request. Deliberately has no 'Show' instance.
data RelayRuntime = RelayRuntime
  { runtimeListenPath :: FilePath
  , runtimeMaxBody :: Int
  , runtimeKey :: Maybe BS.ByteString
  , runtimeManager :: Manager
  , runtimeUpstream :: Request
  , runtimeLogger :: RelayLogger
  }

type RelayLogger = String -> IO ()

-- | Write relay diagnostics to standard error.
stderrLogger :: RelayLogger
stderrLogger message = hPutStrLn stderr ("hutian-llm-relay: " ++ message)

newRelayRuntime :: RelayLogger -> RelaySettings -> Maybe BS.ByteString -> IO RelayRuntime
newRelayRuntime logger settings key = do
  upstream <- parseRequest (relayUpstream settings)
  manager <- newManager tlsManagerSettings
  pure
    RelayRuntime
      { runtimeListenPath = relayListenPath settings
      , runtimeMaxBody = relayMaxBody settings
      , runtimeKey = key
      , runtimeManager = manager
      , runtimeUpstream = upstream
      , runtimeLogger = logger
      }

-- | Bind the Unix socket and serve until the process is stopped.
runRelay :: RelayRuntime -> IO ()
runRelay runtime =
  bracket (listenUnix path) release $ \listener ->
    forever $ do
      (connection, _) <- accept listener
      _ <- forkFinally (handleRelayConnection runtime connection) (\_ -> close connection)
      pure ()
  where
    path = runtimeListenPath runtime
    release listener = do
      close listener
      removeSocketFile path

listenUnix :: FilePath -> IO Socket
listenUnix path = do
  removeStaleSocket path
  sock <- socket AF_UNIX Stream defaultProtocol
  bind sock (SockAddrUnix path)
  listen sock 16
  setFileMode path 0o600
  pure sock

removeStaleSocket :: FilePath -> IO ()
removeStaleSocket path = do
  exists <- doesPathExist path
  when exists $ do
    status <- getFileStatus path
    if isSocket status
      then removeFile path
      else ioError (userError ("hutian-llm-relay: refusing to remove non-socket: " ++ path))

removeSocketFile :: FilePath -> IO ()
removeSocketFile path = do
  exists <- doesPathExist path
  when exists (removeFile path)

-- | Handle one already-accepted connection. Exposed so tests can drive the
-- relay over a socketpair without starting the accept loop.
handleRelayConnection :: RelayRuntime -> Socket -> IO ()
handleRelayConnection runtime connection = do
  result <- try (serve runtime connection) :: IO (Either IOException ())
  case result of
    Left exception ->
      logRelay runtime ("connection failed: " ++ displayException exception)
    Right () -> pure ()

serve :: RelayRuntime -> Socket -> IO ()
serve runtime connection = do
  bodyResult <- recvAllLimited connection (runtimeMaxBody runtime)
  case bodyResult of
    Left WireTooLarge ->
      sendError connection 413 "request body is too large"
    Left WireClosed -> pure ()
    Right body -> forward runtime connection body

-- | Build the one request the relay is allowed to send. The sandboxed client
-- contributes only the body; every other field is fixed here or in the
-- configured upstream URL.
relayRequest :: RelayRuntime -> BS.ByteString -> Request
relayRequest runtime body =
  (runtimeUpstream runtime)
    { method = "POST"
    , requestHeaders =
        [ ("Content-Type", "application/json")
        , ("Accept", "text/event-stream")
        ]
          ++ maybe
            []
            (\key -> [("Authorization", "Bearer " <> key)])
            (runtimeKey runtime)
    , requestBody = RequestBodyLBS (BL.fromStrict body)
    , responseTimeout = responseTimeoutNone
    , redirectCount = 0
    , checkResponse = \_ _ -> pure ()
    }

forward :: RelayRuntime -> Socket -> BS.ByteString -> IO ()
forward runtime connection body = do
  sent <- newIORef False
  result <-
    try
      ( withResponse (relayRequest runtime body) (runtimeManager runtime) $ \response -> do
          writeIORef sent True
          connected <- sendToClient connection (encodeStatus (statusCode (responseStatus response)))
          when connected (streamBody connection (brRead (responseBody response)))
      )
  case result of
    Left (exception :: SomeException) -> do
      alreadySent <- readIORef sent
      unless alreadySent $
        sendError connection 502 "upstream request failed"
      logRelay runtime ("upstream request failed: " ++ displayException exception)
    Right () -> pure ()

streamBody :: Socket -> IO BS.ByteString -> IO ()
streamBody connection readChunk = do
  chunk <- readChunk
  unless (BS.null chunk) $ do
    connected <- sendToClient connection chunk
    when connected (streamBody connection readChunk)

-- | Best-effort write to the sandbox. A closed client is normal: the shell
-- stops reading once the answer is complete. It must not be reported as an
-- upstream failure.
sendToClient :: Socket -> BS.ByteString -> IO Bool
sendToClient connection bytes =
  (sendAll connection bytes >> pure True)
    `catch` \(_ :: IOException) -> pure False

sendError :: Socket -> Int -> String -> IO ()
sendError connection code message =
  void $ sendToClient connection (encodeStatus code <> BS8.pack message)

-- * Logging

logRelay :: RelayRuntime -> String -> IO ()
logRelay runtime message =
  runtimeLogger runtime $
    maybe message (`redactKey` message) (runtimeKey runtime)
