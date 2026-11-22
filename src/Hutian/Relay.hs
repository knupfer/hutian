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
--   upstream body;
-- * a connection failure before any response headers may retry the same fixed
--   request once on a fresh upstream connection.
--
-- A sandboxed client can therefore spend tokens, but it cannot choose the
-- host, path, headers, or credentials of the upstream request.
module Hutian.Relay
  ( RelayAuth (..)
  , RelayAuthPolicy (..)
  , RelaySettings (RelaySettings, relayAuthPolicy)
  , parseRelaySettings
  , defaultRelayMaxBody
  , maxRelayKeyBytes
  , stderrLogger
  , RelayRuntime
  , newRelayRuntime
  , loadRelayAuth
  , trimKey
  , redactKey
  , runRelay
  , handleRelayConnection
  ) where

import Control.Concurrent (forkFinally)
import Control.Exception
  ( Handler (..)
  , IOException
  , bracket
  , bracketOnError
  , catch
  , catches
  , displayException
  , try
  )
import Control.Monad (forever, unless, void, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncodingError
import Data.Text.Read (decimal)
import Hutian.Cli
  ( OptionKind (..)
  , flagPresent
  , missingOptions
  , optionValueFor
  , parseOptions
  )
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
import System.Posix.Files
  ( getFileStatus
  , isSocket
  , setFileCreationMask
  , setFileMode
  )

-- | Whether the relay requires a key. The key itself arrives separately so it
-- never becomes part of parsed settings.
data RelayAuthPolicy
  = RelayAuthRequired
  | RelayAuthDisabled
  deriving stock (Eq, Show)

-- | The credential used for upstream requests. 'Show' never reveals the key.
data RelayAuth
  = RelayAuthNone
  | RelayAuthBearer BS.ByteString
  deriving stock (Eq)

instance Show RelayAuth where
  show RelayAuthNone = "RelayAuthNone"
  show (RelayAuthBearer _) = "RelayAuthBearer <redacted>"

-- | Everything the relay needs at startup. The key is deliberately not part of
-- the settings: it is read from standard input so it never appears in argv.
data RelaySettings = RelaySettings
  { relayListenPath :: FilePath
  , relayUpstream :: String
  , relayMaxBody :: Int
  , relayAuthPolicy :: RelayAuthPolicy
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
parseRelaySettings arguments = do
  options <- withUsage (parseOptions relayOptionSpec arguments)
  case missingOptions requiredRelayOptions options of
    [] -> do
      listenPath <- required "--listen" options
      upstream <- required "--upstream" options
      maxBody <-
        case optionValueFor "--max-body" options of
          Nothing -> Right defaultRelayMaxBody
          Just value -> parsePositive "--max-body" value
      pure
        RelaySettings
          { relayListenPath = listenPath
          , relayUpstream = upstream
          , relayMaxBody = maxBody
          , relayAuthPolicy =
              if flagPresent "--no-auth" options
                then RelayAuthDisabled
                else RelayAuthRequired
          }
    missing -> Left (missingMessage missing)
  where
    required name options =
      case optionValueFor name options of
        Just "" -> Left ("empty value for " ++ name ++ "\n" ++ relayUsage)
        Just value -> Right value
        Nothing -> Left ("missing required argument: " ++ name ++ "\n" ++ relayUsage)

    withUsage result =
      case result of
        Left message -> Left (message ++ "\n" ++ relayUsage)
        Right options -> Right options

    missingMessage missing =
      "missing required argument"
        ++ (if length missing > 1 then "s" else "")
        ++ ": "
        ++ unwords missing
        ++ "\n"
        ++ relayUsage

relayOptionSpec :: [(String, OptionKind)]
relayOptionSpec =
  [ ("--listen", OptionValue)
  , ("--upstream", OptionValue)
  , ("--max-body", OptionValue)
  , ("--no-auth", OptionFlag)
  ]

requiredRelayOptions :: [String]
requiredRelayOptions = ["--listen", "--upstream"]

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

-- | A key is a bearer token, not a document. Reading a bounded prefix keeps a
-- misconfigured file or command from making the relay buffer arbitrary data.
maxRelayKeyBytes :: Int
maxRelayKeyBytes = 64 * 1024

-- | Read the key from the given handle. The launcher pipes it from the
-- configured file or command into standard input, so the key never appears in
-- an argument list or environment.
loadRelayAuth :: Handle -> IO (Either String RelayAuth)
loadRelayAuth handle = fmap (fmap RelayAuthBearer . validateKey) (BS.hGet handle (maxRelayKeyBytes + 1))

validateKey :: BS.ByteString -> Either String BS.ByteString
validateKey raw
  | BS.length raw > maxRelayKeyBytes =
      Left "hutian-llm-relay: the API key is too long"
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
  , runtimeAuth :: RelayAuth
  , runtimeManager :: Manager
  , runtimeUpstream :: Request
  , runtimeLogger :: RelayLogger
  }

type RelayLogger = String -> IO ()

-- | Write relay diagnostics to standard error.
stderrLogger :: RelayLogger
stderrLogger message = hPutStrLn stderr ("hutian-llm-relay: " ++ message)

newRelayRuntime :: RelayLogger -> RelaySettings -> RelayAuth -> IO RelayRuntime
newRelayRuntime logger settings auth = do
  upstream <- parseRequest (relayUpstream settings)
  manager <- newManager tlsManagerSettings
  pure
    RelayRuntime
      { runtimeListenPath = relayListenPath settings
      , runtimeMaxBody = relayMaxBody settings
      , runtimeAuth = auth
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
  -- bind creates the socket with mode 0777 reduced by the process umask, and
  -- the chmod below cannot happen before that instant. Narrowing the umask
  -- means the socket is never group- or world-accessible, even briefly.
  bracket (setFileCreationMask 0o077) setFileCreationMask $ \_ ->
    bracketOnError
      (socket AF_UNIX Stream defaultProtocol)
      close
      ( \sock -> do
          bind sock (SockAddrUnix path)
          setFileMode path 0o600
          listen sock 16
          pure sock
      )

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
        -- Do not reuse an upstream keep-alive connection. Model servers often
        -- close idle connections without a final response, and reusing one
        -- surfaces as an IncompleteHeaders failure before any reply arrives.
        , ("Connection", "close")
        ]
          ++ authHeaders (runtimeAuth runtime)
    , requestBody = RequestBodyLBS (BL.fromStrict body)
    , responseTimeout = responseTimeoutNone
    , redirectCount = 0
    , checkResponse = \_ _ -> pure ()
    }
  where
    authHeaders RelayAuthNone = []
    authHeaders (RelayAuthBearer key) = [("Authorization", "Bearer " <> key)]

-- | One upstream attempt's failure. HTTP failures can be retried when no
-- response headers reached the sandbox; socket failures cannot be classified
-- as safely, so they are reported as-is.
data ForwardFailure
  = ForwardHttp HttpException
  | ForwardIo IOException

forward :: RelayRuntime -> Socket -> BS.ByteString -> IO ()
forward runtime connection body = do
  sent <- newIORef False
  result <- attempt upstreamAttempts sent
  case result of
    Left message -> do
      alreadySent <- readIORef sent
      unless alreadySent $
        sendError connection 502 "upstream request failed"
      logRelay runtime ("upstream request failed: " ++ message)
    Right () -> pure ()
  where
    attempt attemptsLeft sent = do
      outcome <- tryForward sent
      case outcome of
        Right () -> pure (Right ())
        Left failure -> do
          alreadySent <- readIORef sent
          if attemptsLeft > 1 && not alreadySent && retryableFailure failure
            then attempt (attemptsLeft - 1) sent
            else pure (Left (failureText failure))

    tryForward sent =
      ( Right
          <$> withResponse
            (relayRequest runtime body)
            (runtimeManager runtime)
            ( \response -> do
                writeIORef sent True
                connected <-
                  sendToClient
                    connection
                    (encodeStatus (statusCode (responseStatus response)))
                when connected (streamBody connection (brRead (responseBody response)))
            )
      )
        `catches`
          [ Handler (pure . Left . ForwardHttp)
          , Handler (pure . Left . ForwardIo)
          ]

-- | A model server may close a fresh connection before its headers arrive, or
-- the manager may hand back a connection the server has already closed. One
-- retry covers both without risking a duplicate request after a response began.
upstreamAttempts :: Int
upstreamAttempts = 2

retryableFailure :: ForwardFailure -> Bool
retryableFailure (ForwardHttp (HttpExceptionRequest _ content)) =
  case content of
    IncompleteHeaders -> True
    ConnectionClosed -> True
    NoResponseDataReceived -> True
    _ -> False
retryableFailure _ = False

failureText :: ForwardFailure -> String
failureText (ForwardHttp exception) = displayException exception
failureText (ForwardIo exception) = displayException exception

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
  runtimeLogger runtime $ case runtimeAuth runtime of
    RelayAuthNone -> message
    RelayAuthBearer key -> redactKey key message
