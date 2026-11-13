{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Host-side LLM relay. The relay owns the API key and listens on a private
-- Unix socket that is mounted into the sandbox. It forwards only one fixed
-- request target to one fixed upstream, so the sandbox can spend tokens but can
-- never read or redirect the credential.
module Hutian.Relay
  ( RelaySettings (..)
  , KeySource (..)
  , parseRelaySettings
  , defaultRelayMaxBody
  , RelayRuntime
  , newRelayRuntime
  , setRelayLogger
  , loadRelayKey
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
import Data.Text.Read (decimal)
import qualified Data.Text as Text
import Data.Word (Word8)
import Hutian.Http.Wire (WireError (..), recvExactly, recvUntil)
import Hutian.Relay.Http
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (statusCode, statusMessage)
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
import System.IO (hPutStrLn, stderr)
import System.Posix.Files (getFileStatus, isSocket, setFileMode)

-- | Everything the relay needs at startup.
data RelaySettings = RelaySettings
  { relayListenPath :: FilePath
  , relayUpstream :: String
  , relayKeySource :: KeySource
  , relayMaxBody :: Int
  }
  deriving stock (Eq, Show)

data KeySource
  = KeyFile FilePath
  | KeyStdin
  deriving stock (Eq, Show)

-- | One HTTP error the relay can return without involving the upstream.
data RelayError = RelayError
  { relayErrorCode :: Int
  , relayErrorReason :: BS.ByteString
  , relayErrorMessage :: String
  }

badRequest :: String -> RelayError
badRequest = RelayError 400 "Bad Request"

-- | 64 MiB is far more than a chat completion request needs while still
-- bounding what one sandboxed client can make the host buffer.
defaultRelayMaxBody :: Int
defaultRelayMaxBody = 64 * 1024 * 1024

maxRequestHead :: Int
maxRequestHead = 32 * 1024

-- | Parse the relay command line.
parseRelaySettings :: [String] -> Either String RelaySettings
parseRelaySettings = go emptyPartial
  where
    go partial [] = finish partial
    go _ ["--listen"] = missing "--listen"
    go _ ["--upstream"] = missing "--upstream"
    go _ ["--key-file"] = missing "--key-file"
    go _ ["--max-body"] = missing "--max-body"
    go partial ("--listen" : value : rest) =
      go partial {partialListen = Just value} rest
    go partial ("--upstream" : value : rest) =
      go partial {partialUpstream = Just value} rest
    go partial ("--key-file" : value : rest) =
      go partial {partialKeyFile = Just value} rest
    go partial ("--key-stdin" : rest) =
      go partial {partialKeyStdin = True} rest
    go partial ("--max-body" : value : rest) = do
      parsed <- parsePositive "--max-body" value
      go partial {partialMaxBody = parsed} rest
    go _ (argument : _) = Left ("unknown argument: " ++ argument ++ "\n" ++ relayUsage)

    finish partial = do
      listenPath <- required "--listen" (partialListen partial)
      upstreamUrl <- required "--upstream" (partialUpstream partial)
      keySource <- keySourceFor partial
      pure
        RelaySettings
          { relayListenPath = listenPath
          , relayUpstream = upstreamUrl
          , relayKeySource = keySource
          , relayMaxBody = partialMaxBody partial
          }

    required name value =
      case value of
        Nothing -> Left ("missing required argument: " ++ name ++ "\n" ++ relayUsage)
        Just "" -> Left ("empty value for " ++ name ++ "\n" ++ relayUsage)
        Just found -> Right found
    missing name = Left ("missing value for " ++ name ++ "\n" ++ relayUsage)

keySourceFor :: Partial -> Either String KeySource
keySourceFor partial =
  case (partialKeyFile partial, partialKeyStdin partial) of
    (Just path, False)
      | null path -> Left ("empty value for --key-file\n" ++ relayUsage)
      | otherwise -> Right (KeyFile path)
    (Nothing, True) -> Right KeyStdin
    (Just _, True) -> Left ("--key-file and --key-stdin are mutually exclusive\n" ++ relayUsage)
    (Nothing, False) ->
      Left ("missing key source: pass --key-file PATH or --key-stdin\n" ++ relayUsage)

parsePositive :: String -> String -> Either String Int
parsePositive flag value =
  case decimal (Text.pack value) of
    Right (number, rest)
      | Text.null rest && number > 0 -> Right number
    _ -> Left ("invalid value for " ++ flag ++ ": " ++ value ++ "\n" ++ relayUsage)

data Partial = Partial
  { partialListen :: Maybe String
  , partialUpstream :: Maybe String
  , partialKeyFile :: Maybe FilePath
  , partialKeyStdin :: Bool
  , partialMaxBody :: Int
  }

emptyPartial :: Partial
emptyPartial =
  Partial
    { partialListen = Nothing
    , partialUpstream = Nothing
    , partialKeyFile = Nothing
    , partialKeyStdin = False
    , partialMaxBody = defaultRelayMaxBody
    }

relayUsage :: String
relayUsage =
  "usage: hutian-llm-relay --listen PATH --upstream URL (--key-file PATH | --key-stdin) [--max-body BYTES]"

-- | Read the key from its source. The key is never accepted on the command
-- line, and errors never include key material.
loadRelayKey :: KeySource -> IO (Either String BS.ByteString)
loadRelayKey KeyStdin = validateKey <$> BS.getContents
loadRelayKey (KeyFile path) = do
  result <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
  pure $ case result of
    Left _ -> Left ("hutian-llm-relay: cannot read key file: " ++ path)
    Right raw -> validateKey raw

validateKey :: BS.ByteString -> Either String BS.ByteString
validateKey raw
  | BS.null key = Left "hutian-llm-relay: the API key is empty"
  | otherwise = Right key
  where
    key = trimKey raw

-- | A prepared relay: the key, a shared HTTP manager, and the single allowed
-- upstream request. Deliberately has no 'Show' instance.
data RelayRuntime = RelayRuntime
  { runtimeSettings :: RelaySettings
  , runtimeKey :: BS.ByteString
  , runtimeManager :: Manager
  , runtimeUpstream :: Request
  , runtimeTarget :: BS.ByteString
  , runtimeLogger :: String -> IO ()
  }

newRelayRuntime :: RelaySettings -> BS.ByteString -> IO RelayRuntime
newRelayRuntime settings key = do
  manager <- newManager tlsManagerSettings
  upstream <- parseRequest (relayUpstream settings)
  pure
    RelayRuntime
      { runtimeSettings = settings
      , runtimeKey = key
      , runtimeManager = manager
      , runtimeUpstream = upstream
      , runtimeTarget = path upstream <> queryString upstream
      , runtimeLogger = \message -> hPutStrLn stderr ("hutian-llm-relay: " ++ message)
      }

-- | Replace the log sink. Tests use this to assert that expected client
-- disconnects are not reported as upstream failures.
setRelayLogger :: (String -> IO ()) -> RelayRuntime -> RelayRuntime
setRelayLogger logger runtime = runtime {runtimeLogger = logger}

-- | Bind the Unix socket and serve until the process is stopped.
runRelay :: RelayRuntime -> IO ()
runRelay runtime =
  bracket (listenUnix path) release $ \listener ->
    forever $ do
      (connection, _) <- accept listener
      _ <- forkFinally (handleRelayConnection runtime connection) (\_ -> close connection)
      pure ()
  where
    path = relayListenPath (runtimeSettings runtime)
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
  result <- try (serve runtime connection) :: IO (Either SomeException ())
  case result of
    Left exception ->
      logRelay runtime ("connection failed: " ++ displayException exception)
    Right () -> pure ()

serve :: RelayRuntime -> Socket -> IO ()
serve runtime connection = do
  headResult <- readRequestHead connection
  case headResult of
    Left relayError -> sendError connection relayError
    Right (rawHead, leftover) ->
      case parseRequestHead rawHead of
        Left message -> sendError connection (badRequest message)
        Right request ->
          case validateRequest runtime request of
            Left relayError -> sendError connection relayError
            Right bodyLength -> do
              bodyResult <- readExactly connection bodyLength leftover
              case bodyResult of
                Left relayError -> sendError connection relayError
                Right body -> forward runtime connection body

-- | Enforce the narrow relay contract before touching the upstream.
validateRequest
  :: RelayRuntime
  -> RequestHead
  -> Either RelayError Int
validateRequest runtime request
  | headMethod request /= "POST" =
      Left (RelayError 405 "Method Not Allowed" "only POST is allowed")
  | headVersion request /= "HTTP/1.1" =
      Left (RelayError 505 "HTTP Version Not Supported" "only HTTP/1.1 is allowed")
  | headTarget request /= runtimeTarget runtime =
      Left (RelayError 403 "Forbidden" "unexpected request target")
  | hasTransferEncoding =
      Left (badRequest "Transfer-Encoding is not supported")
  | otherwise =
      case contentLengths of
        [] -> Left (badRequest "Content-Length is required")
        [contentLength] ->
          case parseContentLength contentLength of
            Nothing -> Left (badRequest "invalid Content-Length")
            Just bodyLength
              | bodyLength > relayMaxBody (runtimeSettings runtime) ->
                  Left (RelayError 413 "Payload Too Large" "request body is too large")
              | otherwise -> Right bodyLength
        _ -> Left (badRequest "duplicate Content-Length")
  where
    hasTransferEncoding =
      any
        (\(name, value) -> name == "transfer-encoding" && not (BS.null (trimKey value)))
        (headHeaders request)
    contentLengths =
      [ value
      | (name, value) <- headHeaders request
      , name == "content-length"
      ]
    parseContentLength value
      | BS.null value = Nothing
      | BS.all isAsciiDigit value =
          case BS8.readInteger value of
            Just (number, rest)
              | BS.null rest && number >= 0 && number <= fromIntegral (maxBound :: Int) ->
                  Just (fromIntegral number)
            _ -> Nothing
      | otherwise = Nothing

isAsciiDigit :: Word8 -> Bool
isAsciiDigit byte = byte >= 48 && byte <= 57

forward :: RelayRuntime -> Socket -> BS.ByteString -> IO ()
forward runtime connection body = do
  sent <- newIORef False
  let request =
        (runtimeUpstream runtime)
          { method = "POST"
          , requestHeaders =
              [ ("Content-Type", "application/json")
              , ("Accept", "text/event-stream")
              , ("Authorization", "Bearer " <> runtimeKey runtime)
              ]
          , requestBody = RequestBodyLBS (BL.fromStrict body)
          , responseTimeout = responseTimeoutNone
          , redirectCount = 0
          , checkResponse = \_ _ -> pure ()
          }
  result <-
    try
      ( withResponse request (runtimeManager runtime) $ \response -> do
          let code = statusCode (responseStatus response)
              reason = statusMessage (responseStatus response)
          if code >= 200 && code < 300
            then do
              writeIORef sent True
              connected <- sendToClient connection (streamingResponseHead code reason)
              when connected (streamBody connection (brRead (responseBody response)))
            else do
              errorBody <-
                readBodyLimited
                  (brRead (responseBody response))
                  (relayMaxBody (runtimeSettings runtime))
              writeIORef sent True
              void $ sendToClient connection (fullResponse code reason "application/json" errorBody)
      )
  case result of
    Left (exception :: SomeException) -> do
      alreadySent <- readIORef sent
      unless alreadySent $
        sendError connection (RelayError 502 "Bad Gateway" "upstream request failed")
      logRelay runtime ("upstream request failed: " ++ displayException exception)
    Right () -> pure ()

-- * Wire IO

readRequestHead :: Socket -> IO (Either RelayError (BS.ByteString, BS.ByteString))
readRequestHead connection =
  either (Left . wireError) Right
    <$> recvUntil connection "\r\n\r\n" maxRequestHead
  where
    wireError WireTooLarge = badRequest "request head is too large"
    wireError WireClosed = badRequest "incomplete request"

readExactly :: Socket -> Int -> BS.ByteString -> IO (Either RelayError BS.ByteString)
readExactly connection expected leftover =
  either (const (Left (badRequest "request body ended early"))) Right
    <$> recvExactly connection expected leftover

readBodyLimited :: IO BS.ByteString -> Int -> IO BS.ByteString
readBodyLimited readChunk limit = go BS.empty 0
  where
    go accumulated total = do
      chunk <- readChunk
      if BS.null chunk
        then pure accumulated
        else do
          let remaining = limit - total
          if BS.length chunk >= remaining
            then pure (accumulated <> BS.take remaining chunk)
            else go (accumulated <> chunk) (total + BS.length chunk)

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

sendError :: Socket -> RelayError -> IO ()
sendError connection relayError =
  void $
    sendToClient
      connection
      ( fullResponse
          (relayErrorCode relayError)
          (relayErrorReason relayError)
          "text/plain; charset=utf-8"
          (BS8.pack (relayErrorMessage relayError))
      )

fullResponse :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
fullResponse code reason contentType body =
  BS.concat
    [ statusLine code reason
    , "Content-Type: "
    , contentType
    , "\r\nContent-Length: "
    , BS8.pack (show (BS.length body))
    , "\r\nConnection: close\r\n\r\n"
    , body
    ]

streamingResponseHead :: Int -> BS.ByteString -> BS.ByteString
streamingResponseHead code reason =
  BS.concat
    [ statusLine code reason
    , "Content-Type: text/event-stream\r\n"
    , "Cache-Control: no-cache\r\n"
    , "Connection: close\r\n\r\n"
    ]

statusLine :: Int -> BS.ByteString -> BS.ByteString
statusLine code reason =
  "HTTP/1.1 " <> BS8.pack (show code) <> " " <> reason <> "\r\n"

-- * Logging

logRelay :: RelayRuntime -> String -> IO ()
logRelay runtime message =
  runtimeLogger runtime (redactKey (runtimeKey runtime) message)
