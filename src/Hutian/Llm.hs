{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Streaming HTTP client for llama.cpp. Settings, request bodies, and the
-- stream reducer are pure and live in their own modules; this module only
-- performs network and terminal IO.
module Hutian.Llm
  ( module Hutian.Llm.Request
  , module Hutian.Llm.Settings
  , module Hutian.Llm.Stream
  , LlmOutput (..)
  , loadLlmLaunch
  , runLlm
  , runLlmWithTools
  , runLlmContinuation
  ) where

import Control.Concurrent (forkIOWithUnmask, killThread)
import Control.Concurrent.STM
  ( atomically
  , newEmptyTMVarIO
  , orElse
  , putTMVar
  , takeTMVar
  )
import Control.Exception
  ( SomeException
  , displayException
  , evaluate
  , finally
  , mask
  , onException
  , try
  )
import Control.Monad (unless)
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Hutian.Conversation (Conversation)
import Hutian.Http.Body (readBodyAtMost)
import Hutian.Http.Error (trySync)
import Hutian.Interrupt (InterruptFlag, interruptPending, isInterrupted)
import Hutian.Llm.Presentation
  ( StreamPresenter
  , finishPresentation
  , initialStreamPresenterIn
  , presentStreamEvent
  )
import Hutian.Llm.Request
import Hutian.Llm.Settings
import Hutian.Llm.Stream
import Hutian.Llm.UnixSocket (postUnixStream)
import Hutian.Render (TurnContext)
import Hutian.Transcript
  ( Transcript
  , TranscriptEvent (..)
  , recordTranscriptMaybe
  )
import Hutian.Verbosity (Verbosity)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.Directory (removeFile)

-- | Where streamed output and diagnostics go. The caller owns the terminal and
-- can flush or capture each write. The context and verbosity select the palette
-- and how much reasoning is shown.
data LlmOutput = LlmOutput
  { llmOutputWrite :: String -> IO ()
  , llmOutputErrorLine :: String -> IO ()
  , llmOutputContext :: TurnContext
  , llmOutputVerbosity :: Verbosity
  -- | Structured recording is independent of what the terminal displays.
  , llmOutputTranscript :: Maybe Transcript
  }

-- | Load settings and the positional prompt arguments. File-backed model and
-- system-prompt values are read and removed before parsing succeeds, so they
-- are gone before the shell can run a command.
loadLlmLaunch :: [String] -> IO (Either String (LlmSettings, [String]))
loadLlmLaunch = loadLlmWith parseLlmLaunchWithFiles

loadLlmWith
  :: ([(FilePath, String)] -> [String] -> Either String a)
  -> [String]
  -> IO (Either String a)
loadLlmWith parse arguments =
  case llmFilePaths arguments of
    Left message -> pure (Left message)
    Right paths -> do
      loaded <- mapM readAndRemove paths
      pure $ do
        files <- sequence loaded
        parse files arguments
  where
    readAndRemove path = do
      contents <- tryReadText path
      removed <- try (removeFile path) :: IO (Either SomeException ())
      pure $ case (contents, removed) of
        (Left exception, _) ->
          Left ("could not read " ++ path ++ ": " ++ displayException exception)
        (Right _, Left exception) ->
          Left ("could not remove " ++ path ++ ": " ++ displayException exception)
        (Right text, Right ()) -> Right (path, Text.unpack text)

    tryReadText :: FilePath -> IO (Either SomeException Text.Text)
    tryReadText path =
      try $ do
        text <- TextIO.readFile path
        -- Force the lazy read so a mid-file error is reported here and no file
        -- handle stays open after the file is removed.
        _ <- evaluate (Text.length text)
        pure text

runLlm :: LlmOutput -> InterruptFlag -> LlmSettings -> Conversation -> String -> IO (Maybe String)
runLlm output interruptFlag settings conversation prompt =
  llmResponseText <$> runLlmWithTools output interruptFlag settings conversation prompt

runLlmWithTools :: LlmOutput -> InterruptFlag -> LlmSettings -> Conversation -> String -> IO LlmResult
runLlmWithTools output interruptFlag settings conversation prompt
  | all isSpace prompt = pure emptyLlmResult
  | otherwise =
      runRequest output interruptFlag settings $
        requestBodyFor settings conversation prompt

-- | Continue a conversation after tool results have been appended.
runLlmContinuation :: LlmOutput -> InterruptFlag -> LlmSettings -> Conversation -> IO LlmResult
runLlmContinuation output interruptFlag settings conversation =
  runRequest output interruptFlag settings $
    continuationBodyFor settings conversation

runRequest :: LlmOutput -> InterruptFlag -> LlmSettings -> Value -> IO LlmResult
runRequest output interruptFlag settings body = do
  interrupted <- isInterrupted interruptFlag
  if interrupted
    then do
      recordInterrupted output
      pure emptyLlmResult
    else do
      result <-
        raceInterrupt
          interruptFlag
          (streamCompletion output interruptFlag settings body)
      case result of
        Nothing -> do
          recordInterrupted output
          pure emptyLlmResult
        Just (Left message) -> do
          llmOutputErrorLine output ("llm: " ++ message)
          pure emptyLlmResult
        Just (Right response) -> pure response
  where
    recordInterrupted current =
      recordTranscriptMaybe
        (llmOutputTranscript current)
        (TranscriptInterrupted (llmOutputContext current))

-- | Run a request in a worker thread and stop waiting as soon as the interrupt
-- flag is set. Killing the worker unblocks a body read that is waiting on the
-- network, and its bracket closes the connection. 'Nothing' means interrupted.
raceInterrupt :: InterruptFlag -> IO a -> IO (Maybe (Either String a))
raceInterrupt interruptFlag action = mask $ \restore -> do
  outcome <- newEmptyTMVarIO
  finished <- newEmptyTMVarIO
  worker <-
    forkIOWithUnmask $ \unmask ->
      (unmask (trySync action) >>= atomically . putTMVar outcome)
        `finally` atomically (putTMVar finished ())
  let wait =
        atomically $
          (Just <$> takeTMVar outcome)
            `orElse` (interruptPending interruptFlag >> pure Nothing)
            `orElse` (takeTMVar finished >> pure (Just (Left terminatedUnexpectedly)))
  result <- restore wait `onException` killThread worker
  case result of
    Just value -> pure (Just value)
    Nothing -> do
      killThread worker
      atomically (takeTMVar finished)
      pure Nothing
  where
    terminatedUnexpectedly = "request thread terminated unexpectedly"

streamCompletion :: LlmOutput -> InterruptFlag -> LlmSettings -> Value -> IO LlmResult
streamCompletion output interruptFlag settings body =
  case settingSocket settings of
    Just socket ->
      streamCompletionUnix output interruptFlag settings socket (BL.toStrict (Aeson.encode body))
    Nothing -> streamCompletionTcp output interruptFlag settings body

streamCompletionTcp :: LlmOutput -> InterruptFlag -> LlmSettings -> Value -> IO LlmResult
streamCompletionTcp output interruptFlag settings body = do
  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest (unLlmUrl (settingUrl settings))
  let request =
        initialRequest
          { method = "POST"
          , requestHeaders = [("Content-Type", "application/json")]
          , requestBody = RequestBodyLBS (Aeson.encode body)
          , responseTimeout = responseTimeoutNone
          , redirectCount = 0
          , checkResponse = \_ _ -> pure ()
          }
  withResponse request manager $ \response ->
    handleStreamResponse
      output
      interruptFlag
      (statusCode (responseStatus response))
      (brRead (responseBody response))

-- | Send the request through the host relay's Unix socket. The only bytes that
-- cross into the sandbox are the request body and the relay host's name; the
-- API key stays on the host.
streamCompletionUnix
  :: LlmOutput
  -> InterruptFlag
  -> LlmSettings
  -> FilePath
  -> BS.ByteString
  -> IO LlmResult
streamCompletionUnix output interruptFlag _settings socket body =
  postUnixStream socket body $ \code reader ->
    handleStreamResponse output interruptFlag code reader

handleStreamResponse
  :: LlmOutput
  -> InterruptFlag
  -> Int
  -> IO BS.ByteString
  -> IO LlmResult
handleStreamResponse output interruptFlag code reader
  | code < 200 || code >= 300 = do
      responseBody <- readBodyAtMost errorBodyLimit reader
      let message = Text.unpack (Text.strip (decode responseBody))
      llmOutputErrorLine output ("llm: llama.cpp returned HTTP " ++ show code ++ errorSuffix message)
      pure emptyLlmResult
  | otherwise = do
      (endState, endPresenter) <-
        consumeResponse
          output
          interruptFlag
          reader
          BS.empty
          initialStreamState
          (initialStreamPresenterIn (llmOutputVerbosity output) (llmOutputContext output))
      let (_, finalOutput) = finishPresentation endPresenter
      emitText output finalOutput
      interrupted <- isInterrupted interruptFlag
      unless (interrupted || streamSawData endState) $
        llmOutputErrorLine output "llm: no response content received"
      pure (resultUnlessInterrupted interrupted endState)

consumeResponse
  :: LlmOutput
  -> InterruptFlag
  -> IO BS.ByteString
  -> BS.ByteString
  -> StreamState
  -> StreamPresenter
  -> IO (StreamState, StreamPresenter)
consumeResponse output interruptFlag reader buffered state presenter = do
  interrupted <- isInterrupted interruptFlag
  if interrupted
    then pure (state, presenter)
    else do
      chunk <- reader
      if BS.null chunk
        then do
          let (lineChunks, remainder) = splitLines buffered
              finalChunks =
                if BS.null remainder
                  then lineChunks
                  else lineChunks ++ [remainder]
          processLines output finalChunks state presenter
        else do
          let (lineChunks, rest) = splitLines (buffered <> chunk)
          next <- processLines output lineChunks state presenter
          if streamDone (fst next)
            then pure next
            else
              uncurry
                (consumeResponse output interruptFlag reader rest)
                next

processLines
  :: LlmOutput
  -> [BS.ByteString]
  -> StreamState
  -> StreamPresenter
  -> IO (StreamState, StreamPresenter)
processLines _ [] state presenter = pure (state, presenter)
processLines output (line : rest) state presenter
  | streamDone state = pure (state, presenter)
  | otherwise =
      processLine output line state presenter
        >>= uncurry (processLines output rest)

processLine
  :: LlmOutput
  -> BS.ByteString
  -> StreamState
  -> StreamPresenter
  -> IO (StreamState, StreamPresenter)
processLine output rawLine state presenter =
  case parseStreamLine rawLine of
    Nothing -> pure (state, presenter)
    Just event -> do
      recordReasoning output event
      let (presenter', text) = presentStreamEvent event presenter
      emitText output text
      maybe
        (pure ())
        (llmOutputErrorLine output . ("llm: " ++) . Text.unpack)
        (eventError event)
      pure (applyStreamEvent event state, presenter')
  where
    recordReasoning current streamEvent =
      case eventReasoning streamEvent of
        Just text
          | not (Text.null text) ->
              recordTranscriptMaybe
                (llmOutputTranscript current)
                ( TranscriptReasoning
                    (llmOutputContext current)
                    (Text.unpack text)
                )
        _ -> pure ()

emitText :: LlmOutput -> String -> IO ()
emitText output text =
  unless (null text) (llmOutputWrite output text)

-- | Error responses are diagnostics, not data. Read a bounded prefix so a
-- misbehaving upstream cannot make the shell buffer an arbitrary body.
errorBodyLimit :: Int
errorBodyLimit = 64 * 1024

errorSuffix :: String -> String
errorSuffix "" = ""
errorSuffix message = ": " ++ message
