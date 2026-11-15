{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Streaming HTTP client for llama.cpp. Protocol parsing and the stream
-- reducer are pure and live in "Hutian.Llm.Protocol"; this module only performs
-- network and terminal IO.
module Hutian.Llm
  ( module Hutian.Llm.Protocol
  , LlmOutput (..)
  , loadLlmSettings
  , runLlm
  , runLlmWithTools
  , runLlmContinuation
  ) where

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (unless)
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Hutian.Conversation (Conversation)
import Hutian.Interrupt (InterruptFlag, isInterrupted)
import Hutian.Llm.Protocol
import Hutian.Llm.UnixSocket (postUnixStream)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.Directory (removeFile)

-- | Where streamed output and diagnostics go. The caller owns the terminal and
-- can flush or capture each write.
data LlmOutput = LlmOutput
  { llmOutputWrite :: String -> IO ()
  , llmOutputErrorLine :: String -> IO ()
  }

-- | Load settings, including file-backed model and system-prompt values. The
-- files are removed before parsing succeeds so they are gone before the shell
-- can run a command.
loadLlmSettings :: [String] -> IO (Either String LlmSettings)
loadLlmSettings arguments =
  case llmFilePaths arguments of
    Left message -> pure (Left message)
    Right paths -> do
      loaded <- mapM readAndRemove paths
      pure $ do
        files <- sequence loaded
        parseLlmSettingsWithFiles files arguments
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
        requestBodyFor
          (settingModelShell settings)
          (apiFor (settingUrl settings))
          (settingModel settings)
          (settingSystemPrompt settings)
          conversation
          prompt

-- | Continue a conversation after tool results have been appended.
runLlmContinuation :: LlmOutput -> InterruptFlag -> LlmSettings -> Conversation -> IO LlmResult
runLlmContinuation output interruptFlag settings conversation =
  runRequest output interruptFlag settings $
    continuationBodyFor
      (settingModelShell settings)
      (apiFor (settingUrl settings))
      (settingModel settings)
      (settingSystemPrompt settings)
      conversation

runRequest :: LlmOutput -> InterruptFlag -> LlmSettings -> Value -> IO LlmResult
runRequest output interruptFlag settings body = do
  result <-
    try
      (streamCompletion output interruptFlag settings body)
      :: IO (Either SomeException LlmResult)
  case result of
    Left exception -> do
      llmOutputErrorLine output ("llm: " ++ show exception)
      pure emptyLlmResult
    Right response -> pure response

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
      responseBody <- readBodyAtMost errorBodyLimit reader []
      let message = Text.unpack (Text.strip (decode responseBody))
      llmOutputErrorLine output ("llm: llama.cpp returned HTTP " ++ show code ++ errorSuffix message)
      pure emptyLlmResult
  | otherwise = do
      finalState <-
        consumeResponse
          output
          interruptFlag
          reader
          BS.empty
          initialStreamState
      let (endState, finalOutput) = finishStream finalState
      _ <- emitStreamOutput output (endState, finalOutput)
      unless (streamSawData endState) $
        llmOutputErrorLine output "llm: no response content received"
      pure (resultFromState endState)

consumeResponse
  :: LlmOutput
  -> InterruptFlag
  -> IO BS.ByteString
  -> BS.ByteString
  -> StreamState
  -> IO StreamState
consumeResponse output interruptFlag reader buffered state = do
  interrupted <- isInterrupted interruptFlag
  if interrupted
    then pure state
    else do
      chunk <- reader
      if BS.null chunk
        then processLines output (fst (splitLines buffered)) state
        else do
          let (lineChunks, rest) = splitLines (buffered <> chunk)
          state' <- processLines output lineChunks state
          if streamDone state'
            then pure state'
            else consumeResponse output interruptFlag reader rest state'

processLines :: LlmOutput -> [BS.ByteString] -> StreamState -> IO StreamState
processLines _ [] state = pure state
processLines output (line : rest) state
  | streamDone state = pure state
  | otherwise = processLine output line state >>= processLines output rest

processLine :: LlmOutput -> BS.ByteString -> StreamState -> IO StreamState
processLine output rawLine state =
  case parseStreamLine rawLine of
    Nothing -> pure state
    Just event -> emitStreamOutput output (applyStreamEvent event state)

emitStreamOutput :: LlmOutput -> (StreamState, StreamOutput) -> IO StreamState
emitStreamOutput llmOutput (state, streamOutput) = do
  emit (outputText streamOutput)
  maybe (pure ()) (llmOutputErrorLine llmOutput . ("llm: " ++) . Text.unpack) (outputError streamOutput)
  pure state
  where
    emit text = unless (null text) (llmOutputWrite llmOutput text)

-- | Error responses are diagnostics, not data. Read a bounded prefix so a
-- misbehaving upstream cannot make the shell buffer an arbitrary body.
errorBodyLimit :: Int
errorBodyLimit = 64 * 1024

readBodyAtMost :: Int -> IO BS.ByteString -> [BS.ByteString] -> IO BS.ByteString
readBodyAtMost limit reader = go 0
  where
    go total collected
      | total >= limit = pure (BS.concat (reverse collected))
      | otherwise = do
          chunk <- reader
          if BS.null chunk
            then pure (BS.concat (reverse collected))
            else do
              let kept = BS.take (limit - total) chunk
              go (total + BS.length kept) (kept : collected)

errorSuffix :: String -> String
errorSuffix "" = ""
errorSuffix message = ": " ++ message
