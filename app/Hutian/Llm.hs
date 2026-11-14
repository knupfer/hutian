{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Streaming HTTP client for llama.cpp. Protocol parsing and the stream
-- reducer are pure and live in "Hutian.Llm.Protocol"; this module only performs
-- network and terminal IO.
module Hutian.Llm
  ( module Hutian.Llm.Protocol
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
import Hutian.Console (consoleFlush, consoleWrite, consoleWriteErrorLine)
import Hutian.Conversation (Conversation)
import Hutian.Interrupt (InterruptFlag, isInterrupted)
import Hutian.Llm.Protocol
import Hutian.Llm.UnixSocket (postUnixStream)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.Directory (removeFile)

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

runLlm :: InterruptFlag -> LlmSettings -> Conversation -> String -> IO (Maybe String)
runLlm interruptFlag settings conversation prompt =
  llmResponseText <$> runLlmWithTools interruptFlag settings conversation prompt

runLlmWithTools :: InterruptFlag -> LlmSettings -> Conversation -> String -> IO LlmResult
runLlmWithTools interruptFlag settings conversation prompt
  | all isSpace prompt = pure emptyLlmResult
  | otherwise =
      runRequest interruptFlag settings $
        requestBodyFor
          (settingModelShell settings)
          (apiFor (settingUrl settings))
          (settingModel settings)
          (settingSystemPrompt settings)
          conversation
          prompt

-- | Continue a conversation after tool results have been appended.
runLlmContinuation :: InterruptFlag -> LlmSettings -> Conversation -> IO LlmResult
runLlmContinuation interruptFlag settings conversation =
  runRequest interruptFlag settings $
    continuationBodyFor
      (settingModelShell settings)
      (apiFor (settingUrl settings))
      (settingModel settings)
      (settingSystemPrompt settings)
      conversation

runRequest :: InterruptFlag -> LlmSettings -> Value -> IO LlmResult
runRequest interruptFlag settings body = do
  result <-
    try
      (streamCompletion interruptFlag settings body)
      :: IO (Either SomeException LlmResult)
  case result of
    Left exception -> do
      consoleWriteErrorLine ("llm: " ++ show exception)
      pure emptyLlmResult
    Right response -> pure response

streamCompletion :: InterruptFlag -> LlmSettings -> Value -> IO LlmResult
streamCompletion interruptFlag settings body =
  case settingSocket settings of
    Just socket ->
      streamCompletionUnix interruptFlag settings socket (BL.toStrict (Aeson.encode body))
    Nothing -> streamCompletionTcp interruptFlag settings body

streamCompletionTcp :: InterruptFlag -> LlmSettings -> Value -> IO LlmResult
streamCompletionTcp interruptFlag settings body = do
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
      interruptFlag
      (statusCode (responseStatus response))
      (brRead (responseBody response))

-- | Send the request through the host relay's Unix socket. The only bytes that
-- cross into the sandbox are the request body and the relay host's name; the
-- API key stays on the host.
streamCompletionUnix
  :: InterruptFlag
  -> LlmSettings
  -> FilePath
  -> BS.ByteString
  -> IO LlmResult
streamCompletionUnix interruptFlag _settings socket body =
  postUnixStream socket body $ \code reader ->
    handleStreamResponse interruptFlag code reader

handleStreamResponse
  :: InterruptFlag
  -> Int
  -> IO BS.ByteString
  -> IO LlmResult
handleStreamResponse interruptFlag code reader
  | code < 200 || code >= 300 = do
      responseBody <- readBodyAtMost errorBodyLimit reader []
      let message = Text.unpack (Text.strip (decode responseBody))
      consoleWriteErrorLine ("llm: llama.cpp returned HTTP " ++ show code ++ errorSuffix message)
      pure emptyLlmResult
  | otherwise = do
      finalState <-
        consumeResponse
          interruptFlag
          reader
          BS.empty
          initialStreamState
      let (endState, finalOutput) = finishStream finalState
      _ <- emitStreamOutput (endState, finalOutput)
      unless (streamSawData endState) $
        consoleWriteErrorLine "llm: no response content received"
      pure (resultFromState endState)

consumeResponse
  :: InterruptFlag
  -> IO BS.ByteString
  -> BS.ByteString
  -> StreamState
  -> IO StreamState
consumeResponse interruptFlag reader buffered state = do
  interrupted <- isInterrupted interruptFlag
  if interrupted
    then pure state
    else do
      chunk <- reader
      if BS.null chunk
        then processLines (fst (splitLines buffered)) state
        else do
          let (lineChunks, rest) = splitLines (buffered <> chunk)
          state' <- processLines lineChunks state
          if streamDone state'
            then pure state'
            else consumeResponse interruptFlag reader rest state'

processLines :: [BS.ByteString] -> StreamState -> IO StreamState
processLines [] state = pure state
processLines (line : rest) state
  | streamDone state = pure state
  | otherwise = processLine line state >>= processLines rest

processLine :: BS.ByteString -> StreamState -> IO StreamState
processLine rawLine state =
  case parseStreamLine rawLine of
    Nothing -> pure state
    Just event -> emitStreamOutput (applyStreamEvent event state)

emitStreamOutput :: (StreamState, StreamOutput) -> IO StreamState
emitStreamOutput (state, output) = do
  emit (outputText output)
  maybe (pure ()) (consoleWriteErrorLine . ("llm: " ++) . Text.unpack) (outputError output)
  pure state

emit :: String -> IO ()
emit output =
  unless (null output) $ do
    consoleWrite output
    consoleFlush

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
