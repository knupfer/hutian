{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Streaming HTTP client for llama.cpp. Protocol parsing and the stream
-- reducer are pure and live in "Hutian.Llm.Protocol"; this module only performs
-- network and terminal IO.
module Hutian.Llm
  ( module Hutian.Llm.Protocol
  , runLlm
  , runLlmWithTools
  , runLlmContinuation
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace)
import qualified Data.Text as Text
import Hutian.Console (consoleFlush, consoleWrite, consoleWriteErrorLine)
import Hutian.Conversation (Conversation)
import Hutian.Interrupt (InterruptFlag, isInterrupted)
import Hutian.Llm.Protocol
import Hutian.Llm.UnixSocket (postUnixStream)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

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
      responseBody <- readBody reader []
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

readBody :: IO BS.ByteString -> [BS.ByteString] -> IO BS.ByteString
readBody reader chunks = do
  chunk <- reader
  if BS.null chunk
    then pure (BS.concat (reverse chunks))
    else readBody reader (chunk : chunks)

errorSuffix :: String -> String
errorSuffix "" = ""
errorSuffix message = ": " ++ message
