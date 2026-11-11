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
import Data.Char (isSpace)
import qualified Data.Text as Text
import Hutian.Conversation (Conversation)
import Hutian.Interrupt (InterruptFlag, isInterrupted)
import Hutian.Llm.Protocol
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import System.IO (hFlush, hPutStrLn, stderr, stdout)

runLlm :: InterruptFlag -> LlmSettings -> Conversation -> String -> IO (Maybe String)
runLlm interruptFlag settings conversation prompt =
  llmResponseText <$> runLlmWithTools interruptFlag settings conversation prompt

runLlmWithTools :: InterruptFlag -> LlmSettings -> Conversation -> String -> IO LlmResult
runLlmWithTools interruptFlag settings conversation prompt
  | all isSpace prompt = pure emptyLlmResult
  | otherwise =
      runRequest interruptFlag settings $
        requestBodyFor
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
      (apiFor (settingUrl settings))
      (settingModel settings)
      (settingSystemPrompt settings)
      conversation

runRequest :: InterruptFlag -> LlmSettings -> Value -> IO LlmResult
runRequest interruptFlag settings body = do
  result <-
    try
      (streamCompletion interruptFlag (settingUrl settings) body)
      :: IO (Either SomeException LlmResult)
  case result of
    Left exception -> do
      hPutStrLn stderr ("llm: " ++ show exception)
      pure emptyLlmResult
    Right response -> pure response

streamCompletion :: InterruptFlag -> LlmUrl -> Value -> IO LlmResult
streamCompletion interruptFlag url body = do
  manager <- newManager defaultManagerSettings
  initialRequest <- parseRequest (unLlmUrl url)
  let request =
        initialRequest
          { method = "POST"
          , requestHeaders = [("Content-Type", "application/json")]
          , requestBody = RequestBodyLBS (Aeson.encode body)
          , responseTimeout = responseTimeoutNone
          }
  withResponse request manager $ \response -> do
    let code = statusCode (responseStatus response)
    if code < 200 || code >= 300
      then do
        responseBody <- readBody (Network.HTTP.Client.responseBody response) []
        let message = Text.unpack (Text.strip (decode responseBody))
        hPutStrLn stderr ("llm: llama.cpp returned HTTP " ++ show code ++ errorSuffix message)
        pure emptyLlmResult
      else do
        finalState <-
          consumeResponse
            interruptFlag
            (Network.HTTP.Client.responseBody response)
            BS.empty
            initialStreamState
        let (endState, finalOutput) = finishStream finalState
        _ <- emitStreamOutput (endState, finalOutput)
        unless (streamSawData endState) $
          hPutStrLn stderr "llm: no response content received"
        pure (resultFromState endState)

consumeResponse
  :: InterruptFlag
  -> BodyReader
  -> BS.ByteString
  -> StreamState
  -> IO StreamState
consumeResponse interruptFlag reader buffered state = do
  interrupted <- isInterrupted interruptFlag
  if interrupted
    then pure state
    else do
      chunk <- brRead reader
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
  maybe (pure ()) (hPutStrLn stderr . ("llm: " ++) . Text.unpack) (outputError output)
  pure state

emit :: String -> IO ()
emit output =
  unless (null output) $ do
    putStr output
    hFlush stdout

readBody :: BodyReader -> [BS.ByteString] -> IO BS.ByteString
readBody reader chunks = do
  chunk <- brRead reader
  if BS.null chunk
    then pure (BS.concat (reverse chunks))
    else readBody reader (chunk : chunks)

errorSuffix :: String -> String
errorSuffix "" = ""
errorSuffix message = ": " ++ message
