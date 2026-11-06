{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Minimal streaming client for llama.cpp.
module Hutian.Llm
  ( runLlm
  ) where

import Control.Exception (SomeException, fromException, throwIO, try)
import Control.Monad (unless, when)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncodingError
import qualified Data.Vector as Vector
import Data.Word (Word8)
import Hutian.Interrupt (InterruptFlag, isInterrupted, withInterruptFlag)
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import System.Console.Haskeline (Interrupt)
import System.Environment (lookupEnv)
import System.IO (hFlush, hPutStrLn, stderr, stdout)

runLlm :: InterruptFlag -> String -> IO ()
runLlm interruptFlag prompt
  | all isSpace prompt = pure ()
  | otherwise = do
      url <- fromMaybe defaultEndpoint <$> lookupEnv "HUTIAN_LLAMA_URL"
      model <- fromMaybe "default" <$> lookupEnv "HUTIAN_LLAMA_MODEL"
      result <-
        try (withInterruptFlag interruptFlag (streamCompletion interruptFlag url model prompt))
          :: IO (Either SomeException ())
      case result of
        Left exception ->
          case fromException exception :: Maybe Interrupt of
            Just _ -> throwIO exception
            Nothing -> hPutStrLn stderr ("llm: " ++ show exception)
        Right () -> pure ()

defaultEndpoint :: String
defaultEndpoint = "http://127.0.0.1:8080/v1/chat/completions"

data Api
  = ChatApi
  | CompletionApi

apiFor :: String -> Api
apiFor url
  | "/v1/chat/completions" `isInfixOf` url = ChatApi
  | otherwise = CompletionApi

requestBodyFor :: Api -> String -> String -> Aeson.Value
requestBodyFor api model prompt =
  case api of
    ChatApi ->
      object
        [ "model" .= model
        , "messages" .= [object ["role" .= ("user" :: String), "content" .= prompt]]
        , "stream" .= True
        ]
    CompletionApi ->
      object
        [ "model" .= model
        , "prompt" .= prompt
        , "stream" .= True
        ]

streamCompletion :: InterruptFlag -> String -> String -> String -> IO ()
streamCompletion interruptFlag url model prompt = do
  manager <- newManager defaultManagerSettings
  initialRequest <- parseRequest url
  let request =
        initialRequest
          { method = "POST"
          , requestHeaders = [("Content-Type", "application/json")]
          , requestBody = RequestBodyLBS (Aeson.encode (requestBodyFor (apiFor url) model prompt))
          , responseTimeout = responseTimeoutNone
          }
  withResponse request manager $ \response -> do
    let code = statusCode (responseStatus response)
    if code < 200 || code >= 300
      then do
        body <- readBody (responseBody response) []
        let message = Text.unpack (Text.strip (decode body))
        hPutStrLn stderr ("llm: llama.cpp returned HTTP " ++ show code ++ errorSuffix message)
      else do
        finalState <-
          consumeResponse interruptFlag (responseBody response) BS.empty initialState
        when (streamPrinted finalState) (putStrLn "")
        unless (streamSawData finalState) $
          hPutStrLn stderr "llm: no response content received"

data StreamState = StreamState
  { streamPrinted :: Bool
  , streamSawData :: Bool
  , streamDone :: Bool
  }

initialState :: StreamState
initialState = StreamState
  { streamPrinted = False
  , streamSawData = False
  , streamDone = False
  }

readBody :: BodyReader -> [BS.ByteString] -> IO BS.ByteString
readBody reader chunks = do
  chunk <- brRead reader
  if BS.null chunk
    then pure (BS.concat (reverse chunks))
    else readBody reader (chunk : chunks)

consumeResponse :: InterruptFlag -> BodyReader -> BS.ByteString -> StreamState -> IO StreamState
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

splitLines :: BS.ByteString -> ([BS.ByteString], BS.ByteString)
splitLines = go []
  where
    go acc input =
      case BS.elemIndex 10 input of
        Nothing -> (reverse acc, input)
        Just index ->
          let (line, rest) = BS.splitAt index input
           in go (line : acc) (BS.drop 1 rest)

processLines :: [BS.ByteString] -> StreamState -> IO StreamState
processLines [] state = pure state
processLines (line : lines') state
  | streamDone state = pure state
  | otherwise = do
      state' <- processLine line state
      processLines lines' state'

processLine :: BS.ByteString -> StreamState -> IO StreamState
processLine rawLine state
  | BS.null line = pure state
  | "data:" `BS.isPrefixOf` line = do
      let payload = Text.strip (decode (BS.drop 5 line))
      if payload == "[DONE]"
        then pure state { streamSawData = True, streamDone = True }
        else processJson payload state { streamSawData = True }
  | otherwise = processJson (Text.strip (decode line)) state
  where
    line = BS.dropWhileEnd isCarriageReturn rawLine

isCarriageReturn :: Word8 -> Bool
isCarriageReturn = (== 13)

processJson :: Text.Text -> StreamState -> IO StreamState
processJson payload state =
  case Aeson.decodeStrict' (TextEncoding.encodeUtf8 payload) of
    Just (Object value) -> do
      let content = extractContent value
      case content of
        Just text -> do
          putStr (Text.unpack text)
          hFlush stdout
        Nothing -> pure ()
      case extractError value of
        Just message -> hPutStrLn stderr ("llm: " ++ Text.unpack message)
        Nothing -> pure ()
      let printed = streamPrinted state || maybe False (not . Text.null) content
       in pure
            state
              { streamPrinted = printed
              , streamSawData = True
              , streamDone = streamDone state || isStop value
              }
    _ -> pure state

extractContent :: KeyMap.KeyMap Value -> Maybe Text.Text
extractContent value =
  case KeyMap.lookup "content" value of
    Just (String text) -> Just text
    _ ->
      case KeyMap.lookup "choices" value of
        Just (Array choices) -> listToMaybe (mapMaybe choiceContent (Vector.toList choices))
        _ -> Nothing
  where
    choiceContent (Object choice) =
      case KeyMap.lookup "delta" choice of
        Just (Object delta) ->
          case KeyMap.lookup "content" delta of
            Just (String text) -> Just text
            _ -> Nothing
        _ -> Nothing
    choiceContent _ = Nothing

extractError :: KeyMap.KeyMap Value -> Maybe Text.Text
extractError value =
  case KeyMap.lookup "error" value of
    Just (String message) -> Just message
    Just (Object nested) ->
      case KeyMap.lookup "message" nested of
        Just (String message) -> Just message
        _ -> Just (Text.pack (show (Object nested)))
    _ -> Nothing

isStop :: KeyMap.KeyMap Value -> Bool
isStop value =
  case KeyMap.lookup "stop" value of
    Just (Bool True) -> True
    _ ->
      case KeyMap.lookup "choices" value of
        Just (Array choices) -> any choiceFinished (Vector.toList choices)
        _ -> False
  where
    choiceFinished (Object choice) =
      case KeyMap.lookup "finish_reason" choice of
        Nothing -> False
        Just Null -> False
        Just _ -> True
    choiceFinished _ = False

decode :: BS.ByteString -> Text.Text
decode = TextEncoding.decodeUtf8With TextEncodingError.lenientDecode

errorSuffix :: String -> String
errorSuffix "" = ""
errorSuffix message = ": " ++ message
