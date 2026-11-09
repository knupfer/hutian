{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Minimal streaming client for llama.cpp.
module Hutian.Llm
  ( runLlm
  , Api (..)
  , apiFor
  , requestBodyFor
  , splitLines
  , extractContent
  , extractReasoning
  , extractError
  , isStop
  , decode
  , defaultEndpoint
  ) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Control.Monad (unless)
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
import Hutian.Conversation
  ( ChatMessage (..)
  , Conversation
  , completionPrompt
  , requestMessages
  , roleName
  )
import Hutian.Interrupt (InterruptFlag, isInterrupted, withInterruptFlag)
import Hutian.Markdown
  ( MarkdownState
  , finishMarkdown
  , finishMarkdownWith
  , initialMarkdownState
  , renderMarkdown
  , renderMarkdownWith
  )
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import System.Environment (lookupEnv)
import System.IO (hFlush, hPutStrLn, stderr, stdout)

runLlm :: InterruptFlag -> Conversation -> String -> IO (Maybe String)
runLlm interruptFlag conversation prompt
  | all isSpace prompt = pure Nothing
  | otherwise = do
      url <- fromMaybe defaultEndpoint <$> lookupEnv "HUTIAN_LLAMA_URL"
      model <- fromMaybe "default" <$> lookupEnv "HUTIAN_LLAMA_MODEL"
      systemPrompt <- fromMaybe "" <$> lookupEnv "HUTIAN_SYSTEM_PROMPT"
      result <-
        try
          ( withInterruptFlag interruptFlag $
              streamCompletion interruptFlag url model systemPrompt conversation prompt
          )
          :: IO (Either SomeException (Maybe String))
      case result of
        Left exception -> do
          hPutStrLn stderr ("llm: " ++ show exception)
          pure Nothing
        Right response -> pure response

defaultEndpoint :: String
defaultEndpoint = "http://127.0.0.1:8080/v1/chat/completions"

data Api
  = ChatApi
  | CompletionApi
  deriving stock (Eq, Show)

apiFor :: String -> Api
apiFor url
  | "/v1/chat/completions" `isInfixOf` url = ChatApi
  | otherwise = CompletionApi

requestBodyFor :: Api -> String -> String -> Conversation -> String -> Aeson.Value
requestBodyFor api model systemPrompt conversation prompt =
  case api of
    ChatApi ->
      object
        [ "model" .= model
        , "messages" .= map messageValue (requestMessages systemPrompt conversation prompt)
        , "stream" .= True
        ]
    CompletionApi ->
      object
        [ "model" .= model
        , "prompt" .= completionPrompt systemPrompt conversation prompt
        , "stream" .= True
        ]
  where
    messageValue message =
      object
        [ "role" .= roleName (chatRole message)
        , "content" .= chatContent message
        ]

streamCompletion :: InterruptFlag -> String -> String -> String -> Conversation -> String -> IO (Maybe String)
streamCompletion interruptFlag url model systemPrompt conversation prompt = do
  manager <- newManager defaultManagerSettings
  initialRequest <- parseRequest url
  let request =
        initialRequest
          { method = "POST"
          , requestHeaders = [("Content-Type", "application/json")]
          , requestBody =
              RequestBodyLBS
                (Aeson.encode (requestBodyFor (apiFor url) model systemPrompt conversation prompt))
          , responseTimeout = responseTimeoutNone
          }
  withResponse request manager $ \response -> do
    let code = statusCode (responseStatus response)
    if code < 200 || code >= 300
      then do
        body <- readBody (responseBody response) []
        let message = Text.unpack (Text.strip (decode body))
        hPutStrLn stderr ("llm: llama.cpp returned HTTP " ++ show code ++ errorSuffix message)
        pure Nothing
      else do
        finalState <-
          consumeResponse interruptFlag (responseBody response) BS.empty initialState
        endState <- flushReasoning finalState
        let trailing = finishMarkdown (streamMarkdown endState)
        putStr trailing
        unless (null trailing) (putStrLn "")
        unless (streamSawData endState) $
          hPutStrLn stderr "llm: no response content received"
        pure $
          if streamSawData endState
            then Just (Text.unpack (streamAnswerText endState))
            else Nothing

data StreamState = StreamState
  { streamMarkdown :: MarkdownState
  , streamReasoning :: MarkdownState
  , streamReasoningStarted :: Bool
  , streamAnswerStarted :: Bool
  , streamAnswerText :: Text.Text
  , streamSawData :: Bool
  , streamDone :: Bool
  }

initialState :: StreamState
initialState = StreamState
  { streamMarkdown = initialMarkdownState
  , streamReasoning = initialMarkdownState
  , streamReasoningStarted = False
  , streamAnswerStarted = False
  , streamAnswerText = ""
  , streamSawData = False
  , streamDone = False
  }

thinkingGrey :: String
thinkingGrey = "\ESC[38;5;245m"

flushReasoning :: StreamState -> IO StreamState
flushReasoning state
  | not (streamReasoningStarted state) = pure state
  | otherwise = do
      let trailing = finishMarkdownWith thinkingGrey (streamReasoning state)
      unless (null trailing) $ do
        putStr trailing
        putStrLn ""
      putStr "\ESC[0m"
      hFlush stdout
      pure state { streamReasoning = initialMarkdownState }

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
      reasoningState <- renderReasoning (extractReasoning value) state
      contentState <- renderContent (extractContent value) reasoningState
      case extractError value of
        Just message -> hPutStrLn stderr ("llm: " ++ Text.unpack message)
        Nothing -> pure ()
      pure
        contentState
          { streamSawData = True
          , streamDone = streamDone contentState || isStop value
          }
    _ -> pure state

renderReasoning :: Maybe Text.Text -> StreamState -> IO StreamState
renderReasoning Nothing state = pure state
renderReasoning (Just text) state
  | Text.null text = pure state
  | otherwise = do
      let (markdown', output) =
            renderMarkdownWith thinkingGrey (streamReasoning state) text
      emit output
      pure
        state
          { streamReasoning = markdown'
          , streamReasoningStarted = True
          }

renderContent :: Maybe Text.Text -> StreamState -> IO StreamState
renderContent Nothing state = pure state
renderContent (Just text) state
  | Text.null text = pure state
  | otherwise = do
      startingAnswer <- transitionFromReasoning state
      let (markdown', output) =
            renderMarkdown (streamMarkdown startingAnswer) text
      emit output
      pure
        startingAnswer
          { streamMarkdown = markdown'
          , streamAnswerStarted = True
          , streamAnswerText = streamAnswerText startingAnswer <> text
          }

transitionFromReasoning :: StreamState -> IO StreamState
transitionFromReasoning state
  | streamAnswerStarted state = pure state
  | otherwise = flushReasoning state

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

extractReasoning :: KeyMap.KeyMap Value -> Maybe Text.Text
extractReasoning value =
  firstString reasoningKeys value
    <|> choicesReasoning
  where
    choicesReasoning =
      case KeyMap.lookup "choices" value of
        Just (Array choices) ->
          listToMaybe (mapMaybe choiceReasoning (Vector.toList choices))
        _ -> Nothing

    choiceReasoning (Object choice) =
      case KeyMap.lookup "delta" choice of
        Just (Object delta) -> firstString reasoningKeys delta
        _ -> Nothing
    choiceReasoning _ = Nothing

    reasoningKeys = ["reasoning_content", "reasoning", "thinking"]

    firstString keys fields =
      listToMaybe
        [ text
        | key <- keys
        , Just (String text) <- [KeyMap.lookup key fields]
        ]

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
