{-# LANGUAGE OverloadedStrings #-}
{-@ LIQUID "--no-termination" @-}

-- | Pure stream protocol handling: server-sent event parsing, tool-call
-- assembly, and the accumulator that produces a model result. Nothing here
-- writes to the terminal; "Hutian.Llm.Presentation" renders the events.
module Hutian.Llm.Stream
  ( LlmResult (..)
  , emptyLlmResult
  , StreamEvent (StreamEvent, eventReasoning, eventContent, eventError, eventToolCalls)
  , ToolCallDelta (ToolCallDelta)
  , parseStreamLine
  , splitLines
  , extractContent
  , extractReasoning
  , extractError
  , extractToolCalls
  , mergeToolCallDeltas
  , isStop
  , decode
  , StreamState
  , initialStreamState
  , streamAnswerText
  , streamSawData
  , streamDone
  , applyStreamEvent
  , resultFromState
  , resultUnlessInterrupted
  ) where

import Control.Applicative ((<|>))
import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncodingError
import qualified Data.Vector as Vector
import Data.Word (Word8)
import Hutian.Conversation (ToolCall (..))
import Hutian.Json (jsonInt)

-- | The result of one streaming LLM request.
data LlmResult = LlmResult
  { llmResponseText :: Maybe String
  , llmToolCalls :: [ToolCall]
  }
  deriving stock (Eq, Show)

emptyLlmResult :: LlmResult
emptyLlmResult = LlmResult Nothing []

-- | One streaming fragment of a tool call. The first fragment usually carries
-- the id and function name; later fragments append to the arguments.
data ToolCallDelta = ToolCallDelta
  { toolCallDeltaIndex :: Int
  , toolCallDeltaId :: Maybe Text.Text
  , toolCallDeltaName :: Maybe Text.Text
  , toolCallDeltaArguments :: Maybe Text.Text
  }
  deriving stock (Eq, Show)

-- | A parsed server-sent event from a streaming response.
data StreamEvent = StreamEvent
  { eventReasoning :: Maybe Text.Text
  , eventContent :: Maybe Text.Text
  , eventError :: Maybe Text.Text
  , eventToolCalls :: [ToolCallDelta]
  , eventStop :: Bool
  }
  deriving stock (Eq, Show)

-- | The pure accumulator for one streaming response. It keeps only what the
-- result needs; rendering state lives in "Hutian.Llm.Presentation".
data StreamState = StreamState
  { streamAnswerText :: Text.Text
  , streamToolCalls :: IntMap.IntMap ToolCall
  , streamSawData :: Bool
  , streamDone :: Bool
  }

initialStreamState :: StreamState
initialStreamState =
  StreamState
    { streamAnswerText = ""
    , streamToolCalls = IntMap.empty
    , streamSawData = False
    , streamDone = False
    }

-- | Apply one parsed event to the accumulator. Presentation is deliberately
-- separate so the protocol can be tested without a terminal.
applyStreamEvent :: StreamEvent -> StreamState -> StreamState
applyStreamEvent event state =
  state
    { streamAnswerText =
        streamAnswerText state <> fromMaybe "" (eventContent event)
    , streamToolCalls =
        mergeToolCallDeltas (streamToolCalls state) (eventToolCalls event)
    , streamSawData = True
    , streamDone = streamDone state || eventStop event
    }

resultFromState :: StreamState -> LlmResult
resultFromState state =
  LlmResult
    { llmResponseText = responseText
    , llmToolCalls = calls
    }
  where
    -- A delta may arrive for an index whose name fragment was lost or never
    -- sent. Such a call cannot be planned, so it is not handed to the executor.
    calls = filter (not . null . toolCallName) (IntMap.elems (streamToolCalls state))
    answer = streamAnswerText state
    responseText
      | not (streamSawData state) = Nothing
      | Text.null answer && not (null calls) = Nothing
      | otherwise = Just (Text.unpack answer)

-- | Publish a stream result, but never expose partial tool calls when the
-- stream was interrupted. Their argument JSON may be truncated, and executing
-- a half-received command is exactly what the interrupt was meant to prevent.
resultUnlessInterrupted :: Bool -> StreamState -> LlmResult
resultUnlessInterrupted interrupted state
  | interrupted = emptyLlmResult
  | otherwise = resultFromState state

splitLines :: BS.ByteString -> ([BS.ByteString], BS.ByteString)
splitLines = go []
  where
    go acc input =
      case BS.elemIndex 10 input of
        Nothing -> (reverse acc, input)
        Just index ->
          let (line, rest) = BS.splitAt index input
           in go (line : acc) (BS.drop 1 rest)

-- | Parse one SSE line, or a bare JSON line, into a stream event.
parseStreamLine :: BS.ByteString -> Maybe StreamEvent
parseStreamLine rawLine
  | BS.null line = Nothing
  | "data:" `BS.isPrefixOf` line = parsePayload (Text.strip (decode (BS.drop 5 line)))
  | otherwise = parsePayload (Text.strip (decode line))
  where
    line = BS.dropWhileEnd isCarriageReturn rawLine

parsePayload :: Text.Text -> Maybe StreamEvent
parsePayload payload
  | payload == "[DONE]" = Just (StreamEvent Nothing Nothing Nothing [] True)
  | otherwise =
      case Aeson.decodeStrict' (TextEncoding.encodeUtf8 payload) of
        Just (Object value) ->
          Just
            StreamEvent
              { eventReasoning = extractReasoning value
              , eventContent = extractContent value
              , eventError = extractError value
              , eventToolCalls = extractToolCalls value
              , eventStop = isStop value
              }
        _ -> Nothing

isCarriageReturn :: Word8 -> Bool
isCarriageReturn = (== 13)

extractContent :: KeyMap.KeyMap Value -> Maybe Text.Text
extractContent value =
  lookupString "content" value
    <|> listToMaybe (mapMaybe (lookupString "content") (deltas value))

extractReasoning :: KeyMap.KeyMap Value -> Maybe Text.Text
extractReasoning value =
  firstString reasoningKeys value
    <|> listToMaybe (mapMaybe (firstString reasoningKeys) (deltas value))
  where
    reasoningKeys = ["reasoning_content", "reasoning", "thinking"]

extractError :: KeyMap.KeyMap Value -> Maybe Text.Text
extractError value =
  case KeyMap.lookup "error" value of
    Just (String message) -> Just message
    Just (Object nested) ->
      lookupString "message" nested
        <|> Just (Text.pack (show (Object nested)))
    _ -> Nothing

-- | Extract tool-call fragments from either a bare delta object or the deltas
-- nested under @choices@.
extractToolCalls :: KeyMap.KeyMap Value -> [ToolCallDelta]
extractToolCalls value =
  indexedToolCalls 0 (toolCallObjects value)
    ++ concatMap (indexedToolCalls 0 . toolCallObjects) (deltas value)

indexedToolCalls :: Int -> [KeyMap.KeyMap Value] -> [ToolCallDelta]
indexedToolCalls start objects =
  [ ToolCallDelta
      { toolCallDeltaIndex = fromMaybe (start + offset) (lookupIndex call)
      , toolCallDeltaId = lookupString "id" call
      , toolCallDeltaName = lookupFunctionString "name" call
      , toolCallDeltaArguments = lookupFunctionString "arguments" call
      }
  | (offset, call) <- zip [0 ..] objects
  ]

toolCallObjects :: KeyMap.KeyMap Value -> [KeyMap.KeyMap Value]
toolCallObjects fields =
  case KeyMap.lookup "tool_calls" fields of
    Just (Array items) -> [item | Object item <- Vector.toList items]
    _ -> []

lookupIndex :: KeyMap.KeyMap Value -> Maybe Int
lookupIndex fields = do
  value <- KeyMap.lookup "index" fields
  index <- jsonInt value
  if index >= 0 then Just index else Nothing

lookupFunctionString :: Key -> KeyMap.KeyMap Value -> Maybe Text.Text
lookupFunctionString key fields =
  case KeyMap.lookup "function" fields of
    Just (Object function) -> lookupString key function
    _ -> Nothing

-- | Merge streamed tool-call fragments by their index. A sparse or
-- out-of-order index must not create placeholder calls or shift later ones,
-- so the accumulator is keyed by index and compacted only when the stream is
-- finished.
mergeToolCallDeltas :: IntMap.IntMap ToolCall -> [ToolCallDelta] -> IntMap.IntMap ToolCall
mergeToolCallDeltas = foldl' mergeOne
  where
    mergeOne accumulated delta =
      IntMap.alter
        (Just . applyDelta . fromMaybe (ToolCall "" "" ""))
        (toolCallDeltaIndex delta)
        accumulated
      where
        applyDelta existing =
          existing
            { toolCallId = preferText (toolCallId existing) (toolCallDeltaId delta)
            , toolCallName = preferText (toolCallName existing) (toolCallDeltaName delta)
            , toolCallArguments =
                toolCallArguments existing
                  ++ maybe "" Text.unpack (toolCallDeltaArguments delta)
            }

preferText :: String -> Maybe Text.Text -> String
preferText current Nothing = current
preferText current (Just text)
  | Text.null text = current
  | otherwise = Text.unpack text

isStop :: KeyMap.KeyMap Value -> Bool
isStop value =
  case KeyMap.lookup "stop" value of
    Just (Bool True) -> True
    _ -> any choiceFinished (choices value)
  where
    choiceFinished choice =
      case KeyMap.lookup "finish_reason" choice of
        Nothing -> False
        Just Null -> False
        Just _ -> True

-- | The choice objects of a streaming response.
choices :: KeyMap.KeyMap Value -> [KeyMap.KeyMap Value]
choices value =
  case KeyMap.lookup "choices" value of
    Just (Array items) -> [choice | Object choice <- Vector.toList items]
    _ -> []

-- | The non-empty delta objects of a streaming response.
deltas :: KeyMap.KeyMap Value -> [KeyMap.KeyMap Value]
deltas = mapMaybe delta . choices
  where
    delta choice =
      case KeyMap.lookup "delta" choice of
        Just (Object fields) -> Just fields
        _ -> Nothing

lookupString :: Key -> KeyMap.KeyMap Value -> Maybe Text.Text
lookupString key fields =
  case KeyMap.lookup key fields of
    Just (String text) -> Just text
    _ -> Nothing

firstString :: [Key] -> KeyMap.KeyMap Value -> Maybe Text.Text
firstString keys fields =
  listToMaybe
    [ text
    | key <- keys
    , Just (String text) <- [KeyMap.lookup key fields]
    ]

decode :: BS.ByteString -> Text.Text
decode = TextEncoding.decodeUtf8With TextEncodingError.lenientDecode
