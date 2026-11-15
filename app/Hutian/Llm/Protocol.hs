{-# LANGUAGE OverloadedStrings #-}

-- | Pure LLM protocol handling: settings, request bodies, stream parsing, and
-- the stream reducer. No terminal or network IO lives in this module.
module Hutian.Llm.Protocol
  ( LlmUrl (..)
  , ModelName (..)
  , SystemPrompt (..)
  , LlmSettings (..)
  , parseLlmSettings
  , parseLlmSettingsWithFiles
  , llmFilePaths
  , LlmResult (..)
  , emptyLlmResult
  , Api (..)
  , apiFor
  , requestBodyFor
  , continuationBodyFor
  , StreamEvent (..)
  , ToolCallDelta (..)
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
  , streamSawData
  , streamDone
  , StreamOutput (..)
  , applyStreamEvent
  , finishStream
  , resultFromState
  ) where

import Control.Applicative ((<|>))
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import Data.Char (isControl, isSpace)
import Data.List (isPrefixOf, isSuffixOf, nub)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncodingError
import qualified Data.Vector as Vector
import Data.Word (Word8)
import Hutian.Cli
  ( OptionKind (..)
  , hasOption
  , missingOptions
  , optionValueFor
  , parseOptions
  )
import Hutian.Conversation
  ( ChatMessage (..)
  , Conversation
  , ToolCall (..)
  , completionContinuationPrompt
  , completionPrompt
  , continuationMessages
  , requestMessages
  )
import Hutian.Markdown
  ( MarkdownState
  , finishMarkdown
  , finishMarkdownWith
  , initialMarkdownState
  , renderMarkdown
  , renderMarkdownWith
  )
import Hutian.Policy (ModelShell (..), modelShellAdvertised, modelShellFromArgument)
import Hutian.Render (boldTeal, dimTealForeground, grayForeground, reset)
import Hutian.Tool (toolDefinitions)

-- | Connection settings supplied by the launcher.
data LlmSettings = LlmSettings
  { settingUrl :: LlmUrl
  , settingModel :: ModelName
  , settingSystemPrompt :: SystemPrompt
  , settingSocket :: Maybe FilePath
  -- | How the model's shell tool is handled. Term mode is unaffected.
  , settingModelShell :: ModelShell
  }
  deriving stock (Eq, Show)

newtype LlmUrl = LlmUrl { unLlmUrl :: String }
  deriving stock (Eq, Show)

newtype ModelName = ModelName { unModelName :: String }
  deriving stock (Eq, Show)

newtype SystemPrompt = SystemPrompt { unSystemPrompt :: String }
  deriving stock (Eq, Show)

-- | The result of one streaming LLM request.
data LlmResult = LlmResult
  { llmResponseText :: Maybe String
  , llmToolCalls :: [ToolCall]
  }
  deriving stock (Eq, Show)

emptyLlmResult :: LlmResult
emptyLlmResult = LlmResult Nothing []

-- | Parse the launcher-provided connection settings. All three connection
-- options are required so the shell never falls back to ambient environment
-- variables. '--llama-socket' is optional and moves the HTTP request to the
-- host relay. '--model-shell' selects deny, ask, or allow for the model's shell
-- tool; its absence denies the tool, not the user's term mode.
parseLlmSettings :: [String] -> Either String LlmSettings
parseLlmSettings = parseLlmSettingsWithFiles []

-- | Parse settings with already-loaded contents for the file-backed options.
-- 'Hutian.Llm.loadLlmSettings' reads and removes the files before calling this
-- function so the contents never appear in the process listing.
parseLlmSettingsWithFiles :: [(FilePath, String)] -> [String] -> Either String LlmSettings
parseLlmSettingsWithFiles loaded arguments = do
  options <- withUsage (parseOptions llmOptionSpec arguments)
  let missingOption inline file =
        [ inline | not (hasOption inline options || hasOption file options) ]
      missing =
        missingOptions ["--llama-url"] options
          ++ missingOption "--model" "--model-file"
          ++ missingOption "--system-prompt" "--system-prompt-file"
  case missing of
    [] -> do
      url <- required "--llama-url" options
      model <- valueOrFile "--model" "--model-file" loaded options
      systemPrompt <- valueOrFile "--system-prompt" "--system-prompt-file" loaded options
      policy <- policyOption options
      settingsFrom
        (LlmUrl url)
        (ModelName model)
        (SystemPrompt systemPrompt)
        (optionValueFor "--llama-socket" options)
        policy
    _ -> Left (missingMessage missing)
  where
    required name options =
      case optionValueFor name options of
        Just value -> Right value
        Nothing -> Left ("missing required argument: " ++ name ++ "\n" ++ llmUsage)

    valueOrFile inline file fileContents options =
      case (optionValueFor inline options, optionValueFor file options) of
        (Just value, Nothing) -> Right value
        (Nothing, Just path) ->
          case lookup path fileContents of
            Just value -> Right value
            Nothing -> Left ("missing contents for " ++ file ++ ": " ++ path ++ "\n" ++ llmUsage)
        (Just _, Just _) ->
          Left ("use only one of " ++ inline ++ " and " ++ file ++ "\n" ++ llmUsage)
        (Nothing, Nothing) ->
          Left ("missing required argument: " ++ inline ++ " or " ++ file ++ "\n" ++ llmUsage)

    policyOption options =
      case optionValueFor "--model-shell" options of
        Nothing -> Right ModelShellDenied
        Just value ->
          case modelShellFromArgument value of
            Just policy -> Right policy
            Nothing ->
              Left
                ( "invalid value for --model-shell: "
                    ++ value
                    ++ " (expected deny, ask, or allow)\n"
                    ++ llmUsage
                )

    withUsage result =
      case result of
        Left message -> Left (message ++ "\n" ++ llmUsage)
        Right options -> Right options

-- | File paths the caller must read before 'parseLlmSettingsWithFiles'. The
-- paths are deduplicated so a caller can point both options at one file.
llmFilePaths :: [String] -> Either String [FilePath]
llmFilePaths arguments =
  case parseOptions llmOptionSpec arguments of
    Left message -> Left (message ++ "\n" ++ llmUsage)
    Right options ->
      Right . nub $
        catMaybes
          [ optionValueFor "--model-file" options
          , optionValueFor "--system-prompt-file" options
          ]

llmOptionSpec :: [(String, OptionKind)]
llmOptionSpec =
  [ ("--llama-url", OptionValue)
  , ("--model", OptionValue)
  , ("--model-file", OptionValue)
  , ("--system-prompt", OptionValue)
  , ("--system-prompt-file", OptionValue)
  , ("--llama-socket", OptionValue)
  , ("--model-shell", OptionValue)
  ]

-- | Reject malformed URLs, empty model names, and empty socket paths; an empty
-- system prompt is allowed.
settingsFrom :: LlmUrl -> ModelName -> SystemPrompt -> Maybe FilePath -> ModelShell -> Either String LlmSettings
settingsFrom url model systemPrompt socket policy = do
  validateLlmUrl url
  validateNonEmpty "--model" (unModelName model)
  maybe (Right ()) (validateNonEmpty "--llama-socket") socket
  pure (LlmSettings url model systemPrompt socket policy)
  where
    validateNonEmpty name value
      | null value = Left ("empty value for " ++ name ++ "\n" ++ llmUsage)
      | otherwise = Right ()

-- | A URL must use http or https, name a host, and, if it has a path, start
-- that path with a slash. Validation happens before any request is attempted.
validateLlmUrl :: LlmUrl -> Either String ()
validateLlmUrl (LlmUrl url)
  | null url = invalid "empty value for --llama-url"
  | any invalidChar url = invalid "invalid character in --llama-url"
  | otherwise =
      case stripScheme url of
        Nothing -> invalid "invalid value for --llama-url: expected an http:// or https:// URL"
        Just rest
          | null (authorityOf rest) ->
              invalid "invalid value for --llama-url: missing host"
          | not (null path) && take 1 path /= "/" ->
              invalid "invalid value for --llama-url: the path must start with /"
          | otherwise -> Right ()
  where
    invalid message = Left (message ++ "\n" ++ llmUsage)
    invalidChar character = isSpace character || isControl character
    path = pathOf url

stripScheme :: String -> Maybe String
stripScheme text
  | "http://" `isPrefixOf` text = Just (drop (length ("http://" :: String)) text)
  | "https://" `isPrefixOf` text = Just (drop (length ("https://" :: String)) text)
  | otherwise = Nothing

-- | The authority part of a URL after its scheme.
authorityOf :: String -> String
authorityOf = takeWhile (`notElem` ['/', '?', '#'])

-- | The path of a URL without its query or fragment.
pathOf :: String -> String
pathOf = takeWhile (`notElem` ['?', '#']) . dropWhile (`notElem` ['/', '?', '#'])

missingMessage :: [String] -> String
missingMessage missing =
  "missing required argument"
    ++ plural
    ++ ": "
    ++ unwords missing
    ++ "\n"
    ++ llmUsage
  where
    plural = if length missing > 1 then "s" else ""

llmUsage :: String
llmUsage =
  "usage: hutian-shell --llama-url URL (--model MODEL | --model-file PATH) (--system-prompt PROMPT | --system-prompt-file PATH) [--llama-socket PATH] [--model-shell deny|ask|allow]"

data Api
  = ChatApi
  | CompletionApi
  deriving stock (Eq, Show)

apiFor :: LlmUrl -> Api
apiFor (LlmUrl url)
  | "/v1/chat/completions" `isSuffixOf` pathOf url = ChatApi
  | otherwise = CompletionApi

requestBodyFor :: ModelShell -> Api -> ModelName -> SystemPrompt -> Conversation -> String -> Aeson.Value
requestBodyFor policy api model systemPrompt conversation prompt =
  case api of
    ChatApi -> chatBody policy model (requestMessages (unSystemPrompt systemPrompt) conversation prompt)
    CompletionApi ->
      object
        [ "model" .= unModelName model
        , "prompt" .= completionPrompt (unSystemPrompt systemPrompt) conversation prompt
        , "stream" .= True
        ]

-- | Build a follow-up request after tool results, without a new user message.
continuationBodyFor :: ModelShell -> Api -> ModelName -> SystemPrompt -> Conversation -> Aeson.Value
continuationBodyFor policy api model systemPrompt conversation =
  case api of
    ChatApi -> chatBody policy model (continuationMessages (unSystemPrompt systemPrompt) conversation)
    CompletionApi ->
      object
        [ "model" .= unModelName model
        , "prompt" .= completionContinuationPrompt (unSystemPrompt systemPrompt) conversation
        , "stream" .= True
        ]

chatBody :: ModelShell -> ModelName -> [ChatMessage] -> Aeson.Value
chatBody policy model messages =
  object $
    [ "model" .= unModelName model
    , "messages" .= map messageValue messages
    , "stream" .= True
    ]
      ++ toolFields
  where
    toolFields
      | modelShellAdvertised policy =
          [ "tools" .= toolDefinitions
          , "tool_choice" .= ("auto" :: String)
          ]
      | otherwise = []

messageValue :: ChatMessage -> Aeson.Value
messageValue message =
  object $
    case message of
      SystemMessage text -> roleMessage "system" text
      UserMessage text -> roleMessage "user" text
      AssistantMessage text -> roleMessage "assistant" text
      AssistantToolCallMessage text calls ->
        roleMessage "assistant" text
          ++ [ "tool_calls" .= map toolCallValue calls
             | not (null calls)
             ]
      ToolResultMessage callId output ->
        roleMessage "tool" output
          ++ ["tool_call_id" .= callId]
  where
    roleMessage role text =
      [ "role" .= (role :: String)
      , "content" .= text
      ]

toolCallValue :: ToolCall -> Aeson.Value
toolCallValue call =
  object
    [ "id" .= toolCallId call
    , "type" .= ("function" :: String)
    , "function" .=
        object
          [ "name" .= toolCallName call
          , "arguments" .= toolCallArguments call
          ]
    ]

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

-- | The pure accumulator for one streaming response.
data StreamState = StreamState
  { streamMarkdown :: MarkdownState
  , streamReasoning :: MarkdownState
  , streamReasoningStarted :: Bool
  , streamAnswerStarted :: Bool
  , streamAnswerText :: Text.Text
  , streamToolCalls :: [ToolCall]
  , streamSawData :: Bool
  , streamDone :: Bool
  }

initialStreamState :: StreamState
initialStreamState =
  StreamState
    { streamMarkdown = initialMarkdownState
    , streamReasoning = initialMarkdownState
    , streamReasoningStarted = False
    , streamAnswerStarted = False
    , streamAnswerText = ""
    , streamToolCalls = []
    , streamSawData = False
    , streamDone = False
    }

-- | Everything one stream step wants written to the terminal.
data StreamOutput = StreamOutput
  { outputText :: String
  , outputError :: Maybe Text.Text
  }
  deriving stock (Eq, Show)

emptyStreamOutput :: StreamOutput
emptyStreamOutput = StreamOutput "" Nothing

-- | Apply one parsed event to the accumulator. This is pure so the whole
-- streaming protocol can be tested without a terminal or a network.
applyStreamEvent :: StreamEvent -> StreamState -> (StreamState, StreamOutput)
applyStreamEvent event state =
  ( finalState
  , StreamOutput
      { outputText = outputText reasoningOutput ++ outputText contentOutput
      , outputError = eventError event
      }
  )
  where
    (reasoningState, reasoningOutput) = appendReasoning (eventReasoning event) state
    (contentState, contentOutput) = appendContent (eventContent event) reasoningState
    toolState =
      contentState
        { streamToolCalls =
            mergeToolCallDeltas (streamToolCalls contentState) (eventToolCalls event)
        }
    finalState =
      toolState
        { streamSawData = True
        , streamDone = streamDone toolState || eventStop event
        }

appendReasoning :: Maybe Text.Text -> StreamState -> (StreamState, StreamOutput)
appendReasoning Nothing state = (state, emptyStreamOutput)
appendReasoning (Just text) state
  | Text.null text = (state, emptyStreamOutput)
  | otherwise = (state', StreamOutput (header ++ output) Nothing)
  where
    header
      | streamReasoningStarted state = ""
      | otherwise = dimTealForeground ++ "think:" ++ reset ++ "\n"
    (markdown', output) =
      renderMarkdownWith thinkingGrey (streamReasoning state) text
    state' =
      state
        { streamReasoning = markdown'
        , streamReasoningStarted = True
        }

appendContent :: Maybe Text.Text -> StreamState -> (StreamState, StreamOutput)
appendContent Nothing state = (state, emptyStreamOutput)
appendContent (Just text) state
  | Text.null text = (state, emptyStreamOutput)
  | otherwise = (state', StreamOutput (flushed ++ header ++ output) Nothing)
  where
    (startingAnswer, flushed) = flushReasoning state
    header
      | streamAnswerStarted startingAnswer = ""
      | otherwise = boldTeal ++ "answer:" ++ reset ++ "\n"
    (markdown', output) = renderMarkdown (streamMarkdown startingAnswer) text
    state' =
      startingAnswer
        { streamMarkdown = markdown'
        , streamAnswerStarted = True
        , streamAnswerText = streamAnswerText startingAnswer <> text
        }

-- | Flush buffered reasoning text when the answer begins or the stream ends.
flushReasoning :: StreamState -> (StreamState, String)
flushReasoning state
  | not (streamReasoningStarted state) = (state, "")
  | otherwise = (state { streamReasoning = initialMarkdownState }, output)
  where
    trailing = finishMarkdownWith thinkingGrey (streamReasoning state)
    output = (if null trailing then "" else trailing ++ "\n") ++ reset

-- | Flush the final partial answer and any open code fence.
finishStream :: StreamState -> (StreamState, StreamOutput)
finishStream state =
  (stateAfterReasoning, StreamOutput (reasoningOutput ++ answerOutput) Nothing)
  where
    (stateAfterReasoning, reasoningOutput) = flushReasoning state
    trailing = finishMarkdown (streamMarkdown stateAfterReasoning)
    answerOutput = trailing ++ if null trailing then "" else "\n"

resultFromState :: StreamState -> LlmResult
resultFromState state =
  LlmResult
    { llmResponseText = responseText
    , llmToolCalls = streamToolCalls state
    }
  where
    calls = streamToolCalls state
    answer = streamAnswerText state
    responseText
      | not (streamSawData state) = Nothing
      | Text.null answer && not (null calls) = Nothing
      | otherwise = Just (Text.unpack answer)

-- | Thinking text uses the same grey as command output.
thinkingGrey :: String
thinkingGrey = grayForeground

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
lookupIndex fields =
  case KeyMap.lookup "index" fields of
    Just (Number number) -> Just (round number)
    _ -> Nothing

lookupFunctionString :: Key -> KeyMap.KeyMap Value -> Maybe Text.Text
lookupFunctionString key fields =
  case KeyMap.lookup "function" fields of
    Just (Object function) -> lookupString key function
    _ -> Nothing

-- | Merge streamed tool-call fragments by their index.
mergeToolCallDeltas :: [ToolCall] -> [ToolCallDelta] -> [ToolCall]
mergeToolCallDeltas = foldl' mergeOne
  where
    mergeOne calls delta =
      let index = toolCallDeltaIndex delta
          (before, rest) = splitAt index calls
          existing =
            case rest of
              call : _ -> call
              [] -> ToolCall "" "" ""
          updated =
            existing
              { toolCallId = preferText (toolCallId existing) (toolCallDeltaId delta)
              , toolCallName = preferText (toolCallName existing) (toolCallDeltaName delta)
              , toolCallArguments =
                  toolCallArguments existing
                    ++ maybe "" Text.unpack (toolCallDeltaArguments delta)
              }
       in before ++ [updated] ++ drop 1 rest

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
