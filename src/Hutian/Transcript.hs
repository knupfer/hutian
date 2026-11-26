{-# LANGUAGE OverloadedStrings #-}

-- | Pure transcript model, JSON encoding, Markdown rendering, and path
-- derivation. The recorder in "Hutian.Transcript.Recorder" performs the IO.
module Hutian.Transcript
  ( SessionInfo (..)
  , ToolOutcome (..)
  , ToolResult (..)
  , TranscriptEvent (..)
  , TranscriptPaths (..)
  , transcriptPaths
  , encodeTranscriptEvent
  , renderTranscriptEvent
  , shiftMarkdownHeadings
  ) where

import Data.Aeson (ToJSON (..), Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, intercalate)
import Hutian.Context (TurnContext, contextPath)
import Hutian.Conversation
  ( Conversation
  , ConversationEntry (..)
  , ToolCall (..)
  )
import Hutian.Editor (InputMode (..))
import Hutian.Render (ToolApproval (..))
import System.FilePath
  ( hasTrailingPathSeparator
  , replaceExtension
  , takeExtension
  , (</>)
  , (<.>)
  )

-- | Metadata recorded when a session starts.
newtype SessionInfo = SessionInfo
  { sessionInfoModel :: String
  }
  deriving stock (Eq, Show)

-- | Whether a tool call succeeded, independent of the output text.
data ToolOutcome
  = ToolSucceeded
  | ToolFailed
  deriving stock (Eq, Show)

-- | One finished tool call.
data ToolResult = ToolResult
  { toolResultCallId :: String
  , toolResultName :: String
  , toolResultOutcome :: ToolOutcome
  , toolResultText :: String
  }
  deriving stock (Eq, Show)

-- | One semantic session event. The constructors carry data, not rendered text,
-- so verbosity cannot hide an event from the transcript.
data TranscriptEvent
  = TranscriptSessionStarted SessionInfo
  | TranscriptSessionEnded String
  | TranscriptInput InputMode String
  | TranscriptTerminalCommand String
  | TranscriptReasoning TurnContext String
  | TranscriptAssistant TurnContext String [ToolCall]
  | TranscriptAgentPrompt TurnContext String String
  | TranscriptToolRequest TurnContext String ToolCall
  | TranscriptToolDecision TurnContext String ToolApproval
  | TranscriptToolResult TurnContext ToolResult
  | TranscriptDiagnostic String
  | TranscriptInterrupted TurnContext
  | TranscriptConversationSnapshot Conversation
  deriving stock (Eq, Show)

-- | The two transcript files.
data TranscriptPaths = TranscriptPaths
  { transcriptJsonPath :: FilePath
  , transcriptMarkdownPath :: FilePath
  }
  deriving stock (Eq, Show)

-- | The JSON Lines and Markdown paths for a user-supplied transcript path. A
-- path ending in @.jsonl@ or @.md@ names that file, and the sibling gets the
-- other extension; any other path gets both extensions.
transcriptPaths :: FilePath -> TranscriptPaths
transcriptPaths path
  | hasTrailingPathSeparator path =
      TranscriptPaths
        (path </> "transcript.jsonl")
        (path </> "transcript.md")
  | otherwise =
      case map toLower (takeExtension path) of
        ".jsonl" ->
          TranscriptPaths path (replaceExtension path "md")
        ".md" ->
          TranscriptPaths (replaceExtension path "jsonl") path
        _ ->
          TranscriptPaths (path <.> "jsonl") (path <.> "md")

-- | Encode one event as a JSON Lines record, including its trailing newline.
encodeTranscriptEvent :: String -> TranscriptEvent -> BL.ByteString
encodeTranscriptEvent at event =
  Aeson.encode (TranscriptEnvelope at event) <> "\n"

data TranscriptEnvelope = TranscriptEnvelope String TranscriptEvent

instance ToJSON TranscriptEnvelope where
  toJSON (TranscriptEnvelope at event) =
    case toJSON event of
      Object fields ->
        Object
          ( KeyMap.insert
              "schema"
              (toJSON (1 :: Int))
              (KeyMap.insert "at" (toJSON at) fields)
          )
      value -> value

instance ToJSON TranscriptEvent where
  toJSON event =
    object (("type" .= eventType event) : eventFields event)

eventType :: TranscriptEvent -> String
eventType event =
  case event of
    TranscriptSessionStarted {} -> "session.start"
    TranscriptSessionEnded {} -> "session.end"
    TranscriptInput {} -> "input"
    TranscriptTerminalCommand {} -> "term.command"
    TranscriptReasoning {} -> "reasoning"
    TranscriptAssistant {} -> "assistant"
    TranscriptAgentPrompt {} -> "agent.prompt"
    TranscriptToolRequest {} -> "tool.request"
    TranscriptToolDecision {} -> "tool.decision"
    TranscriptToolResult {} -> "tool.result"
    TranscriptDiagnostic {} -> "diagnostic"
    TranscriptInterrupted {} -> "interrupted"
    TranscriptConversationSnapshot {} -> "conversation.snapshot"

eventFields :: TranscriptEvent -> [(Key, Value)]
eventFields event =
  case event of
    TranscriptSessionStarted info ->
      ["model" .= sessionInfoModel info]
    TranscriptSessionEnded reason -> ["reason" .= reason]
    TranscriptInput mode text ->
      ["mode" .= inputModeName mode, "text" .= text]
    TranscriptTerminalCommand text -> ["text" .= text]
    TranscriptReasoning context text ->
      ["context" .= contextName context, "text" .= text]
    TranscriptAssistant context text calls ->
      [ "context" .= contextName context
      , "text" .= text
      , "toolCalls" .= map toolCallJSON calls
      ]
    TranscriptAgentPrompt context label text ->
      [ "context" .= contextName context
      , "label" .= label
      , "text" .= text
      ]
    TranscriptToolRequest context displayName call ->
      [ "context" .= contextName context
      , "displayName" .= displayName
      , "call" .= toolCallJSON call
      ]
    TranscriptToolDecision context callId decision ->
      [ "context" .= contextName context
      , "callId" .= callId
      , "decision" .= approvalName decision
      ]
    TranscriptToolResult context result ->
      [ "context" .= contextName context
      , "callId" .= toolResultCallId result
      , "name" .= toolResultName result
      , "ok" .= toolOutcomeOk (toolResultOutcome result)
      , "text" .= toolResultText result
      ]
    TranscriptDiagnostic text -> ["text" .= text]
    TranscriptInterrupted context -> ["context" .= contextName context]
    TranscriptConversationSnapshot conversation ->
      ["conversation" .= map conversationEntryJSON conversation]

-- | Render one event as Markdown. Every prompt and block label is a level-one
-- heading. Headings inside any block are shifted by one unless they are inside a
-- fenced code block. No block is wrapped in a synthetic fence.
renderTranscriptEvent :: String -> TranscriptEvent -> String
renderTranscriptEvent at event =
  case event of
    TranscriptSessionStarted info ->
      "Hutian session — "
        ++ at
        ++ "\n\n- model: `"
        ++ sessionInfoModel info
        ++ "`\n\n---\n"
    TranscriptSessionEnded reason ->
      "\n---\nSession ended at " ++ at ++ " (" ++ reason ++ ").\n"
    TranscriptInput mode text -> section (inputLabel mode) text
    TranscriptTerminalCommand text -> section "shell" text
    TranscriptReasoning context text
      | all isSpace text -> ""
      | otherwise -> section (contextLabel context "thinking") text
    TranscriptAssistant context text _
      | all isSpace text -> ""
      | otherwise -> section (contextLabel context "answer") text
    TranscriptAgentPrompt _ label text -> section label text
    TranscriptToolRequest context displayName call ->
      section (contextLabel context displayName) (toolCallArguments call)
    TranscriptToolDecision _ _ decision ->
      "\n" ++ decisionText decision ++ "\n"
    TranscriptToolResult context result ->
      section
        (contextLabel context (toolResultName result ++ outcomeLabel (toolResultOutcome result)))
        (toolResultText result)
    TranscriptDiagnostic text
      | all isSpace text -> ""
      | otherwise -> "\n" ++ blockquote text
    TranscriptInterrupted context ->
      "\n> " ++ contextLabel context "interrupted" ++ ".\n"
    TranscriptConversationSnapshot _ -> ""

-- | One labelled Markdown block.
section :: String -> String -> String
section label text =
  "\n" ++ heading label ++ "\n" ++ contentBlock text

-- | A block's content, with leading and trailing blank lines removed and its
-- headings demoted under the block heading. Fenced code is left alone.
contentBlock :: String -> String
contentBlock text =
  case dropWhileEnd (all isSpace) (dropWhile (all isSpace) (lines text)) of
    [] -> ""
    body -> shiftMarkdownHeadings 1 (unlines body)

heading :: String -> String
heading text = "# " ++ text

contextLabel :: TurnContext -> String -> String
contextLabel context name =
  case contextName context of
    "" -> name
    path -> path ++ "." ++ name

contextName :: TurnContext -> String
contextName = intercalate "." . contextPath

inputModeName :: InputMode -> String
inputModeName LlmInput = "llm"
inputModeName TermInput = "term"

inputLabel :: InputMode -> String
inputLabel LlmInput = "ask"
inputLabel TermInput = "shell"

approvalName :: ToolApproval -> String
approvalName ToolApproved = "approved"
approvalName ToolDenied = "denied"
approvalName ToolAborted = "aborted"

decisionText :: ToolApproval -> String
decisionText ToolApproved = "_Approved._"
decisionText ToolDenied = "_Denied._"
decisionText ToolAborted = "_Aborted._"

outcomeLabel :: ToolOutcome -> String
outcomeLabel ToolSucceeded = " result"
outcomeLabel ToolFailed = " error"

toolOutcomeOk :: ToolOutcome -> Bool
toolOutcomeOk ToolSucceeded = True
toolOutcomeOk ToolFailed = False

toolCallJSON :: ToolCall -> Value
toolCallJSON call =
  object
    [ "id" .= toolCallId call
    , "name" .= toolCallName call
    , "arguments" .= toolCallArguments call
    ]

conversationEntryJSON :: ConversationEntry -> Value
conversationEntryJSON entry =
  case entry of
    UserEntry text ->
      object ["kind" .= ("user" :: String), "text" .= text]
    AssistantEntry text ->
      object ["kind" .= ("assistant" :: String), "text" .= text]
    TerminalEntry command ->
      object ["kind" .= ("terminal" :: String), "command" .= command]
    AssistantToolCallEntry text calls ->
      object
        [ "kind" .= ("assistant_tool_calls" :: String)
        , "text" .= text
        , "toolCalls" .= map toolCallJSON calls
        ]
    ToolResultEntry callId output ->
      object
        [ "kind" .= ("tool_result" :: String)
        , "callId" .= callId
        , "text" .= output
        ]

blockquote :: String -> String
blockquote text = unlines (map ("> " ++) (lines text))

-- | Shift top-level ATX headings outside fenced code blocks. Setext headings and
-- headings nested in blockquotes or lists are left unchanged. Headings at level
-- six stay at level six because Markdown has no level seven.
shiftMarkdownHeadings :: Int -> String -> String
shiftMarkdownHeadings amount = unlines . go Nothing . lines
  where
    go _ [] = []
    go fence (line : rest) =
      case fence of
        Just (character, length') ->
          line
            : go
              ( if isClosingFence character length' line
                  then Nothing
                  else Just (character, length')
              )
              rest
        Nothing ->
          case openingFence line of
            Just marker -> line : go (Just marker) rest
            Nothing -> shiftLine amount line : go Nothing rest

-- | An opening fence is up to three spaces followed by at least three backticks
-- or tildes. Return the marker character and its run length.
openingFence :: String -> Maybe (Char, Int)
openingFence line =
  case span (== ' ') line of
    (spaces, rest)
      | length spaces <= 3 ->
          case rest of
            character : _
              | character == '`' || character == '~' ->
                  let run = takeWhile (== character) rest
                   in if length run >= 3
                        then Just (character, length run)
                        else Nothing
            _ -> Nothing
    _ -> Nothing

isClosingFence :: Char -> Int -> String -> Bool
isClosingFence character length' line =
  case span (== ' ') line of
    (spaces, rest)
      | length spaces <= 3 ->
          let (run, after) = span (== character) rest
           in length run >= length' && all isSpace after
    _ -> False

shiftLine :: Int -> String -> String
shiftLine amount line =
  case span (== ' ') line of
    (spaces, rest)
      | length spaces <= 3 ->
          let (hashes, after) = span (== '#') rest
           in if not (null hashes)
                && length hashes <= 6
                && (null after || take 1 after == " ")
                then
                  spaces
                    ++ hashes
                    ++ replicate (min amount (6 - length hashes)) '#'
                    ++ after
                else line
    _ -> line
