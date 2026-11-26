{-# LANGUAGE OverloadedStrings #-}

-- | Structured session transcripts. Events are written as JSON Lines for tools
-- and as Markdown for people. Every prompt and block label is a level-one
-- heading, and model-authored headings inside a block are shifted down by one
-- unless they are inside a fenced code block.
module Hutian.Transcript
  ( Transcript
  , SessionInfo (..)
  , TranscriptEvent (..)
  , openTranscript
  , closeTranscript
  , recordTranscript
  , recordTranscriptMaybe
  , transcriptPaths
  , shiftMarkdownHeadings
  , encodeTranscriptEvent
  , renderTranscriptEvent
  ) where

import Control.Exception (finally, onException)
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Key (Key)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace, toLower)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (dropWhileEnd, intercalate)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Hutian.Context (TurnContext, contextPath)
import Hutian.Conversation
  ( Conversation
  , ConversationEntry (..)
  , ToolCall (..)
  )
import Hutian.Editor (InputMode (..))
import Hutian.Render (ToolApproval (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath
  ( hasTrailingPathSeparator
  , replaceExtension
  , takeDirectory
  , takeExtension
  , (</>)
  , (<.>)
  )
import System.IO
  ( BufferMode (LineBuffering)
  , Handle
  , IOMode (WriteMode)
  , hClose
  , hFlush
  , hPutStr
  , hSetBuffering
  , openFile
  )
import System.Posix.Files (setFileMode)

-- | Metadata recorded when a session starts.
newtype SessionInfo = SessionInfo
  { sessionInfoModel :: String
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
  | TranscriptToolResult TurnContext String String Bool String
  | TranscriptDiagnostic String
  | TranscriptInterrupted TurnContext
  | TranscriptConversationSnapshot Conversation
  deriving stock (Eq, Show)

-- | Two open files and the reasoning chunks that have not been flushed yet.
-- Reasoning arrives in stream fragments; buffering it yields one Markdown block
-- instead of one block per token.
data Transcript = Transcript
  { transcriptJsonHandle :: Handle
  , transcriptMarkdownHandle :: Handle
  , transcriptPendingReasoning :: IORef (Maybe (TurnContext, [String]))
  }

-- | Open the JSON Lines and Markdown files for a transcript path. A path ending
-- in @.jsonl@ or @.md@ names that file, and the sibling gets the other
-- extension; any other path gets both extensions.
openTranscript :: FilePath -> IO Transcript
openTranscript path = do
  let (jsonPath, markdownPath) = transcriptPaths path
  createDirectoryIfMissing True (takeDirectory jsonPath)
  jsonHandle <- openFile jsonPath WriteMode
  markdownHandle <-
    openFile markdownPath WriteMode `onException` hClose jsonHandle
  let closeBoth = hClose markdownHandle >> hClose jsonHandle
  ( do
      setFileMode jsonPath 0o600
      setFileMode markdownPath 0o600
      hSetBuffering jsonHandle LineBuffering
      hSetBuffering markdownHandle LineBuffering
    )
    `onException` closeBoth
  pending <- newIORef Nothing
  pure
    Transcript
      { transcriptJsonHandle = jsonHandle
      , transcriptMarkdownHandle = markdownHandle
      , transcriptPendingReasoning = pending
      }

-- | Flush any buffered reasoning and close both files.
closeTranscript :: Transcript -> IO ()
closeTranscript transcript = flushAndClose `finally` closeBoth
  where
    flushAndClose = do
      flushReasoning transcript
      hFlush (transcriptJsonHandle transcript)
      hFlush (transcriptMarkdownHandle transcript)
    closeBoth = do
      hClose (transcriptJsonHandle transcript)
      hClose (transcriptMarkdownHandle transcript)

-- | Record one event. Reasoning is buffered until the next event, so its
-- streamed fragments become one transcript block.
recordTranscript :: Transcript -> TranscriptEvent -> IO ()
recordTranscript transcript event =
  case event of
    TranscriptReasoning context text
      | not (null text) -> appendReasoning transcript context text
      | otherwise -> pure ()
    _ -> do
      flushReasoning transcript
      writeEvent transcript event

-- | 'recordTranscript' for an optional recorder.
recordTranscriptMaybe :: Maybe Transcript -> TranscriptEvent -> IO ()
recordTranscriptMaybe Nothing _ = pure ()
recordTranscriptMaybe (Just transcript) event = recordTranscript transcript event

-- | The JSON Lines and Markdown paths for a user-supplied transcript path.
transcriptPaths :: FilePath -> (FilePath, FilePath)
transcriptPaths path
  | hasTrailingPathSeparator path =
      (path </> "transcript.jsonl", path </> "transcript.md")
  | otherwise =
      case map toLower (takeExtension path) of
        ".jsonl" -> (path, replaceExtension path "md")
        ".md" -> (replaceExtension path "jsonl", path)
        _ -> (path <.> "jsonl", path <.> "md")

appendReasoning :: Transcript -> TurnContext -> String -> IO ()
appendReasoning transcript context text = do
  pending <- readIORef (transcriptPendingReasoning transcript)
  case pending of
    Just (pendingContext, chunks)
      | pendingContext == context ->
          writeIORef
            (transcriptPendingReasoning transcript)
            (Just (pendingContext, text : chunks))
      | otherwise -> do
          flushReasoning transcript
          writeIORef
            (transcriptPendingReasoning transcript)
            (Just (context, [text]))
    Nothing ->
      writeIORef
        (transcriptPendingReasoning transcript)
        (Just (context, [text]))

flushReasoning :: Transcript -> IO ()
flushReasoning transcript = do
  pending <- readIORef (transcriptPendingReasoning transcript)
  case pending of
    Nothing -> pure ()
    Just (context, chunks) -> do
      writeIORef (transcriptPendingReasoning transcript) Nothing
      writeEvent transcript (TranscriptReasoning context (concat (reverse chunks)))

writeEvent :: Transcript -> TranscriptEvent -> IO ()
writeEvent transcript event = do
  now <- getCurrentTime
  let at = formatTime defaultTimeLocale "%FT%T%QZ" now
  BL.hPut
    (transcriptJsonHandle transcript)
    (encodeTranscriptEvent at event)
  hFlush (transcriptJsonHandle transcript)
  hPutStr
    (transcriptMarkdownHandle transcript)
    (renderTranscriptEvent at event)
  hFlush (transcriptMarkdownHandle transcript)

-- | Encode one event as a JSON Lines record, including its trailing newline.
encodeTranscriptEvent :: String -> TranscriptEvent -> BL.ByteString
encodeTranscriptEvent at event =
  Aeson.encode (transcriptEventJSON at event) <> "\n"

transcriptEventJSON :: String -> TranscriptEvent -> Value
transcriptEventJSON at event =
  object
    ( [ "schema" .= (1 :: Int)
      , "at" .= at
      , "type" .= eventType event
      ]
        ++ eventFields event
    )

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
    TranscriptToolResult context callId name ok text ->
      [ "context" .= contextName context
      , "callId" .= callId
      , "name" .= name
      , "ok" .= ok
      , "text" .= text
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
    TranscriptToolResult context _ name ok text ->
      section
        (contextLabel context (name ++ if ok then " result" else " error"))
        text
    TranscriptDiagnostic text
      | all isSpace text -> ""
      | otherwise -> "\n" ++ blockquote text
    TranscriptInterrupted context ->
      "\n> " ++ contextLabel context "interrupted" ++ ".\n"
    -- The snapshot duplicates the Markdown above. It is the JSON Lines
    -- representation a later process uses to restore the conversation.
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

-- | Shift ATX headings outside fenced code blocks. Headings at level six stay
-- at level six because Markdown has no level seven.
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


