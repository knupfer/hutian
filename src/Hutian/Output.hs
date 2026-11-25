{-# LANGUAGE RankNTypes #-}

-- | Session presentation. Tool execution and agent announcements talk to this
-- record instead of the console directly, so batch mode can ignore every
-- presentation event in one place. Diagnostics still use "Hutian.Console"
-- directly: they are not presentation and must reach stderr in every mode.
module Hutian.Output
  ( Output (..)
  , OutputEvent (..)
  , interactiveOutput
  , silentOutput
  ) where

import Control.Monad (unless)
import Hutian.Console
  ( confirmToolApproval
  , confirmToolCommandIn
  , consoleFlush
  , consoleIsErrorInteractive
  , consoleIsInteractive
  , consoleWrite
  , consoleWriteBytes
  , consoleWriteErrorBytes
  , consoleWriteGrayLine
  , consoleWriteHandlerPromptIn
  , consoleWriteLine
  , consoleWriteSubagentPrompt
  , consoleWriteSystemPrompt
  , consoleWriteToolCommandIn
  , consoleWriteToolDefinitions
  , consoleWriteToolLabelIn
  , consoleWriteToolTextIn
  , printOutputLabelIn
  , withGrayOutput
  , withSubagentCursor
  )
import Hutian.Context (TurnContext)
import Hutian.Highlight (sanitizeOutput, sanitizeOutputBytes)
import Hutian.Jobs
  ( Job
  , Truncation
  , jobListResult
  , jobOutputBlock
  )
import Hutian.Render
  ( OutputKind
  , ToolApproval (..)
  , grayOutputBytes
  , grayOutputText
  , trailingNewlineBytes
  )
import Hutian.Shell (CapturedOutput (..))

-- | Everything the agent and tool layer may present. An event carries no
-- decisions of its own; a mode that should stay silent simply ignores it.
data OutputEvent
  = PresentChunk String
  | PresentSystemPrompt TurnContext String
  | PresentToolDefinitions TurnContext [String]
  | PresentSubagentPrompt String String
  | PresentHandlerPrompt TurnContext String Bool String
  | PresentToolCommand TurnContext String String
  | PresentToolArguments TurnContext String String
  | PresentToolLabel TurnContext String
  | PresentResultLabel TurnContext String OutputKind
  | PresentGrayLine String
  | PresentCapturedOutput CapturedOutput
  | PresentTextBlock String
  | PresentJobList [Job]
  | PresentJobLog String Truncation

-- | The presentation and interaction capabilities a turn needs. Approval
-- questions are part of the capability rather than the caller's decision: a
-- silent output denies them without showing anything.
data Output = Output
  { outputPresent :: OutputEvent -> IO ()
  , outputSubagentCursor :: forall a. IO a -> IO a
  , outputConfirmToolCommand :: TurnContext -> String -> String -> IO ToolApproval
  , outputConfirmApproval :: IO ToolApproval
  }

-- | Draw every event on the terminal and ask the user for approvals.
interactiveOutput :: Output
interactiveOutput =
  Output
    { outputPresent = presentInteractive
    , outputSubagentCursor = withSubagentCursor
    , outputConfirmToolCommand = confirmToolCommandIn
    , outputConfirmApproval = confirmToolApproval
    }

-- | Ignore every event and deny approval questions. Used by one-turn batch
-- runs, where only the final answer is printed.
silentOutput :: Output
silentOutput =
  Output
    { outputPresent = const (pure ())
    , outputSubagentCursor = id
    , outputConfirmToolCommand = \_ _ _ -> pure ToolDenied
    , outputConfirmApproval = pure ToolDenied
    }

presentInteractive :: OutputEvent -> IO ()
presentInteractive event =
  case event of
    PresentChunk text ->
      unless (null text) (consoleWrite text >> consoleFlush)
    PresentSystemPrompt context prompt ->
      consoleWriteSystemPrompt context prompt
    PresentToolDefinitions context summaries ->
      consoleWriteToolDefinitions context summaries
    PresentSubagentPrompt label prompt ->
      consoleWriteSubagentPrompt label prompt
    PresentHandlerPrompt context label showBody prompt ->
      consoleWriteHandlerPromptIn context label showBody prompt
    PresentToolCommand context name command ->
      consoleWriteToolCommandIn context name command
    PresentToolArguments context name arguments ->
      consoleWriteToolTextIn context name arguments
    PresentToolLabel context name ->
      consoleWriteToolLabelIn context name
    PresentResultLabel context name kind ->
      printOutputLabelIn context name kind
    PresentGrayLine message ->
      consoleWriteGrayLine message
    PresentCapturedOutput captured ->
      displayCapturedOutput captured
    PresentTextBlock text ->
      withGrayOutput (consoleWrite (sanitizeOutput text) >> consoleFlush)
    PresentJobList current ->
      withGrayOutput
        (mapM_ (consoleWriteLine . sanitizeOutput) (lines (jobListResult current)))
    PresentJobLog output truncation ->
      displayJobOutput output truncation

-- | Display captured command output the same way it would appear if the command
-- had inherited the terminal. Bytes are sanitized and written directly, so a
-- large result is not copied through 'String'.
displayCapturedOutput :: CapturedOutput -> IO ()
displayCapturedOutput captured = do
  stdoutInteractive <- consoleIsInteractive
  stderrInteractive <- consoleIsErrorInteractive
  consoleWriteBytes
    ( grayOutputBytes
        stdoutInteractive
        (trailingNewlineBytes (sanitizeOutputBytes (capturedStdout captured)))
    )
  consoleWriteErrorBytes
    ( grayOutputBytes
        stderrInteractive
        (trailingNewlineBytes (sanitizeOutputBytes (capturedStderr captured)))
    )
  consoleFlush

-- | Print a job log tail, marking a truncated window at the top.
displayJobOutput :: String -> Truncation -> IO ()
displayJobOutput output truncation = do
  interactive <- consoleIsInteractive
  consoleWrite
    (grayOutputText interactive (sanitizeOutput (jobOutputBlock output truncation)))
  consoleFlush
