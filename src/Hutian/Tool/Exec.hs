-- | Terminal-side execution of model tool calls. Planning is pure and lives in
-- "Hutian.Tool"; this module only carries out the resulting plan.
module Hutian.Tool.Exec
  ( runToolCalls
  ) where

import Hutian.Console
  ( confirmToolCommand
  , consoleFlush
  , consoleIsErrorInteractive
  , consoleIsInteractive
  , consoleWrite
  , consoleWriteError
  , consoleWriteErrorLine
  , consoleWriteGrayLine
  , consoleWriteLine
  , consoleWriteToolCommand
  , consoleWriteToolLabel
  , printOutputLabel
  , withGrayOutput
  )
import Hutian.Conversation
  ( Conversation
  , ToolCall (..)
  , appendToolResult
  )
import Hutian.Highlight (sanitizeOutput)
import Hutian.Jobs
  ( Job
  , jobKillResult
  , jobListResult
  , jobOutputResult
  , jobOutputTruncationNote
  , jobStartedLine
  )
import Hutian.Jobs.Runtime
  ( JobRuntime
  , killJob
  , listJobs
  , readJobOutput
  , startJob
  )
import Hutian.Policy (ModelShell)
import Hutian.Render (OutputKind (..), ToolApproval (..), grayOutputText, trailingNewline)
import Hutian.Shell
  ( CommandResult (..)
  , commandResultText
  , runShellCommandOutput
  )
import Hutian.Tool
  ( CommandMode (..)
  , ToolPlan (..)
  , abortedToolMessage
  , planTool
  )

-- | Run each requested call in order. Once a call aborts the turn, the
-- remaining calls get an error result instead of running.
runToolCalls :: JobRuntime -> FilePath -> ModelShell -> Conversation -> [ToolCall] -> IO (Bool, Conversation)
runToolCalls jobs workingDirectory policy = go False
  where
    go aborted conversation [] = pure (aborted, conversation)
    go True conversation (call : rest) =
      go True (appendToolResult (toolCallId call) abortedToolMessage conversation) rest
    go False conversation (call : rest) = do
      (callAborted, conversation') <- runToolCall jobs workingDirectory policy conversation call
      go callAborted conversation' rest

runToolCall :: JobRuntime -> FilePath -> ModelShell -> Conversation -> ToolCall -> IO (Bool, Conversation)
runToolCall jobs workingDirectory policy conversation call =
  case planTool policy call of
    PlanRefuse message -> refuse message
    PlanRun mode command -> do
      consoleWriteToolCommand (toolCallName call) command
      runPlanned mode command
    PlanApprove mode command -> do
      approval <- confirmToolCommand (toolCallName call) command
      case approval of
        ToolApproved -> runPlanned mode command
        ToolDenied -> refuse "tool error: the user did not approve the shell command"
        ToolAborted ->
          pure (True, appendToolResult (toolCallId call) abortedToolMessage conversation)
    PlanListJobs -> runJobsTool
    PlanJobOutput target requestedLines -> runJobOutputTool target requestedLines
    PlanJobKill target -> runJobKillTool target
  where
    refuse message =
      pure (False, appendToolResult (toolCallId call) message conversation)

    runPlanned mode command =
      case mode of
        Foreground -> do
          conversation' <- executeShellTool workingDirectory conversation call command
          pure (False, conversation')
        Background -> do
          conversation' <- executeBackgroundTool jobs conversation call command
          pure (False, conversation')

    runJobsTool = do
      consoleWriteToolLabel "jobs"
      printOutputLabel ToolOutput
      current <- listJobs jobs
      printJobRows current
      pure (False, appendToolResult (toolCallId call) (jobListResult current) conversation)

    runJobOutputTool target requestedLines = do
      consoleWriteToolLabel "job_output"
      printOutputLabel ToolOutput
      result <- readJobOutput jobs target requestedLines
      case result of
        Left message -> do
          consoleWriteErrorLine message
          pure (False, appendToolResult (toolCallId call) message conversation)
        Right (job, output, truncated) -> do
          displayJobOutput output truncated
          pure
            ( False
            , appendToolResult (toolCallId call) (jobOutputResult job output truncated) conversation
            )

    runJobKillTool target = do
      consoleWriteToolLabel "job_kill"
      printOutputLabel ToolOutput
      result <- killJob jobs target
      case result of
        Left message -> do
          consoleWriteErrorLine message
          pure (False, appendToolResult (toolCallId call) message conversation)
        Right (job, wasRunning) -> do
          let message = jobKillResult wasRunning job
          consoleWriteGrayLine message
          pure (False, appendToolResult (toolCallId call) message conversation)

-- | Run a command the model is already allowed to execute. The caller is
-- responsible for showing the command first.
executeShellTool :: FilePath -> Conversation -> ToolCall -> String -> IO Conversation
executeShellTool workingDirectory conversation call command = do
  printOutputLabel ToolOutput
  result <- runShellCommandOutput workingDirectory command
  case result of
    Left message -> do
      consoleWriteErrorLine message
      pure (appendToolResult (toolCallId call) message conversation)
    Right output -> do
      displayCommandResult output
      pure (appendToolResult (toolCallId call) (commandResultText output) conversation)

-- | Start a command as a job and return its id to the model. The job's output
-- goes to its log, so the turn is not held open by a long-running process.
executeBackgroundTool :: JobRuntime -> Conversation -> ToolCall -> String -> IO Conversation
executeBackgroundTool jobs conversation call command = do
  printOutputLabel ToolOutput
  result <- startJob jobs command
  case result of
    Left message -> do
      consoleWriteErrorLine message
      pure (appendToolResult (toolCallId call) message conversation)
    Right job -> do
      consoleWriteGrayLine (jobStartedLine job)
      pure (appendToolResult (toolCallId call) (jobStartedLine job) conversation)

-- | Display captured command output the same way it would appear if the command
-- had inherited the terminal.
displayCommandResult :: CommandResult -> IO ()
displayCommandResult result = do
  stdoutInteractive <- consoleIsInteractive
  stderrInteractive <- consoleIsErrorInteractive
  let output = trailingNewline (sanitizeOutput (commandStdout result))
      errors = trailingNewline (sanitizeOutput (commandStderr result))
  consoleWrite (grayOutputText stdoutInteractive output)
  consoleWriteError (grayOutputText stderrInteractive errors)
  consoleFlush

-- | Print the job table in gray.
printJobRows :: [Job] -> IO ()
printJobRows current =
  withGrayOutput (mapM_ (consoleWriteLine . sanitizeOutput) (lines (jobListResult current)))

-- | Print a job log tail, marking a truncated window at the top.
displayJobOutput :: String -> Bool -> IO ()
displayJobOutput output truncated = do
  interactive <- consoleIsInteractive
  let note = if truncated then jobOutputTruncationNote ++ "\n" else ""
      body = if null output then "(no output)\n" else output
      text = trailingNewline (sanitizeOutput (note ++ body))
  consoleWrite (grayOutputText interactive text)
  consoleFlush
