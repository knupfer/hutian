-- | Terminal-side execution of model tool calls. Planning is pure and lives in
-- "Hutian.Tool"; this module only carries out the resulting plan.
module Hutian.Tool.Exec
  ( ToolRuntime (..)
  , runToolCalls
  , runToolCallsWith
  ) where

import Hutian.Console
  ( confirmToolCommandIn
  , consoleFlush
  , consoleIsErrorInteractive
  , consoleIsInteractive
  , consoleWrite
  , consoleWriteError
  , consoleWriteErrorLine
  , consoleWriteGrayLine
  , consoleWriteLine
  , consoleWriteSubagentPrompt
  , consoleWriteToolCommandIn
  , consoleWriteToolLabelIn
  , printOutputLabelIn
  , withGrayOutput
  , withSubagentCursor
  )
import Hutian.Conversation
  ( Conversation
  , ToolCall (..)
  , abortedToolMessage
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
import Hutian.Policy (ToolPolicy)
import Hutian.Render
  ( OutputKind (..)
  , ToolApproval (..)
  , TurnContext (..)
  , grayOutputText
  , trailingNewline
  )
import Hutian.Shell
  ( CommandResult (..)
  , commandResultText
  , runShellCommandOutput
  )
import Hutian.Tool
  ( CommandMode (..)
  , ToolPlan (..)
  , planTool
  , planToolWith
  )
import Hutian.Tool.Config
  ( SubagentConfig (..)
  , ToolConfig (..)
  , WebsearchConfig (..)
  )
import Hutian.Web (fetchResultText, fetchUrl, websearchRequestUrl)

-- | The effects a tool run can reach. 'toolRuntimeContext' selects the main or
-- subagent palette. 'toolRuntimeSubagent' is a callback so the executor stays
-- independent of the model loop that runs a subagent.
data ToolRuntime = ToolRuntime
  { toolRuntimeJobs :: JobRuntime
  , toolRuntimeWorkingDirectory :: FilePath
  , toolRuntimeContext :: TurnContext
  , toolRuntimeSubagent :: SubagentConfig -> String -> IO (Either String String)
  }

-- | Run each requested call in order with a shell-only policy. This preserves
-- the shell tool entry point used by tests and direct callers.
runToolCalls :: JobRuntime -> FilePath -> ToolPolicy -> Conversation -> [ToolCall] -> IO (Bool, Conversation)
runToolCalls jobs workingDirectory policy =
  runToolCallsPlanned
    (planTool policy)
    ToolRuntime
      { toolRuntimeJobs = jobs
      , toolRuntimeWorkingDirectory = workingDirectory
      , toolRuntimeContext = MainContext
      , toolRuntimeSubagent = \_ _ ->
          pure (Left "tool error: the model subagent tool is disabled")
      }

-- | Run each requested call in order using the full tool configuration. Once a
-- call aborts the turn, the remaining calls get an error result instead of
-- running.
runToolCallsWith :: ToolRuntime -> ToolConfig -> Conversation -> [ToolCall] -> IO (Bool, Conversation)
runToolCallsWith runtime config =
  runToolCallsPlanned (planToolWith config) runtime

runToolCallsPlanned
  :: (ToolCall -> ToolPlan)
  -> ToolRuntime
  -> Conversation
  -> [ToolCall]
  -> IO (Bool, Conversation)
runToolCallsPlanned planner runtime = go False
  where
    go aborted conversation [] = pure (aborted, conversation)
    go True conversation (call : rest) =
      go True (appendToolResult (toolCallId call) abortedToolMessage conversation) rest
    go False conversation (call : rest) = do
      (callAborted, conversation') <- runToolCall planner runtime conversation call
      go callAborted conversation' rest

runToolCall
  :: (ToolCall -> ToolPlan)
  -> ToolRuntime
  -> Conversation
  -> ToolCall
  -> IO (Bool, Conversation)
runToolCall planner runtime conversation call =
  case planner call of
    PlanRefuse message -> refuse message
    PlanRun mode command -> do
      consoleWriteToolCommandIn context (toolCallName call) command
      runPlanned mode command
    PlanApprove mode command -> do
      approval <- confirmToolCommandIn context (toolCallName call) command
      case approval of
        ToolApproved -> runPlanned mode command
        ToolDenied -> refuse "tool error: the user did not approve the shell command"
        ToolAborted ->
          pure (True, appendToolResult (toolCallId call) abortedToolMessage conversation)
    PlanListJobs -> runJobsTool
    PlanJobOutput target requestedLines -> runJobOutputTool target requestedLines
    PlanJobKill target -> runJobKillTool target
    PlanRunWebfetch url -> do
      consoleWriteToolCommandIn context "webfetch" url
      runWebfetchTool url
    PlanApproveWebfetch url -> do
      approval <- confirmToolCommandIn context "webfetch" url
      case approval of
        ToolApproved -> runWebfetchTool url
        ToolDenied -> refuse "tool error: the user did not approve the webfetch"
        ToolAborted -> aborted
    PlanRunWebsearch configured query -> do
      consoleWriteToolCommandIn context "websearch" query
      runWebsearchTool configured query
    PlanApproveWebsearch configured query -> do
      approval <- confirmToolCommandIn context "websearch" query
      case approval of
        ToolApproved -> runWebsearchTool configured query
        ToolDenied -> refuse "tool error: the user did not approve the websearch"
        ToolAborted -> aborted
    PlanSubagent agent prompt -> runSubagentTool agent prompt
  where
    jobs = toolRuntimeJobs runtime
    workingDirectory = toolRuntimeWorkingDirectory runtime
    context = toolRuntimeContext runtime

    refuse message =
      pure (False, appendToolResult (toolCallId call) message conversation)

    aborted =
      pure (True, appendToolResult (toolCallId call) abortedToolMessage conversation)

    runPlanned mode command =
      case mode of
        Foreground -> do
          conversation' <- executeShellTool context workingDirectory conversation call command
          pure (False, conversation')
        Background -> do
          conversation' <- executeBackgroundTool context jobs conversation call command
          pure (False, conversation')

    runJobsTool = do
      consoleWriteToolLabelIn context "jobs"
      printOutputLabelIn context ToolOutput
      current <- listJobs jobs
      printJobRows current
      pure (False, appendToolResult (toolCallId call) (jobListResult current) conversation)

    runJobOutputTool target requestedLines = do
      consoleWriteToolLabelIn context "job_output"
      printOutputLabelIn context ToolOutput
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
      consoleWriteToolLabelIn context "job_kill"
      printOutputLabelIn context ToolOutput
      result <- killJob jobs target
      case result of
        Left message -> do
          consoleWriteErrorLine message
          pure (False, appendToolResult (toolCallId call) message conversation)
        Right (job, wasRunning) -> do
          let message = jobKillResult wasRunning job
          consoleWriteGrayLine message
          pure (False, appendToolResult (toolCallId call) message conversation)

    runWebfetchTool url = do
      printOutputLabelIn context ToolOutput
      result <- fetchUrl url
      case result of
        Left message -> do
          consoleWriteErrorLine message
          pure (False, appendToolResult (toolCallId call) message conversation)
        Right fetched -> do
          let text = fetchResultText fetched
          displayWebText text
          pure (False, appendToolResult (toolCallId call) text conversation)

    runWebsearchTool configured query = do
      let url = websearchRequestUrl (websearchUrl configured) query
      runWebfetchTool url

    runSubagentTool agent prompt = do
      consoleWriteSubagentPrompt (subagentName agent) prompt
      result <- withSubagentCursor (toolRuntimeSubagent runtime agent prompt)
      case result of
        Left message -> do
          consoleWriteErrorLine message
          pure (False, appendToolResult (toolCallId call) message conversation)
        Right answer ->
          pure (False, appendToolResult (toolCallId call) answer conversation)

-- | Run a command the model is already allowed to execute. The caller is
-- responsible for showing the command first.
executeShellTool :: TurnContext -> FilePath -> Conversation -> ToolCall -> String -> IO Conversation
executeShellTool context workingDirectory conversation call command = do
  printOutputLabelIn context ToolOutput
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
executeBackgroundTool :: TurnContext -> JobRuntime -> Conversation -> ToolCall -> String -> IO Conversation
executeBackgroundTool context jobs conversation call command = do
  printOutputLabelIn context ToolOutput
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

-- | Print a fetched page or search result in the same muted style as other
-- tool output.
displayWebText :: String -> IO ()
displayWebText text =
  withGrayOutput (consoleWrite (sanitizeOutput text) >> consoleFlush)

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
