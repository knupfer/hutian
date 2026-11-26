{-# LANGUAGE OverloadedStrings #-}

-- | Terminal-side execution of model tool calls. Planning is pure and lives in
-- "Hutian.Tool"; this module only carries out the resulting plan.
module Hutian.Tool.Exec
  ( ToolRuntime (..)
  , ToolRun (..)
  , runToolCalls
  , runToolCallsWith
  ) where

import Hutian.Console (consoleWriteErrorLine)
import Hutian.Conversation
  ( Conversation
  , ToolCall (..)
  , abortedToolMessage
  , appendToolResult
  )
import Control.Monad (when)
import Hutian.Context (TurnContext (MainContext))
import Hutian.Json (jsonText)
import Hutian.Jobs
  ( jobKillResult
  , jobListResult
  , jobOutputResult
  , jobStartedLine
  )
import Hutian.Jobs.Runtime
  ( JobRuntime
  , killJob
  , listJobs
  , readJobOutput
  , startJob
  )
import Hutian.Mcp (McpSessionCache, callMcpToolWithSessions, newMcpSessionCache)
import Hutian.Mcp.Protocol (mcpErrorText, mcpToolDisplayName)
import Hutian.Output
  ( Output (..)
  , OutputEvent (..)
  , interactiveOutput
  )
import Hutian.Policy (ToolPolicy)
import Hutian.Render
  ( OutputKind (..)
  , ToolApproval (..)
  , trailingNewline
  )
import Hutian.Shell
  ( capturedOutputText
  , runShellCommandCaptured
  )
import Hutian.Tool
  ( Approval (..)
  , CommandMode (..)
  , ToolAction (..)
  , ToolPlan (..)
  , ToolRun (..)
  , planTool
  , planToolWith
  )
import Hutian.Tool.Catalog
  ( McpTool (..)
  , ToolCatalog
  , mcpServerHandlersOf
  , mcpServerNameOf
  )
import Hutian.Tool.Config
  ( HandlerConfig
  , SubagentConfig (..)
  , handlerConfigFor
  )
import Hutian.Transcript
  ( Transcript
  , TranscriptEvent (..)
  , recordTranscriptMaybe
  )
import Hutian.Web (fetchResultText, fetchUrl)
import Hutian.Verbosity
  ( RenderMode (..)
  , Verbosity (..)
  , outputBodyVisible
  , renderMode
  )

-- | The effects a tool run can reach. 'toolRuntimeContext' selects the main or
-- subagent palette. The callbacks keep the executor independent of the model
-- loop that runs a subagent or a tool-result handler.
data ToolRuntime = ToolRuntime
  { toolRuntimeJobs :: JobRuntime
  , toolRuntimeWorkingDirectory :: FilePath
  , toolRuntimeContext :: TurnContext
  , toolRuntimeVerbosity :: Verbosity
  -- | How calls, results, and approval questions are presented.
  , toolRuntimeOutput :: Output
  , toolRuntimeMcpSessions :: McpSessionCache
  -- | Structured recording, independent of what the terminal displays.
  , toolRuntimeTranscript :: Maybe Transcript
  , toolRuntimeSubagent :: SubagentConfig -> String -> IO (Either String String)
  , toolRuntimeHandler :: String -> String -> HandlerConfig -> String -> IO (Either String String)
  }

-- | Run each requested call in order with a shell-only policy. This preserves
-- the shell tool entry point used by tests and direct callers.
runToolCalls :: JobRuntime -> FilePath -> ToolPolicy -> Conversation -> [ToolCall] -> IO (ToolRun, Conversation)
runToolCalls jobs workingDirectory policy conversation calls = do
  mcpSessions <- newMcpSessionCache
  runToolCallsPlanned
    (planTool policy)
    ToolRuntime
      { toolRuntimeJobs = jobs
      , toolRuntimeWorkingDirectory = workingDirectory
      , toolRuntimeContext = MainContext
      , toolRuntimeVerbosity = VerbosityHigh
      , toolRuntimeOutput = interactiveOutput
      , toolRuntimeMcpSessions = mcpSessions
      , toolRuntimeTranscript = Nothing
      , toolRuntimeSubagent = \_ _ ->
          pure (Left "tool error: the model subagent tool is disabled")
      , toolRuntimeHandler = \_ _ _ _ ->
          pure (Left "tool error: the model result handler is disabled")
      }
    conversation
    calls

-- | Run each requested call in order using the full tool configuration. Once a
-- call aborts the turn, the remaining calls get an error result instead of
-- running.
runToolCallsWith :: ToolRuntime -> ToolCatalog -> Conversation -> [ToolCall] -> IO (ToolRun, Conversation)
runToolCallsWith runtime catalog =
  runToolCallsPlanned (planToolWith catalog) runtime

recordTool :: ToolRuntime -> TranscriptEvent -> IO ()
recordTool runtime = recordTranscriptMaybe (toolRuntimeTranscript runtime)

-- | The human-facing tool name. MCP calls use the dotted display name in the
-- transcript, matching the handler and subagent context paths.
planDisplayName :: ToolPlan -> ToolCall -> String
planDisplayName plan call =
  case plan of
    PlanExecute _ (CallMcp server tool _) ->
      mcpToolDisplayName (mcpServerNameOf server) (mcpToolName tool)
    _ -> toolCallName call

runToolCallsPlanned
  :: (ToolCall -> ToolPlan)
  -> ToolRuntime
  -> Conversation
  -> [ToolCall]
  -> IO (ToolRun, Conversation)
runToolCallsPlanned planner runtime = go ToolsCompleted
  where
    go status conversation [] = pure (status, conversation)
    go ToolsAborted conversation (call : rest) = do
      let displayName = planDisplayName (planner call) call
      recordTool runtime
        (TranscriptToolRequest (toolRuntimeContext runtime) displayName call)
      recordTool runtime
        ( TranscriptToolResult
            (toolRuntimeContext runtime)
            (toolCallId call)
            displayName
            False
            abortedToolMessage
        )
      go ToolsAborted (appendToolResult (toolCallId call) abortedToolMessage conversation) rest
    go ToolsCompleted conversation (call : rest) = do
      (status, conversation') <- runToolCall planner runtime conversation call
      go status conversation' rest

runToolCall
  :: (ToolCall -> ToolPlan)
  -> ToolRuntime
  -> Conversation
  -> ToolCall
  -> IO (ToolRun, Conversation)
runToolCall planner runtime conversation call = do
  recordTool runtime (TranscriptToolRequest context transcriptName call)
  case plan of
    PlanRefuse message -> refuse message
    PlanExecute approval action -> do
      decision <-
        case approval of
          RunNow -> describeAction action >> pure ToolApproved
          AskUser -> confirmAction action
      -- Only an interactive approval has a decision worth recording. RunNow is
      -- the policy's automatic path, not a user response.
      case approval of
        AskUser ->
          recordTool runtime (TranscriptToolDecision context (toolCallId call) decision)
        RunNow -> pure ()
      case decision of
        ToolApproved -> executeAction action
        ToolDenied -> refuse (deniedMessage action)
        ToolAborted -> aborted
  where
    plan = planner call
    transcriptName = planDisplayName plan call
    jobs = toolRuntimeJobs runtime
    workingDirectory = toolRuntimeWorkingDirectory runtime
    context = toolRuntimeContext runtime
    output = toolRuntimeOutput runtime
    toolOutput =
      modeToolOutput (renderMode (toolRuntimeVerbosity runtime) context)
    showOutputBody = outputBodyVisible toolOutput
    present = outputPresent output

    refuse message = do
      recordToolResult False message
      pure (ToolsCompleted, appendToolResult (toolCallId call) message conversation)

    aborted = do
      recordToolResult False abortedToolMessage
      pure (ToolsAborted, appendToolResult (toolCallId call) abortedToolMessage conversation)

    finish :: Either String String -> IO (ToolRun, Conversation)
    finish = either finishError finishRaw

    finishError message = do
      consoleWriteErrorLine message
      recordToolResult False message
      pure (ToolsCompleted, appendToolResult (toolCallId call) message conversation)

    finishRaw raw = do
      recordToolResult True raw
      pure (ToolsCompleted, appendToolResult (toolCallId call) raw conversation)

    recordToolResult ok message =
      recordTool runtime
        ( TranscriptToolResult
            context
            (toolCallId call)
            transcriptName
            ok
            message
        )

    finishResult handlerConfig serverName toolName raw =
      case handlerConfig of
        Nothing -> finishRaw raw
        Just config -> do
          result <-
            outputSubagentCursor output
              (toolRuntimeHandler runtime serverName toolName config raw)
          case result of
            Left message -> finishError message
            Right answer ->
              pure (ToolsCompleted, appendToolResult (toolCallId call) answer conversation)

    -- Show a call before it runs. Approval-capable actions show their own label
    -- inside 'confirmAction', so this only handles the immediate path.
    describeAction action =
      case action of
        RunShell _ command ->
          present (PresentToolCommand context (toolCallName call) command)
        FetchUrl url ->
          present (PresentToolCommand context "webfetch" url)
        CallMcp server tool arguments ->
          showMcpCall server tool arguments
        ListJobs -> present (PresentToolLabel context "jobs")
        ReadJobOutput {} -> present (PresentToolLabel context "job_output")
        KillJob {} -> present (PresentToolLabel context "job_kill")
        RunSubagent {} -> pure ()

    confirmAction action =
      case action of
        RunShell _ command ->
          outputConfirmToolCommand output context (toolCallName call) command
        FetchUrl url ->
          outputConfirmToolCommand output context "webfetch" url
        CallMcp server tool arguments -> do
          showMcpCall server tool arguments
          outputConfirmApproval output
        KillJob {} -> do
          present (PresentToolLabel context "job_kill")
          outputConfirmApproval output
        _ -> pure ToolApproved

    deniedMessage action =
      case action of
        RunShell {} -> "tool error: the user did not approve the shell command"
        FetchUrl {} -> "tool error: the user did not approve the webfetch"
        CallMcp {} -> "tool error: the user did not approve the MCP tool call"
        KillJob {} -> "tool error: the user did not approve stopping the job"
        _ -> "tool error: the user did not approve the tool call"

    executeAction action =
      case action of
        RunShell mode command -> runPlanned mode command
        ListJobs -> runJobsTool
        ReadJobOutput target requestedLines -> runJobOutputTool target requestedLines
        KillJob target -> runJobKillTool target
        FetchUrl url -> runWebfetchTool url
        RunSubagent agent prompt -> runSubagentTool agent prompt
        CallMcp server tool arguments -> runMcpTool server tool arguments

    runPlanned mode command =
      case mode of
        Foreground -> do
          present (PresentResultLabel context (toolCallName call) ToolOutput)
          finish
            =<< executeShellTool
              output
              (outputBodyVisible toolOutput)
              workingDirectory
              command
        Background -> do
          present (PresentResultLabel context (toolCallName call) ToolOutput)
          result <- startJob jobs command
          case result of
            Left message -> finishError message
            Right job -> do
              let message = jobStartedLine job
              when showOutputBody (present (PresentGrayLine message))
              finishRaw message

    runJobsTool = do
      present (PresentToolLabel context "jobs")
      present (PresentResultLabel context "jobs" ToolOutput)
      current <- listJobs jobs
      when showOutputBody (present (PresentJobList current))
      finishRaw (jobListResult current)

    runJobOutputTool target requestedLines = do
      present (PresentToolLabel context "job_output")
      present (PresentResultLabel context "job_output" ToolOutput)
      result <- readJobOutput jobs target requestedLines
      case result of
        Left message -> finishError message
        Right (job, output', truncation) -> do
          when showOutputBody (present (PresentJobLog output' truncation))
          finishRaw (jobOutputResult job output' truncation)

    runJobKillTool target = do
      present (PresentToolLabel context "job_kill")
      present (PresentResultLabel context "job_kill" ToolOutput)
      result <- killJob jobs target
      case result of
        Left message -> finishError message
        Right (job, activity) -> do
          let message = jobKillResult activity job
          when showOutputBody (present (PresentGrayLine message))
          finishRaw message

    runWebfetchTool url = do
      present (PresentResultLabel context "webfetch" ToolOutput)
      result <- fetchUrl url
      case result of
        Left message -> finishError message
        Right fetched -> do
          let text = fetchResultText fetched
          when showOutputBody (present (PresentTextBlock text))
          finishRaw text

    runSubagentTool agent prompt = do
      result <- outputSubagentCursor output (toolRuntimeSubagent runtime agent prompt)
      finish result

    runMcpTool server tool arguments = do
      let localName = mcpToolName tool
          serverName = mcpServerNameOf server
          displayName = mcpToolDisplayName serverName localName
          handlerConfig =
            handlerConfigFor localName (mcpServerHandlersOf server)
      result <-
        callMcpToolWithSessions
          (toolRuntimeMcpSessions runtime)
          server
          tool
          arguments
      case result of
        Left failure -> finishError (mcpErrorText failure)
        Right text -> do
          let formatted = trailingNewline text
          case handlerConfig of
            Nothing -> do
              present (PresentResultLabel context displayName ToolOutput)
              when showOutputBody (present (PresentTextBlock formatted))
            Just _ -> pure ()
          finishResult handlerConfig serverName localName formatted

    showMcpCall server tool arguments =
      present
        ( PresentToolArguments
            context
            (mcpToolDisplayName (mcpServerNameOf server) (mcpToolName tool))
            (jsonText arguments)
        )

-- | Run a command the model is already allowed to execute and display it.
executeShellTool
  :: Output
  -> Bool
  -> FilePath
  -> String
  -> IO (Either String String)
executeShellTool output showOutput workingDirectory command = do
  result <- runShellCommandCaptured workingDirectory command
  case result of
    Left message -> pure (Left message)
    Right captured -> do
      when showOutput (outputPresent output (PresentCapturedOutput captured))
      pure (Right (capturedOutputText captured))
