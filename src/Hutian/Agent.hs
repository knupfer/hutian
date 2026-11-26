-- | Model turns, tool execution, subagents, and terminal input. The composition
-- root in "Main" creates one 'RuntimeEnv' and passes it to every entry point,
-- instead of threading the working directory, job runtime, settings, and
-- interrupt flag through each function.
module Hutian.Agent
  ( RuntimeEnv (..)
  , discoverMcpSettings
  , runInput
  , runBatchInput
  ) where

import Control.Monad (unless, when)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Hutian.Command (TerminalInput (..), classifyTerminalInput)
import Hutian.Console
  ( consoleIsInteractive
  , consoleWriteErrorLine
  , printOutputLabelIn
  , withGrayOutput
  , withRespondingCursor
  , withoutEcho
  )
import Hutian.Context
  ( TurnContext (MainContext)
  , askLabel
  , subagentContext
  )
import Hutian.Conversation
  ( Conversation
  , appendResult
  , appendTerminal
  , emptyConversation
  )
import Hutian.Editor (InputMode (..))
import Hutian.Highlight (sanitizeOutputBytes)
import Hutian.Interrupt (InterruptFlag)
import Hutian.Jobs.Runtime (JobRuntime)
import Hutian.Llm
  ( LlmOutput (..)
  , LlmResult (..)
  , LlmSettings (..)
  , ModelName (..)
  , SystemPrompt (..)
  , runLlmContinuation
  , runLlmWithTools
  )
import Hutian.Llm.Loop (LlmLoop (..), runToolLoopWithResult)
import Hutian.Mcp (McpSessionCache, discoverMcpServers)
import Hutian.Mcp.Protocol (mcpHandlerName)
import Hutian.Output (Output (..), OutputEvent (..))
import Hutian.Render (OutputKind (..))
import Hutian.Shell
  ( runShellCommand
  , runShellCommandStreaming
  )
import Hutian.Tool (toolDefinitionsFor)
import Hutian.Tool.Catalog
  ( ToolCatalog
  , catalogMcp
  , mcpServerConfig
  , toolCatalog
  , withMcpServers
  )
import Hutian.Tool.Config
  ( AgentConfig (..)
  , SubagentConfig (..)
  , ToolConfig (..)
  )
import Hutian.Tool.Exec (ToolRuntime (..), runToolCallsWith)
import Hutian.Tool.Summary (toolSummaryLines)
import Hutian.Transcript (TranscriptEvent (..))
import Hutian.Transcript.Recorder
  ( Transcript
  , recordTranscriptMaybe
  )
import Hutian.Verbosity
  ( Verbosity (..)
  , handlerBodyVisible
  )
import System.IO (stderr, stdout)

-- | Everything a model turn needs. Constructing it once in "Main" keeps the
-- signatures below small and makes the shared state explicit.
data RuntimeEnv = RuntimeEnv
  { envWorkingDirectory :: FilePath
  , envJobs :: JobRuntime
  , envSettings :: LlmSettings
  , envInterrupt :: InterruptFlag
  , envMcpSessions :: McpSessionCache
  -- | Structured session recording, if the user requested a transcript.
  , envTranscript :: Maybe Transcript
  -- | How turns present their activity. Batch runs use the silent output.
  , envOutput :: Output
  }

-- | How an agent's prompt is announced. Result handlers use tool-output
-- styling and show their body only at high verbosity because it is the raw
-- tool result.
data PromptPresentation
  = SubagentPromptPresentation
  | HandlerPromptPresentation Bool

-- | Discover every MCP server in a catalog and replace its configured entries
-- with the discovery results.
discoverCatalog :: ToolCatalog -> IO ToolCatalog
discoverCatalog catalog = do
  servers <-
    discoverMcpServers
      consoleWriteErrorLine
      (map mcpServerConfig (catalogMcp catalog))
  pure (withMcpServers servers catalog)

-- | Discover the MCP tools configured by a tool configuration.
discoverTools :: ToolConfig -> IO ToolCatalog
discoverTools = discoverCatalog . toolCatalog

-- | Replace the settings' catalog with one whose MCP servers are discovered.
discoverMcpSettings :: LlmSettings -> IO LlmSettings
discoverMcpSettings settings = do
  tools <- discoverCatalog (settingTools settings)
  pure settings {settingTools = tools}

runInput :: RuntimeEnv -> InputMode -> String -> Conversation -> IO Conversation
runInput env mode text conversation =
  case mode of
    LlmInput -> do
      unless (all isSpace text) $
        recordEvent env (TranscriptInput LlmInput text)
      runLlmInput env conversation text
    TermInput ->
      runTerminalInput
        (envWorkingDirectory env)
        (envTranscript env)
        conversation
        text

recordEvent :: RuntimeEnv -> TranscriptEvent -> IO ()
recordEvent env = recordTranscriptMaybe (envTranscript env)

-- | Run a prompt through the model, executing any tools it requests and
-- feeding the results back until it produces a final answer.
runLlmInput :: RuntimeEnv -> Conversation -> String -> IO Conversation
runLlmInput env conversation prompt =
  withoutEcho $ withRespondingCursor $ do
    let runtime = toolRuntime env MainContext
    fst
      <$> runModelTurn
        (envInterrupt env)
        (envSettings env)
        runtime
        conversation
        prompt

-- | Run one non-interactive turn and return only the main agent's final answer.
-- The runtime suppresses reasoning, tool calls, and tool output, while errors
-- still reach stderr.
runBatchInput :: RuntimeEnv -> String -> IO (Either String String)
runBatchInput env prompt = do
  unless (all isSpace prompt) $
    recordEvent env (TranscriptInput LlmInput prompt)
  let runtime = toolRuntime env MainContext
  (conversation, result) <-
    runModelTurn
      (envInterrupt env)
      (envSettings env)
      runtime
      emptyConversation
      prompt
  recordEvent env (TranscriptConversationSnapshot conversation)
  pure (finalAnswer "hutian: the model returned no answer" result)

-- | The answer text of a finished turn, or the caller's message when the model
-- produced none.
finalAnswer :: String -> LlmResult -> Either String String
finalAnswer missing result =
  case llmResponseText result of
    Just answer -> Right answer
    Nothing -> Left missing

-- | Send one prompt, then run the tool/continuation loop until the model
-- answers. Returns the updated conversation and the result that ended the turn.
-- The runtime context selects the output palette, so the initial request and
-- every continuation always agree.
runModelTurn
  :: InterruptFlag
  -> LlmSettings
  -> ToolRuntime
  -> Conversation
  -> String
  -> IO (Conversation, LlmResult)
runModelTurn interrupt settings runtime conversation prompt = do
  let output = llmOutputFor runtime
      transcript = toolRuntimeTranscript runtime
      context = toolRuntimeContext runtime
      recordAssistant result =
        case (llmResponseText result, llmToolCalls result) of
          (Nothing, []) -> pure ()
          (answer, calls) ->
            recordTranscriptMaybe
              transcript
              (TranscriptAssistant context (fromMaybe "" answer) calls)
      loop =
        LlmLoop
          { loopRunTools = runToolCallsWith runtime (settingTools settings)
          , loopContinue = \current -> do
              next <- runLlmContinuation output interrupt settings current
              recordAssistant next
              pure next
          }
  result <-
    runLlmWithTools output interrupt settings conversation prompt
  recordAssistant result
  runToolLoopWithResult
    loop
    interrupt
    (appendResult (Just prompt) (llmResponseText result) (llmToolCalls result) conversation)
    result

-- | The runtime for one model turn. Subagents reuse the same API, settings,
-- job runtime, and interrupt flag, but substitute their own model, system
-- prompt, and tools. They run synchronously inside the tool call, so only the
-- final answer is appended to the caller's conversation.
toolRuntime :: RuntimeEnv -> TurnContext -> ToolRuntime
toolRuntime env context =
  ToolRuntime
    { toolRuntimeJobs = envJobs env
    , toolRuntimeWorkingDirectory = envWorkingDirectory env
    , toolRuntimeContext = context
    , toolRuntimeVerbosity = settingVerbosity (envSettings env)
    , toolRuntimeOutput = envOutput env
    , toolRuntimeMcpSessions = envMcpSessions env
    , toolRuntimeTranscript = envTranscript env
    , toolRuntimeSubagent = runSubagent env context
    , toolRuntimeHandler = runResultHandler env context
    }

runSubagent
  :: RuntimeEnv
  -> TurnContext
  -> SubagentConfig
  -> String
  -> IO (Either String String)
runSubagent env context config =
  runAgent
    env
    context
    SubagentPromptPresentation
    (subagentName config)
    (subagentAgent config)

-- | Run a tool-result handler. The handler receives only the tool result as its
-- prompt, and only its final answer is returned. Its own tool results use its
-- own handlers, never the caller's.
runResultHandler
  :: RuntimeEnv
  -> TurnContext
  -> String
  -> String
  -> AgentConfig
  -> String
  -> IO (Either String String)
runResultHandler env parent serverName localName =
  runAgent
    env
    parent
    (HandlerPromptPresentation displayedOutput)
    (mcpHandlerName serverName localName)
  where
    displayedOutput =
      handlerBodyVisible (settingVerbosity (envSettings env))

runAgent
  :: RuntimeEnv
  -> TurnContext
  -> PromptPresentation
  -> String
  -> AgentConfig
  -> String
  -> IO (Either String String)
runAgent env parent presentation agentName agent prompt = do
  tools <- discoverTools (agentTools agent)
  let childContext = subagentContext parent agentName
      subagentSettings =
        (envSettings env)
          { settingModel = ModelName (agentModel agent)
          , settingSystemPrompt = SystemPrompt (agentSystemPrompt agent)
          , settingTools = tools
          }
      childEnv = env {envSettings = subagentSettings}
      runtime = toolRuntime childEnv childContext
      output = toolRuntimeOutput runtime
  when (settingVerbosity (envSettings env) == VerbosityHigh) $ do
    outputPresent
      output
      (PresentSystemPrompt childContext (agentSystemPrompt agent))
    outputPresent
      output
      ( PresentToolDefinitions
          childContext
          (toolSummaryLines (toolDefinitionsFor (settingTools subagentSettings)))
      )
  case presentation of
    SubagentPromptPresentation ->
      outputPresent output (PresentSubagentPrompt (askLabel childContext) prompt)
    HandlerPromptPresentation showBody ->
      -- The handler runs as a subagent, so its label uses the child palette.
      outputPresent
        output
        (PresentHandlerPrompt childContext (askLabel childContext) showBody prompt)
  recordTranscriptMaybe
    (envTranscript env)
    (TranscriptAgentPrompt childContext (askLabel childContext) prompt)
  (_, finalResult) <-
    runModelTurn
      (envInterrupt childEnv)
      subagentSettings
      runtime
      emptyConversation
      prompt
  pure (finalAnswer "tool error: the subagent returned no answer" finalResult)

-- | The output sink for one runtime, so the initial request and every
-- continuation use the same palette and verbosity. The presentation event is
-- routed through the runtime's output, which drops it in batch mode.
llmOutputFor :: ToolRuntime -> LlmOutput
llmOutputFor runtime =
  LlmOutput
    { llmOutputWrite =
        outputPresent (toolRuntimeOutput runtime) . PresentChunk
    , llmOutputErrorLine = \message -> do
        recordTranscriptMaybe
          (toolRuntimeTranscript runtime)
          (TranscriptDiagnostic message)
        consoleWriteErrorLine message
    , llmOutputContext = toolRuntimeContext runtime
    , llmOutputVerbosity = toolRuntimeVerbosity runtime
    , llmOutputTranscript = toolRuntimeTranscript runtime
    }

runTerminalInput :: FilePath -> Maybe Transcript -> Conversation -> String -> IO Conversation
runTerminalInput workingDirectory transcript conversation text =
  case classifyTerminalInput text of
    NoTerminalInput -> pure conversation
    SingleLineInput line -> runTerminalScript workingDirectory transcript conversation line
    MultiLineInput script -> runTerminalScript workingDirectory transcript conversation script

runTerminalScript :: FilePath -> Maybe Transcript -> Conversation -> String -> IO Conversation
runTerminalScript workingDirectory transcript conversation command = do
  recordTranscriptMaybe transcript (TranscriptTerminalCommand command)
  printOutputLabelIn MainContext "shell" TermOutput
  withGrayOutput (runStreamingCommand transcript workingDirectory command)
  pure (appendTerminal command conversation)

-- | Run a command the user typed. A terminal gets inherited stdio so
-- full-screen programs work; otherwise output is streamed through the
-- sanitizer so escape sequences do not reach logs.
runStreamingCommand :: Maybe Transcript -> FilePath -> String -> IO ()
runStreamingCommand transcript workingDirectory command = do
  interactive <- consoleIsInteractive
  result <-
    if interactive
      then runShellCommand workingDirectory command
      else
        runShellCommandStreaming
          sanitizeOutputBytes
          stdout
          stderr
          workingDirectory
          command
  case result of
    Left message -> do
      recordTranscriptMaybe transcript (TranscriptDiagnostic message)
      consoleWriteErrorLine message
    Right () -> pure ()
