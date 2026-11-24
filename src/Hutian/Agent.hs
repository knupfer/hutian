-- | Model turns, tool execution, subagents, and terminal input. The composition
-- root in "Main" creates one 'RuntimeEnv' and passes it to every entry point,
-- instead of threading the working directory, job runtime, settings, and
-- interrupt flag through each function.
module Hutian.Agent
  ( RuntimeEnv (..)
  , PromptPresentation (..)
  , discoverCatalog
  , discoverMcpSettings
  , runInput
  , runLlmInput
  , runModelTurn
  , toolRuntime
  , runSubagent
  , runResultHandler
  , runAgent
  , consoleLlmOutputFor
  , runTerminalInput
  , runTerminalScript
  , runStreamingCommand
  ) where

import Control.Monad (unless, when)
import Hutian.Command (TerminalInput (..), classifyTerminalInput)
import Hutian.Console
  ( consoleFlush
  , consoleIsInteractive
  , consoleWrite
  , consoleWriteErrorLine
  , consoleWriteHandlerPromptIn
  , consoleWriteSubagentPrompt
  , consoleWriteSystemPrompt
  , consoleWriteToolDefinitions
  , printOutputLabelIn
  , withGrayOutput
  , withRespondingCursor
  , withoutEcho
  )
import Hutian.Context (TurnContext (MainContext), pathLabel, subagentContext)
import Hutian.Conversation
  ( Conversation
  , ToolCall (..)
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
import Hutian.Mcp (discoverMcpServers)
import Hutian.Mcp.Protocol (mcpHandlerName, mcpToolDisplayName)
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
import Hutian.Verbosity
  ( Verbosity (..)
  , modeToolOutput
  , outputBodyVisible
  , renderMode
  )
import System.IO (stderr, stdout)

-- | Everything a model turn needs. Constructing it once in "Main" keeps the
-- signatures below small and makes the shared state explicit.
data RuntimeEnv = RuntimeEnv
  { envWorkingDirectory :: FilePath
  , envJobs :: JobRuntime
  , envSettings :: LlmSettings
  , envInterrupt :: InterruptFlag
  }

-- | How an agent's prompt is announced. Result handlers use tool-output
-- styling and visibility because their prompt is the raw tool result.
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
    LlmInput -> runLlmInput env conversation text
    TermInput -> runTerminalInput (envWorkingDirectory env) conversation text

-- | Run a prompt through the model, executing any tools it requests and
-- feeding the results back until it produces a final answer.
runLlmInput :: RuntimeEnv -> Conversation -> String -> IO Conversation
runLlmInput env conversation prompt =
  withoutEcho $ withRespondingCursor $ do
    let runtime = toolRuntime env MainContext
    fst <$> runModelTurn env (envSettings env) runtime conversation prompt

-- | Send one prompt, then run the tool/continuation loop until the model
-- answers. Returns the updated conversation and the result that ended the turn.
-- The runtime context selects the output palette, so the initial request and
-- every continuation always agree.
runModelTurn
  :: RuntimeEnv
  -> LlmSettings
  -> ToolRuntime
  -> Conversation
  -> String
  -> IO (Conversation, LlmResult)
runModelTurn env settings runtime conversation prompt = do
  let output =
        consoleLlmOutputFor
          (toolRuntimeVerbosity runtime)
          (toolRuntimeContext runtime)
  result <-
    runLlmWithTools output (envInterrupt env) settings conversation prompt
  runToolLoopWithResult
    (llmLoop env settings runtime)
    (envInterrupt env)
    (appendResult (Just prompt) (llmResponseText result) (llmToolCalls result) conversation)
    result

-- | Bind the loop's effects to the real executor and model client.
llmLoop :: RuntimeEnv -> LlmSettings -> ToolRuntime -> LlmLoop
llmLoop env settings runtime =
  LlmLoop
    { loopRunTools = runToolCallsWith runtime (settingTools settings)
    , loopContinue =
        runLlmContinuation
          ( consoleLlmOutputFor
              (toolRuntimeVerbosity runtime)
              (toolRuntimeContext runtime)
          )
          (envInterrupt env)
          settings
    }

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

-- | Run a tool-result handler. The handler gets the raw result as its prompt,
-- but only its final answer is returned. Its own tool results use its own
-- handlers, never the caller's.
runResultHandler
  :: RuntimeEnv
  -> TurnContext
  -> String
  -> String
  -> ToolCall
  -> AgentConfig
  -> String
  -> IO (Either String String)
runResultHandler env parent serverName localName call config raw =
  runAgent
    env
    parent
    (HandlerPromptPresentation displayedOutput)
    (mcpHandlerName serverName localName)
    config
    ( "Tool: "
        ++ mcpToolDisplayName serverName localName
        ++ "\nArguments: "
        ++ toolCallArguments call
        ++ "\n\nResult:\n"
        ++ raw
    )
  where
    displayedOutput =
      outputBodyVisible
        (modeToolOutput (renderMode (settingVerbosity (envSettings env)) parent))

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
      childLabel = pathLabel childContext
      subagentSettings =
        (envSettings env)
          { settingModel = ModelName (agentModel agent)
          , settingSystemPrompt = SystemPrompt (agentSystemPrompt agent)
          , settingTools = tools
          }
      childEnv = env {envSettings = subagentSettings}
      runtime = toolRuntime childEnv childContext
  when (settingVerbosity (envSettings env) == VerbosityHigh) $ do
    consoleWriteSystemPrompt
      childContext
      (agentSystemPrompt agent)
    consoleWriteToolDefinitions
      childContext
      (toolSummaryLines (toolDefinitionsFor (settingTools subagentSettings)))
  case presentation of
    SubagentPromptPresentation ->
      consoleWriteSubagentPrompt childLabel prompt
    HandlerPromptPresentation showBody ->
      consoleWriteHandlerPromptIn parent childLabel showBody prompt
  (_, finalResult) <-
    runModelTurn
      childEnv
      subagentSettings
      runtime
      emptyConversation
      prompt
  pure $
    case llmResponseText finalResult of
      Just answer -> Right answer
      Nothing -> Left "tool error: the subagent returned no answer"

-- | Stream LLM output through the terminal boundary, flushing each piece so a
-- response appears as it arrives.
consoleLlmOutputFor :: Verbosity -> TurnContext -> LlmOutput
consoleLlmOutputFor verbosity context =
  LlmOutput
    { llmOutputWrite = \text -> unless (null text) (consoleWrite text >> consoleFlush)
    , llmOutputErrorLine = consoleWriteErrorLine
    , llmOutputContext = context
    , llmOutputVerbosity = verbosity
    }

runTerminalInput :: FilePath -> Conversation -> String -> IO Conversation
runTerminalInput workingDirectory conversation text =
  case classifyTerminalInput text of
    NoTerminalInput -> pure conversation
    SingleLineInput line -> runTerminalScript workingDirectory conversation line
    MultiLineInput script -> runTerminalScript workingDirectory conversation script

runTerminalScript :: FilePath -> Conversation -> String -> IO Conversation
runTerminalScript workingDirectory conversation command = do
  printOutputLabelIn MainContext "shell" TermOutput
  withGrayOutput (runStreamingCommand workingDirectory command)
  pure (appendTerminal command conversation)

-- | Run a command the user typed. A terminal gets inherited stdio so
-- full-screen programs work; otherwise output is streamed through the
-- sanitizer so escape sequences do not reach logs.
runStreamingCommand :: FilePath -> String -> IO ()
runStreamingCommand workingDirectory command = do
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
  either consoleWriteErrorLine pure result
