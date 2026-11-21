-- | Entry point and interactive shell for the hutian sandbox.
module Main (main) where

import Control.Exception (finally)
import Control.Monad (unless)
import Hutian.Command
  ( TerminalInput (..)
  , classifyTerminalInput
  )
import Hutian.Console
  ( configureConsole
  , consoleFlush
  , consoleIsInteractive
  , consoleWrite
  , consoleWriteErrorLine
  , printOutputLabel
  , resetTerminal
  , withGrayOutput
  , withRespondingCursor
  , withoutEcho
  )
import Hutian.Conversation
  ( Conversation
  , appendResult
  , appendTerminal
  , emptyConversation
  )
import Hutian.Editor
  ( InputMode (..)
  , initialPromptBuffers
  )
import Hutian.Highlight (sanitizeOutputBytes)
import Hutian.History
  ( History (..)
  , HistoryTarget (..)
  )
import Hutian.History.Store
  ( HistoryFile (..)
  , loadHistoryFile
  )
import Hutian.Interrupt
  ( InterruptFlag
  , clearInterrupt
  , installInterruptHandler
  , newInterruptFlag
  )
import Hutian.Jobs.Runtime
  ( JobRuntime
  , withJobRuntime
  )
import Hutian.Llm
  ( LlmOutput (..)
  , LlmResult (..)
  , LlmSettings (..)
  , ModelName (..)
  , SystemPrompt (..)
  , loadLlmSettings
  , runLlmContinuation
  , runLlmWithTools
  )
import Hutian.Llm.Loop (LlmLoop (..), runToolLoopWithResult)
import Hutian.Mcp (discoverMcpServers)
import Hutian.Render (OutputKind (..), TurnContext (..))
import Hutian.Session
  ( Session (..)
  , SessionEffects (..)
  , rememberInput
  , runSession
  )
import Hutian.Shell
  ( executableCommands
  , runShellCommand
  , runShellCommandStreaming
  )
import Hutian.Terminal (promptForInput)
import Hutian.Tool.Catalog (ToolCatalog (..), toolCatalog)
import Hutian.Tool.Config (SubagentConfig (..), ToolConfig (..))
import Hutian.Tool.Exec (ToolRuntime (..), runToolCallsWith)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (stderr, stdout)

-- | The interactive loop's effects, bound to the real terminal, history files,
-- job runtime, and model client.
sessionEffects :: FilePath -> [String] -> JobRuntime -> LlmSettings -> InterruptFlag -> SessionEffects
sessionEffects workingDirectory commands jobs settings interruptFlag =
  SessionEffects
    { sessionPrompt = promptForInput workingDirectory commands
    , sessionRememberInput = rememberInput historyFileFor
    , sessionClearInterrupt = clearInterrupt interruptFlag
    , sessionRunInput =
        runInput workingDirectory jobs settings interruptFlag
    }

main :: IO ()
main = do
  configureConsole
  arguments <- getArgs
  parsed <- loadLlmSettings arguments
  case parsed of
    Left message -> do
      consoleWriteErrorLine message
      exitWith (ExitFailure 2)
    Right loaded -> do
      settings <- discoverMcpSettings loaded
      -- The launcher starts the shell in /source. Leave it before the session
      -- begins so the long-lived process does not pin that mount in gVisor;
      -- each command enters the working directory for its own lifetime.
      workingDirectory <- getCurrentDirectory
      setCurrentDirectory "/"
      commands <- executableCommands
      termHistory <- loadHistoryFile termHistoryFile
      llmHistory <- loadHistoryFile llmHistoryFile
      interruptFlag <- newInterruptFlag
      installInterruptHandler interruptFlag
      withJobRuntime jobDirectory workingDirectory $ \jobs -> do
        let session =
              Session
                { sessionMode = LlmInput
                , sessionBuffers = initialPromptBuffers
                , sessionHistory = History termHistory llmHistory
                , sessionConversation = emptyConversation
                }
        runSession
          (sessionEffects workingDirectory commands jobs settings interruptFlag)
          session
          `finally` resetTerminal

discoverMcpSettings :: LlmSettings -> IO LlmSettings
discoverMcpSettings settings = do
  servers <-
    discoverMcpServers
      consoleWriteErrorLine
      (toolMcp (catalogConfig (settingTools settings)))
  pure
    settings
      { settingTools =
          (settingTools settings) {catalogMcp = servers}
      }

historyFileFor :: HistoryTarget -> HistoryFile
historyFileFor TermHistory = termHistoryFile
historyFileFor LlmHistory = llmHistoryFile

runInput :: FilePath -> JobRuntime -> LlmSettings -> InterruptFlag -> InputMode -> String -> Conversation -> IO Conversation
runInput workingDirectory jobs settings interruptFlag mode text conversation =
  case mode of
    LlmInput -> runLlmInput workingDirectory jobs settings interruptFlag conversation text
    TermInput -> runTerminalInput workingDirectory conversation text

-- | Run a prompt through the model, executing any tools it requests and
-- feeding the results back until it produces a final answer.
runLlmInput :: FilePath -> JobRuntime -> LlmSettings -> InterruptFlag -> Conversation -> String -> IO Conversation
runLlmInput workingDirectory jobs settings interruptFlag conversation prompt =
  withoutEcho $ withRespondingCursor $ do
    let runtime = toolRuntime MainContext workingDirectory jobs settings interruptFlag
    fst <$> runModelTurn interruptFlag settings runtime conversation prompt

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
runModelTurn interruptFlag settings runtime conversation prompt = do
  let output = consoleLlmOutputIn (toolRuntimeContext runtime)
  result <- runLlmWithTools output interruptFlag settings conversation prompt
  runToolLoopWithResult
    (llmLoop interruptFlag runtime settings)
    interruptFlag
    (appendResult (Just prompt) (llmResponseText result) (llmToolCalls result) conversation)
    result

-- | Bind the loop's effects to the real executor and model client.
llmLoop :: InterruptFlag -> ToolRuntime -> LlmSettings -> LlmLoop
llmLoop interruptFlag runtime settings =
  LlmLoop
    { loopRunTools = runToolCallsWith runtime (settingTools settings)
    , loopContinue =
        runLlmContinuation
          (consoleLlmOutputIn (toolRuntimeContext runtime))
          interruptFlag
          settings
    }

-- | The runtime for one model turn. Subagents reuse the same API, settings,
-- job runtime, and interrupt flag, but substitute their own model, system
-- prompt, and tools. They run synchronously inside the tool call, so only the
-- final answer is appended to the caller's conversation.
toolRuntime :: TurnContext -> FilePath -> JobRuntime -> LlmSettings -> InterruptFlag -> ToolRuntime
toolRuntime context workingDirectory jobs settings interruptFlag =
  ToolRuntime
    { toolRuntimeJobs = jobs
    , toolRuntimeWorkingDirectory = workingDirectory
    , toolRuntimeContext = context
    , toolRuntimeSubagent = runSubagent workingDirectory jobs settings interruptFlag
    }

runSubagent
  :: FilePath
  -> JobRuntime
  -> LlmSettings
  -> InterruptFlag
  -> SubagentConfig
  -> String
  -> IO (Either String String)
runSubagent workingDirectory jobs settings interruptFlag agent prompt = do
  servers <-
    discoverMcpServers
      consoleWriteErrorLine
      (toolMcp (subagentTools agent))
  let subagentSettings =
        settings
          { settingModel = ModelName (subagentModel agent)
          , settingSystemPrompt = SystemPrompt (subagentSystemPrompt agent)
          , settingTools =
              (toolCatalog (subagentTools agent)) {catalogMcp = servers}
          }
      runtime =
        toolRuntime SubagentContext workingDirectory jobs subagentSettings interruptFlag
  (_, finalResult) <-
    runModelTurn
      interruptFlag
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
consoleLlmOutputIn :: TurnContext -> LlmOutput
consoleLlmOutputIn context =
  LlmOutput
    { llmOutputWrite = \text -> unless (null text) (consoleWrite text >> consoleFlush)
    , llmOutputErrorLine = consoleWriteErrorLine
    , llmOutputContext = context
    }

runTerminalInput :: FilePath -> Conversation -> String -> IO Conversation
runTerminalInput workingDirectory conversation text =
  case classifyTerminalInput text of
    NoTerminalInput -> pure conversation
    SingleLineInput line -> runTerminalScript workingDirectory conversation line
    MultiLineInput script -> runTerminalScript workingDirectory conversation script

runTerminalScript :: FilePath -> Conversation -> String -> IO Conversation
runTerminalScript workingDirectory conversation command = do
  printOutputLabel TermOutput
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

jobDirectory :: FilePath
jobDirectory = "/tmp/hutian-jobs"

termHistoryFile, llmHistoryFile :: HistoryFile
termHistoryFile = HistoryFile "/tmp/hutian-shell-history"
llmHistoryFile = HistoryFile "/tmp/hutian-llm-history"
