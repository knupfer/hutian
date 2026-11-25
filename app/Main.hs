-- | Entry point and interactive shell for the hutian sandbox.
module Main (main) where

import Control.Exception (finally)
import Control.Monad (when)
import Hutian.Agent
  ( RuntimeEnv (..)
  , discoverMcpSettings
  , runInput
  )
import Hutian.Console
  ( configureConsole
  , consoleWriteErrorLine
  , consoleWriteSystemPrompt
  , consoleWriteToolDefinitions
  , resetTerminal
  )
import Hutian.Context (TurnContext (MainContext))
import Hutian.Conversation (emptyConversation)
import Hutian.Editor (InputMode (..), initialPromptBuffers)
import Hutian.History (History (..))
import Hutian.Interrupt
  ( clearInterrupt
  , installInterruptHandler
  , newInterruptFlag
  )
import Hutian.Jobs.Runtime (withJobRuntime)
import Hutian.Llm
  ( LlmSettings (..)
  , SystemPrompt (..)
  , loadLlmSettings
  )
import Hutian.Mcp (newMcpSessionCache)
import Hutian.Session
  ( Session (..)
  , SessionEffects (..)
  , runSession
  )
import Hutian.Shell (executableCommands)
import Hutian.Terminal (promptForInput)
import Hutian.Tool (toolDefinitionsFor)
import Hutian.Tool.Summary (toolSummaryLines)
import Hutian.Verbosity (Verbosity (..))
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))

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
      interruptFlag <- newInterruptFlag
      installInterruptHandler interruptFlag
      mcpSessions <- newMcpSessionCache
      directory <- jobDirectory
      withJobRuntime directory workingDirectory $ \jobs -> do
        let env =
              RuntimeEnv
                { envWorkingDirectory = workingDirectory
                , envJobs = jobs
                , envSettings = settings
                , envInterrupt = interruptFlag
                , envMcpSessions = mcpSessions
                }
            session =
              Session
                { sessionMode = LlmInput
                , sessionBuffers = initialPromptBuffers
                , sessionHistory = History [] []
                , sessionConversation = emptyConversation
                }
        when (settingVerbosity settings == VerbosityHigh) $ do
          consoleWriteSystemPrompt
            MainContext
            (unSystemPrompt (settingSystemPrompt settings))
          consoleWriteToolDefinitions
            MainContext
            (toolSummaryLines (toolDefinitionsFor (settingTools settings)))
        runSession
          (sessionEffects env commands)
          session
          `finally` resetTerminal

-- | The interactive loop's effects, bound to the real terminal, job runtime,
-- and model client.
sessionEffects :: RuntimeEnv -> [String] -> SessionEffects
sessionEffects env commands =
  SessionEffects
    { sessionPrompt = promptForInput (envWorkingDirectory env) commands
    , sessionClearInterrupt = clearInterrupt (envInterrupt env)
    , sessionRunInput = runInput env
    }

-- | Keep job logs under the session's runtime directory when one is
-- configured, matching the relay's socket convention.
jobDirectory :: IO FilePath
jobDirectory = do
  runtimeDirectory <- lookupEnv "XDG_RUNTIME_DIR"
  pure $ case runtimeDirectory of
    Just directory | not (null directory) -> directory </> "hutian-jobs"
    _ -> "/tmp/hutian-jobs"
