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
  , LlmSettings
  , loadLlmSettings
  , runLlmContinuation
  , runLlmWithTools
  , settingModelShell
  )
import Hutian.Llm.Loop (LlmLoop (..), runToolLoop)
import Hutian.Render (OutputKind (..))
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
import Hutian.Tool.Exec (runToolCalls)
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
    Right settings -> do
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

historyFileFor :: HistoryTarget -> HistoryFile
historyFileFor TermHistory = termHistoryFile
historyFileFor LlmHistory = llmHistoryFile

runInput :: FilePath -> JobRuntime -> LlmSettings -> InterruptFlag -> InputMode -> String -> Conversation -> IO Conversation
runInput workingDirectory jobs settings interruptFlag mode text conversation =
  case mode of
    LlmInput -> runLlmInput workingDirectory jobs settings interruptFlag conversation text
    TermInput -> runTerminalInput workingDirectory conversation text

-- | Run a prompt through the model, executing any shell tools it requests and
-- feeding the results back until it produces a final answer.
runLlmInput :: FilePath -> JobRuntime -> LlmSettings -> InterruptFlag -> Conversation -> String -> IO Conversation
runLlmInput workingDirectory jobs settings interruptFlag conversation prompt =
  withoutEcho $ withRespondingCursor $ do
    result <- runLlmWithTools consoleLlmOutput interruptFlag settings conversation prompt
    runToolLoop
      (llmLoop interruptFlag workingDirectory jobs settings)
      interruptFlag
      (appendResult (Just prompt) (llmResponseText result) (llmToolCalls result) conversation)
      result

-- | Bind the loop's effects to the real executor and model client.
llmLoop :: InterruptFlag -> FilePath -> JobRuntime -> LlmSettings -> LlmLoop
llmLoop interruptFlag workingDirectory jobs settings =
  LlmLoop
    { loopRunTools =
        runToolCalls
          jobs
          workingDirectory
          (settingModelShell settings)
    , loopContinue =
        runLlmContinuation consoleLlmOutput interruptFlag settings
    }

-- | Stream LLM output through the terminal boundary, flushing each piece so a
-- response appears as it arrives.
consoleLlmOutput :: LlmOutput
consoleLlmOutput =
  LlmOutput
    { llmOutputWrite = \text -> unless (null text) (consoleWrite text >> consoleFlush)
    , llmOutputErrorLine = consoleWriteErrorLine
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
