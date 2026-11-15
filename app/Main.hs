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
  , PromptBuffers
  , initialPromptBuffers
  )
import Hutian.History
  ( History (..)
  , HistoryTarget (..)
  , inputDestination
  , recordHistory
  )
import Hutian.History.Store
  ( HistoryFile (..)
  , appendHistoryFile
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
import Hutian.Render (OutputKind (..))
import Hutian.Shell (executableCommands, runShellCommand)
import Hutian.Terminal (promptForInput)
import Hutian.Tool.Exec (runToolCalls)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)

-- | Everything the interactive loop carries between prompts.
data ShellState = ShellState
  { shellSettings :: LlmSettings
  , shellCommands :: [String]
  , shellHistory :: History
  , shellInterrupt :: InterruptFlag
  , shellMode :: InputMode
  , shellBuffers :: PromptBuffers
  , shellConversation :: Conversation
  , shellJobs :: JobRuntime
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
      commands <- executableCommands
      termHistory <- loadHistoryFile termHistoryFile
      llmHistory <- loadHistoryFile llmHistoryFile
      interruptFlag <- newInterruptFlag
      installInterruptHandler interruptFlag
      withJobRuntime jobDirectory $ \jobs -> do
        let state =
              ShellState
                { shellSettings = settings
                , shellCommands = commands
                , shellHistory = History termHistory llmHistory
                , shellInterrupt = interruptFlag
                , shellMode = LlmInput
                , shellBuffers = initialPromptBuffers
                , shellConversation = emptyConversation
                , shellJobs = jobs
                }
        shellLoop state `finally` resetTerminal

shellLoop :: ShellState -> IO ()
shellLoop state = do
  (nextMode, input, nextBuffers) <-
    promptForInput
      (shellCommands state)
      (shellMode state)
      (shellHistory state)
      (shellBuffers state)
  let prompted =
        state
          { shellMode = nextMode
          , shellBuffers = nextBuffers
          }
  case input of
    Nothing -> pure ()
    Just text -> do
      history' <- rememberInput nextMode text (shellHistory state)
      clearInterrupt (shellInterrupt state)
      conversation <-
        runInput
          (shellJobs state)
          (shellSettings state)
          (shellInterrupt state)
          nextMode
          text
          (shellConversation state)
      shellLoop
        prompted
          { shellHistory = history'
          , shellConversation = conversation
          }

rememberInput :: InputMode -> String -> History -> IO History
rememberInput mode text history =
  case inputDestination mode text of
    Nothing -> pure history
    Just target -> do
      appendHistoryFile (historyFileFor target) text
      pure (recordHistory target text history)

historyFileFor :: HistoryTarget -> HistoryFile
historyFileFor TermHistory = termHistoryFile
historyFileFor LlmHistory = llmHistoryFile

runInput :: JobRuntime -> LlmSettings -> InterruptFlag -> InputMode -> String -> Conversation -> IO Conversation
runInput jobs settings interruptFlag mode text conversation =
  case mode of
    LlmInput -> runLlmInput jobs settings interruptFlag conversation text
    TermInput -> runTerminalInput conversation text

-- | Run a prompt through the model, executing any shell tools it requests and
-- feeding the results back until it produces a final answer.
runLlmInput :: JobRuntime -> LlmSettings -> InterruptFlag -> Conversation -> String -> IO Conversation
runLlmInput jobs settings interruptFlag conversation prompt =
  withoutEcho $ withRespondingCursor $ do
    result <- runLlmWithTools consoleLlmOutput interruptFlag settings conversation prompt
    continueLlm
      jobs
      settings
      interruptFlag
      (appendResult (Just prompt) (llmResponseText result) (llmToolCalls result) conversation)
      result

continueLlm :: JobRuntime -> LlmSettings -> InterruptFlag -> Conversation -> LlmResult -> IO Conversation
continueLlm jobs settings interruptFlag conversation result
  | null (llmToolCalls result) = pure conversation
  | otherwise = do
      (aborted, conversation') <-
        runToolCalls
          jobs
          (settingModelShell settings)
          conversation
          (llmToolCalls result)
      if aborted
        then pure conversation'
        else do
          next <- runLlmContinuation consoleLlmOutput interruptFlag settings conversation'
          continueLlm
            jobs
            settings
            interruptFlag
            (appendResult Nothing (llmResponseText next) (llmToolCalls next) conversation')
            next

-- | Stream LLM output through the terminal boundary, flushing each piece so a
-- response appears as it arrives.
consoleLlmOutput :: LlmOutput
consoleLlmOutput =
  LlmOutput
    { llmOutputWrite = \text -> unless (null text) (consoleWrite text >> consoleFlush)
    , llmOutputErrorLine = consoleWriteErrorLine
    }

runTerminalInput :: Conversation -> String -> IO Conversation
runTerminalInput conversation text =
  case classifyTerminalInput text of
    NoTerminalInput -> pure conversation
    SingleLineInput line -> runTerminalScript conversation line
    MultiLineInput script -> runTerminalScript conversation script

runTerminalScript :: Conversation -> String -> IO Conversation
runTerminalScript conversation command = do
  printOutputLabel TermOutput
  withGrayOutput (runStreamingCommand command)
  pure (appendTerminal command conversation)

runStreamingCommand :: String -> IO ()
runStreamingCommand command =
  runShellCommand command >>= either consoleWriteErrorLine pure

jobDirectory :: FilePath
jobDirectory = "/tmp/hutian-jobs"

termHistoryFile, llmHistoryFile :: HistoryFile
termHistoryFile = HistoryFile "/tmp/hutian-shell-history"
llmHistoryFile = HistoryFile "/tmp/hutian-llm-history"
