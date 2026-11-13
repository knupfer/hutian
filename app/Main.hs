{-# LANGUAGE ScopedTypeVariables #-}

-- | Entry point and interactive shell for the hutian sandbox.
module Main (main) where

import Control.Exception (finally)
import Control.Monad (foldM, unless)
import Data.List (isSuffixOf)
import Hutian.Command
  ( Command (..)
  , TerminalInput (..)
  , classifyTerminalInput
  , helpText
  , parseCommandLine
  )
import Hutian.Console
  ( configureConsole
  , consoleFlush
  , consoleIsErrorInteractive
  , consoleIsInteractive
  , consoleWrite
  , consoleWriteError
  , consoleWriteErrorLine
  , consoleWriteLine
  , consoleWriteToolCommand
  , printOutputLabel
  , resetTerminal
  , withGrayOutput
  , withRespondingCursor
  , withoutEcho
  )
import Hutian.Conversation
  ( Conversation
  , ToolCall (..)
  , appendResult
  , appendTerminal
  , appendToolResult
  , emptyConversation
  )
import Hutian.Editor
  ( PromptBuffers
  , PromptMode (..)
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
import Hutian.Llm
  ( LlmResult (..)
  , LlmSettings
  , parseLlmSettings
  , runLlmContinuation
  , runLlmWithTools
  , settingShell
  , toolCommand
  )
import Hutian.Policy (ShellAccess (..))
import Hutian.Output (grayOutputText)
import Hutian.Shell
  ( CommandResult (..)
  , commandResultText
  , executableCommands
  , runShellCommand
  , runShellCommandOutput
  )
import Hutian.Terminal (promptForInput)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)

-- | Everything the interactive loop carries between prompts.
data ShellState = ShellState
  { shellSettings :: LlmSettings
  , shellCommands :: [String]
  , shellHistory :: History
  , shellInterrupt :: InterruptFlag
  , shellMode :: PromptMode
  , shellBuffers :: PromptBuffers
  , shellConversation :: Conversation
  }

main :: IO ()
main = do
  configureConsole
  arguments <- getArgs
  case parseLlmSettings arguments of
    Left message -> do
      consoleWriteErrorLine message
      exitWith (ExitFailure 2)
    Right settings -> do
      commands <- executableCommands
      termHistory <- loadHistoryFile termHistoryFile
      llmHistory <- loadHistoryFile llmHistoryFile
      interruptFlag <- newInterruptFlag
      installInterruptHandler interruptFlag
      let state =
            ShellState
              { shellSettings = settings
              , shellCommands = commands
              , shellHistory = History termHistory llmHistory
              , shellInterrupt = interruptFlag
              , shellMode = LlmMode
              , shellBuffers = initialPromptBuffers
              , shellConversation = emptyConversation
              }
      shellLoop state `finally` resetTerminal

-- | The fixed prompt shown for term-mode commands. It deliberately does not
-- look like a shell working-directory prompt because commands do not persist
-- shell state between invocations.
termPrompt :: String
termPrompt = "shell:\n"

shellLoop :: ShellState -> IO ()
shellLoop state = do
  (nextMode, input, nextBuffers) <-
    promptForInput
      (shellCommands state)
      termPrompt
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

rememberInput :: PromptMode -> String -> History -> IO History
rememberInput mode text history =
  case inputDestination mode text of
    Nothing -> pure history
    Just target -> do
      appendHistoryFile (historyFileFor target) text
      pure (recordHistory target text history)

historyFileFor :: HistoryTarget -> HistoryFile
historyFileFor TermHistory = termHistoryFile
historyFileFor LlmHistory = llmHistoryFile

runInput :: LlmSettings -> InterruptFlag -> PromptMode -> String -> Conversation -> IO Conversation
runInput settings interruptFlag mode text conversation =
  case mode of
    LlmMode -> runLlmInput settings interruptFlag conversation text
    TermMode -> runTerminalInput conversation text
    ToolMode -> pure conversation

-- | Run a prompt through the model, executing any shell tools it requests and
-- feeding the results back until it produces a final answer.
runLlmInput :: LlmSettings -> InterruptFlag -> Conversation -> String -> IO Conversation
runLlmInput settings interruptFlag conversation prompt =
  withoutEcho $ withRespondingCursor $ do
    result <- runLlmWithTools interruptFlag settings conversation prompt
    continueLlm
      settings
      interruptFlag
      (appendResult (Just prompt) (llmResponseText result) (llmToolCalls result) conversation)
      result

continueLlm :: LlmSettings -> InterruptFlag -> Conversation -> LlmResult -> IO Conversation
continueLlm settings interruptFlag conversation result
  | null (llmToolCalls result) = pure conversation
  | otherwise = do
      conversation' <-
        runToolCalls
          (settingShell settings)
          conversation
          (llmToolCalls result)
      next <- runLlmContinuation interruptFlag settings conversation'
      continueLlm
        settings
        interruptFlag
        (appendResult Nothing (llmResponseText next) (llmToolCalls next) conversation')
        next

runToolCalls :: ShellAccess -> Conversation -> [ToolCall] -> IO Conversation
runToolCalls access = foldM (runToolCall access)

runToolCall :: ShellAccess -> Conversation -> ToolCall -> IO Conversation
runToolCall ShellDenied conversation call =
  pure (appendToolResult (toolCallId call) "tool error: shell access is disabled" conversation)
runToolCall ShellAllowed conversation call =
  case toolCommand call of
    Left message ->
      pure (appendToolResult (toolCallId call) ("tool error: " ++ message) conversation)
    Right command -> do
      consoleWriteToolCommand (toolCallName call) command
      printOutputLabel ToolMode
      result <- runShellCommandOutput command
      case result of
        Left message -> do
          consoleWriteErrorLine message
          pure (appendToolResult (toolCallId call) message conversation)
        Right output -> do
          displayCommandResult output
          pure (appendToolResult (toolCallId call) (commandResultText output) conversation)

runTerminalInput :: Conversation -> String -> IO Conversation
runTerminalInput conversation text =
  case classifyTerminalInput text of
    NoTerminalInput -> pure conversation
    SingleLineInput line -> runTerminalLine conversation line
    MultiLineInput script -> do
      printOutputLabel TermMode
      withGrayOutput (runStreamingCommand script)
      pure (appendTerminal script conversation)

runTerminalLine :: Conversation -> String -> IO Conversation
runTerminalLine conversation line =
  case parseCommandLine line of
    CommandEmpty -> pure conversation
    CommandHelp -> do
      withGrayOutput (mapM_ consoleWriteLine helpText)
      pure (appendTerminal line conversation)
    CommandShell commandLine -> do
      printOutputLabel TermMode
      withGrayOutput (runStreamingCommand commandLine)
      pure (appendTerminal line conversation)

runStreamingCommand :: String -> IO ()
runStreamingCommand command =
  runShellCommand command >>= either consoleWriteErrorLine pure

-- | Display captured command output the same way it would appear if the command
-- had inherited the terminal.
displayCommandResult :: CommandResult -> IO ()
displayCommandResult result = do
  stdoutInteractive <- consoleIsInteractive
  stderrInteractive <- consoleIsErrorInteractive
  let output = commandStdout result
  consoleWrite (grayOutputText stdoutInteractive output)
  consoleWriteError (grayOutputText stderrInteractive (commandStderr result))
  unless (null output || "\n" `isSuffixOf` output) (consoleWrite "\n")
  consoleFlush

termHistoryFile, llmHistoryFile :: HistoryFile
termHistoryFile = HistoryFile "/tmp/hutian-shell-history"
llmHistoryFile = HistoryFile "/tmp/hutian-llm-history"
