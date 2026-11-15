{-# LANGUAGE ScopedTypeVariables #-}

-- | Entry point and interactive shell for the hutian sandbox.
module Main (main) where

import Control.Exception (finally)
import Control.Monad (unless)
import Data.List (isSuffixOf)
import Hutian.Command
  ( Command (..)
  , TerminalInput (..)
  , classifyTerminalInput
  , helpText
  , jobUsageText
  , parseCommandLine
  )
import Hutian.Console
  ( ToolApproval (..)
  , configureConsole
  , confirmToolCommand
  , consoleFlush
  , consoleIsErrorInteractive
  , consoleIsInteractive
  , consoleWrite
  , consoleWriteError
  , consoleWriteErrorLine
  , consoleWriteJobEvent
  , consoleWriteLine
  , consoleWriteToolCommand
  , consoleWriteToolLabel
  , printJobOutputLabel
  , printJobsLabel
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
import Hutian.Jobs
  ( Job
  , JobId
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
  , withJobRuntime
  )
import Hutian.Llm
  ( LlmResult (..)
  , LlmSettings
  , loadLlmSettings
  , runLlmContinuation
  , runLlmWithTools
  , settingModelShell
  )
import Hutian.Output (grayOutputText)
import Hutian.Policy (ModelShell (..))
import Hutian.Shell
  ( CommandResult (..)
  , commandResultText
  , executableCommands
  , runShellCommand
  , runShellCommandOutput
  )
import Hutian.Terminal (promptForInput)
import Hutian.Tool (CommandMode (..), ToolPlan (..), planTool)
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
                , shellMode = LlmMode
                , shellBuffers = initialPromptBuffers
                , shellConversation = emptyConversation
                , shellJobs = jobs
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

runInput :: JobRuntime -> LlmSettings -> InterruptFlag -> PromptMode -> String -> Conversation -> IO Conversation
runInput jobs settings interruptFlag mode text conversation =
  case mode of
    LlmMode -> runLlmInput jobs settings interruptFlag conversation text
    TermMode -> runTerminalInput jobs conversation text
    ToolMode -> pure conversation

-- | Run a prompt through the model, executing any shell tools it requests and
-- feeding the results back until it produces a final answer.
runLlmInput :: JobRuntime -> LlmSettings -> InterruptFlag -> Conversation -> String -> IO Conversation
runLlmInput jobs settings interruptFlag conversation prompt =
  withoutEcho $ withRespondingCursor $ do
    result <- runLlmWithTools interruptFlag settings conversation prompt
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
          next <- runLlmContinuation interruptFlag settings conversation'
          continueLlm
            jobs
            settings
            interruptFlag
            (appendResult Nothing (llmResponseText next) (llmToolCalls next) conversation')
            next

runToolCalls :: JobRuntime -> ModelShell -> Conversation -> [ToolCall] -> IO (Bool, Conversation)
runToolCalls jobs policy = go False
  where
    go aborted conversation [] = pure (aborted, conversation)
    go True conversation (call : rest) =
      go True (appendToolResult (toolCallId call) abortedToolMessage conversation) rest
    go False conversation (call : rest) = do
      (callAborted, conversation') <- runToolCall jobs policy conversation call
      go callAborted conversation' rest

abortedToolMessage :: String
abortedToolMessage = "tool error: the user aborted the LLM turn"

runToolCall :: JobRuntime -> ModelShell -> Conversation -> ToolCall -> IO (Bool, Conversation)
runToolCall jobs policy conversation call =
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
    PlanJobOutput target lineCount -> runJobOutputTool target lineCount
    PlanJobKill target -> runJobKillTool target
  where
    refuse message =
      pure (False, appendToolResult (toolCallId call) message conversation)

    runPlanned mode command =
      case mode of
        Foreground -> do
          conversation' <- executeShellTool conversation call command
          pure (False, conversation')
        Background -> do
          conversation' <- executeBackgroundTool jobs conversation call command
          pure (False, conversation')

    runJobsTool = do
      consoleWriteToolLabel "jobs"
      printOutputLabel ToolMode
      current <- listJobs jobs
      printJobRows current
      pure (False, appendToolResult (toolCallId call) (jobListResult current) conversation)

    runJobOutputTool target lineCount = do
      consoleWriteToolLabel "job_output"
      printOutputLabel ToolMode
      result <- readJobOutput jobs target lineCount
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
      printOutputLabel ToolMode
      result <- killJob jobs target
      case result of
        Left message -> do
          consoleWriteErrorLine message
          pure (False, appendToolResult (toolCallId call) message conversation)
        Right (job, wasRunning) -> do
          let message = jobKillResult wasRunning job
          withGrayOutput (consoleWriteLine message)
          pure (False, appendToolResult (toolCallId call) message conversation)

-- | Run a command the model is already allowed to execute. The caller is
-- responsible for showing the command first.
executeShellTool :: Conversation -> ToolCall -> String -> IO Conversation
executeShellTool conversation call command = do
  printOutputLabel ToolMode
  result <- runShellCommandOutput command
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
  printOutputLabel ToolMode
  result <- startJob jobs command
  case result of
    Left message -> do
      consoleWriteErrorLine message
      pure (appendToolResult (toolCallId call) message conversation)
    Right job -> do
      withGrayOutput (consoleWriteLine (jobStartedLine job))
      pure (appendToolResult (toolCallId call) (jobStartedLine job) conversation)

runTerminalInput :: JobRuntime -> Conversation -> String -> IO Conversation
runTerminalInput jobs conversation text =
  case classifyTerminalInput text of
    NoTerminalInput -> pure conversation
    SingleLineInput line -> runTerminalLine jobs conversation line
    MultiLineInput script -> runTerminalScript conversation script script

runTerminalLine :: JobRuntime -> Conversation -> String -> IO Conversation
runTerminalLine jobs conversation line =
  case parseCommandLine line of
    CommandEmpty -> pure conversation
    CommandHelp -> do
      withGrayOutput (mapM_ consoleWriteLine helpText)
      pure (appendTerminal line conversation)
    CommandJobUsage -> do
      withGrayOutput (mapM_ consoleWriteLine jobUsageText)
      pure (appendTerminal line conversation)
    CommandJobs -> do
      current <- listJobs jobs
      printJobsList current
      pure (appendTerminal line conversation)
    CommandJobOutput target lineCount -> do
      result <- readJobOutput jobs target lineCount
      case result of
        Left message -> consoleWriteErrorLine message
        Right (_, output, truncated) -> printJobOutput target output truncated
      pure (appendTerminal line conversation)
    CommandJobStart command -> do
      result <- startJob jobs command
      case result of
        Left message -> consoleWriteErrorLine message
        Right job -> consoleWriteJobEvent (jobStartedLine job)
      pure (appendTerminal line conversation)
    CommandJobKill target -> do
      result <- killJob jobs target
      case result of
        Left message -> consoleWriteErrorLine message
        Right (job, wasRunning) -> consoleWriteJobEvent (jobKillResult wasRunning job)
      pure (appendTerminal line conversation)
    CommandShell commandLine -> runTerminalScript conversation line commandLine

runTerminalScript :: Conversation -> String -> String -> IO Conversation
runTerminalScript conversation recorded command = do
  printOutputLabel TermMode
  withGrayOutput (runStreamingCommand command)
  pure (appendTerminal recorded conversation)

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

-- | Print the job table in gray, omitting the label when the table is empty.
printJobsList :: [Job] -> IO ()
printJobsList current = do
  unless (null current) printJobsLabel
  printJobRows current

-- | Print job rows without a label. Used by the model-facing job tool after its
-- @tool output:@ line.
printJobRows :: [Job] -> IO ()
printJobRows current =
  withGrayOutput (mapM_ consoleWriteLine (lines (jobListResult current)))

-- | Print a job log tail. Empty output folds into the label line.
printJobOutput :: JobId -> String -> Bool -> IO ()
printJobOutput target output truncated
  | null output && not truncated = printJobOutputLabel target " (no output)"
  | otherwise = do
      printJobOutputLabel target ""
      displayJobOutput output truncated

-- | Print a job log tail, marking a truncated window at the top.
displayJobOutput :: String -> Bool -> IO ()
displayJobOutput output truncated = do
  interactive <- consoleIsInteractive
  let note = if truncated then jobOutputTruncationNote ++ "\n" else ""
      body = if null output then "(no output)\n" else output
      text = note ++ body
  consoleWrite (grayOutputText interactive text)
  unless (null text || "\n" `isSuffixOf` text) (consoleWrite "\n")
  consoleFlush

jobDirectory :: FilePath
jobDirectory = "/tmp/hutian-jobs"

termHistoryFile, llmHistoryFile :: HistoryFile
termHistoryFile = HistoryFile "/tmp/hutian-shell-history"
llmHistoryFile = HistoryFile "/tmp/hutian-llm-history"
