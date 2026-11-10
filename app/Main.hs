{-# LANGUAGE ScopedTypeVariables #-}

-- | Entry point and interactive shell for the hutian sandbox.
module Main (main) where

import Control.Exception (finally)
import Control.Monad (foldM)
import Data.Maybe (fromMaybe)
import GHC.IO.Encoding (setLocaleEncoding)
import Hutian.Command
  ( Command (..)
  , TerminalInput (..)
  , classifyTerminalInput
  , parseCommandLine
  )
import Hutian.Conversation
  ( Conversation
  , ToolCall (..)
  , appendAssistant
  , appendAssistantToolCalls
  , appendExchange
  , appendTerminal
  , appendToolResult
  , appendUser
  , emptyConversation
  )
import Hutian.History
  ( InputDestination (..)
  , appendHistoryFile
  , inputDestination
  , loadHistoryFile
  )
import Hutian.Interrupt (InterruptFlag, clearInterrupt, newInterruptFlag)
import Hutian.Llm
  ( LlmResult (..)
  , LlmSettings
  , parseLlmSettings
  , runLlmContinuation
  , runLlmWithTools
  , toolCommand
  )
import Hutian.Output (printOutputLabel, withGrayOutput)
import Hutian.Shell
  ( executableCommands
  , runShellCommand
  , runShellCommandOutput
  )
import Hutian.Terminal
  ( PromptBuffers
  , PromptMode (..)
  , initialPromptBuffers
  , promptForInput
  , renderToolCommand
  , resetTerminal
  )
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO

-- | Everything the interactive loop carries between prompts.
data ShellState = ShellState
  { shellSettings :: LlmSettings
  , shellCommands :: [String]
  , shellTermHistory :: [String]
  , shellLlmHistory :: [String]
  , shellInterrupt :: InterruptFlag
  , shellMode :: PromptMode
  , shellBuffers :: PromptBuffers
  , shellConversation :: Conversation
  }

main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  hSetBuffering stdout LineBuffering
  arguments <- getArgs
  case parseLlmSettings arguments of
    Left message -> do
      hPutStrLn stderr message
      exitWith (ExitFailure 2)
    Right settings -> do
      commands <- executableCommands
      termHistory <- loadHistoryFile termHistoryFile
      llmHistory <- loadHistoryFile llmHistoryFile
      interruptFlag <- newInterruptFlag
      let state =
            ShellState
              { shellSettings = settings
              , shellCommands = commands
              , shellTermHistory = termHistory
              , shellLlmHistory = llmHistory
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
      (shellTermHistory state)
      (shellLlmHistory state)
      (shellBuffers state)
  let prompted =
        state
          { shellMode = nextMode
          , shellBuffers = nextBuffers
          }
  case input of
    Nothing -> pure ()
    Just text -> do
      (termHistory', llmHistory') <-
        rememberInput
          nextMode
          text
          (shellTermHistory state)
          (shellLlmHistory state)
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
          { shellTermHistory = termHistory'
          , shellLlmHistory = llmHistory'
          , shellConversation = conversation
          }

rememberInput :: PromptMode -> String -> [String] -> [String] -> IO ([String], [String])
rememberInput mode text termHistory llmHistory =
  case inputDestination mode text of
    DiscardInput -> pure (termHistory, llmHistory)
    TermHistoryEntry -> do
      appendHistoryFile termHistoryFile text
      pure (termHistory ++ [text], llmHistory)
    LlmHistoryEntry -> do
      appendHistoryFile llmHistoryFile text
      pure (termHistory, llmHistory ++ [text])

runInput :: LlmSettings -> InterruptFlag -> PromptMode -> String -> Conversation -> IO Conversation
runInput settings interruptFlag LlmMode text conversation =
  runLlmInput settings interruptFlag conversation text
runInput _ interruptFlag TermMode text conversation =
  runTerminalInput interruptFlag conversation text
runInput _ interruptFlag ToolMode text conversation =
  runTerminalInput interruptFlag conversation text

-- | Run a prompt through the model, executing any shell tools it requests and
-- feeding the results back until it produces a final answer.
runLlmInput :: LlmSettings -> InterruptFlag -> Conversation -> String -> IO Conversation
runLlmInput settings interruptFlag conversation prompt = do
  result <- runLlmWithTools interruptFlag settings conversation prompt
  continueLlm
    settings
    interruptFlag
    (recordPromptResult prompt result conversation)
    result
    0

recordPromptResult :: String -> LlmResult -> Conversation -> Conversation
recordPromptResult prompt result conversation =
  case llmToolCalls result of
    [] -> appendExchange prompt (llmResponseText result) conversation
    calls ->
      appendAssistantToolCalls
        (fromMaybe "" (llmResponseText result))
        calls
        (appendUser prompt conversation)

recordContinuationResult :: LlmResult -> Conversation -> Conversation
recordContinuationResult result conversation =
  case llmToolCalls result of
    [] -> appendAssistant (fromMaybe "" (llmResponseText result)) conversation
    calls ->
      appendAssistantToolCalls
        (fromMaybe "" (llmResponseText result))
        calls
        conversation

continueLlm :: LlmSettings -> InterruptFlag -> Conversation -> LlmResult -> Int -> IO Conversation
continueLlm settings interruptFlag conversation result rounds
  | null (llmToolCalls result) = pure conversation
  | rounds >= maxToolRounds = do
      putStrLn "llm: stopped after the maximum number of tool calls"
      pure (rejectToolCalls conversation (llmToolCalls result))
  | otherwise = do
      conversation' <- runToolCalls interruptFlag conversation (llmToolCalls result)
      next <- runLlmContinuation interruptFlag settings conversation'
      continueLlm
        settings
        interruptFlag
        (recordContinuationResult next conversation')
        next
        (rounds + 1)

maxToolRounds :: Int
maxToolRounds = 8

rejectToolCalls :: Conversation -> [ToolCall] -> Conversation
rejectToolCalls =
  foldl'
    (\conversation call ->
       appendToolResult
         (toolCallId call)
         "tool error: maximum number of tool calls reached"
         conversation
    )

runToolCalls :: InterruptFlag -> Conversation -> [ToolCall] -> IO Conversation
runToolCalls interruptFlag = foldM (runToolCall interruptFlag)

runToolCall :: InterruptFlag -> Conversation -> ToolCall -> IO Conversation
runToolCall interruptFlag conversation call =
  case toolCommand call of
    Left message ->
      pure (appendToolResult (toolCallId call) ("tool error: " ++ message) conversation)
    Right command -> do
      renderToolCommand (toolCallName call) command
      printOutputLabel ToolMode
      output <- runShellCommandOutput interruptFlag command
      pure (appendToolResult (toolCallId call) output conversation)

runTerminalInput :: InterruptFlag -> Conversation -> String -> IO Conversation
runTerminalInput interruptFlag conversation text =
  case classifyTerminalInput text of
    NoTerminalInput -> pure conversation
    SingleLineInput line -> runTerminalLine interruptFlag conversation line
    MultiLineInput script -> do
      printOutputLabel TermMode
      withGrayOutput (runShellCommand interruptFlag script)
      pure (appendTerminal script conversation)

runTerminalLine :: InterruptFlag -> Conversation -> String -> IO Conversation
runTerminalLine interruptFlag conversation line =
  case parseCommandLine line of
    CommandEmpty -> pure conversation
    CommandHelp ->
      withGrayOutput printHelp
        >> pure (appendTerminal line conversation)
    CommandShell commandLine -> do
      printOutputLabel TermMode
      withGrayOutput (runShellCommand interruptFlag commandLine)
      pure (appendTerminal line conversation)

printHelp :: IO ()
printHelp =
  mapM_
    putStrLn
    [ "The interactive input has two modes:"
    , "  llm mode   send text to llama.cpp (pink ask: line)"
    , "  term mode  run a shell command (shell: line)"
    , "Ctrl+X toggles between the modes; each mode keeps its own input."
    , "The app starts in llm mode."
    , "In llm mode the model may call the shell tool. Tool calls are shown"
    , "with a bold teal 'tool shell:' line."
    , "Shell and tool output is preceded by an 'output:' line and shown in"
    , "gray."
    , ""
    , "Term-mode commands:"
    , "  help       show this help"
    , "Every other line is executed with /bin/sh in the sandbox working"
    , "directory. Each command runs in a fresh shell, so cd, exported"
    , "variables, and other shell state do not persist between commands."
    , "Tab completes commands from PATH and filesystem paths; ambiguous"
    , "matches are listed below the input."
    , "Enter runs a command in term mode and adds a line in llm mode."
    , "Up/Down recall history; trailing \\ or open quotes continue a command."
    , "Ctrl+D on empty input exits; Enter runs term commands."
    , "Ctrl+C clears the current input."
    ]

termHistoryFile, llmHistoryFile :: FilePath
termHistoryFile = "/tmp/hutian-shell-history"
llmHistoryFile = "/tmp/hutian-llm-history"
