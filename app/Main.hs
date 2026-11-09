{-# LANGUAGE ScopedTypeVariables #-}

-- | Entry point and interactive shell for the hutian sandbox.
module Main (main) where

import Control.Exception (IOException, catch, finally)
import Control.Monad (foldM, when)
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
  , runLlmContinuation
  , runLlmWithTools
  , toolCommand
  )
import Hutian.Output (termOutputBackground, withGrayOutput)
import Hutian.Paths (displayPath)
import Hutian.Shell
  ( changeDirectory
  , executableCommands
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
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.Environment (lookupEnv)
import System.IO

-- | Everything the interactive loop carries between prompts.
data ShellState = ShellState
  { shellCommands :: [String]
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
  workdir <- fromMaybe "/" <$> lookupEnv "HUTIAN_WORKDIR"
  setCurrentDirectory workdir `catch` \(_ :: IOException) -> pure ()
  commands <- executableCommands
  termHistory <- loadHistoryFile termHistoryFile
  llmHistory <- loadHistoryFile llmHistoryFile
  interruptFlag <- newInterruptFlag
  let state =
        ShellState
          { shellCommands = commands
          , shellTermHistory = termHistory
          , shellLlmHistory = llmHistory
          , shellInterrupt = interruptFlag
          , shellMode = LlmMode
          , shellBuffers = initialPromptBuffers
          , shellConversation = emptyConversation
          }
  shellLoop state `finally` resetTerminal

shellLoop :: ShellState -> IO ()
shellLoop state = do
  home <- fromMaybe "/" <$> lookupEnv "HOME"
  cwd <- getCurrentDirectory
  let termPrompt = displayPath home cwd ++ " "
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
    Nothing -> shellLoop prompted
    Just text -> do
      (termHistory', llmHistory') <-
        rememberInput
          nextMode
          text
          (shellTermHistory state)
          (shellLlmHistory state)
      clearInterrupt (shellInterrupt state)
      (continue, conversation) <-
        runInput (shellInterrupt state) nextMode text (shellConversation state)
      when continue $
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

runInput :: InterruptFlag -> PromptMode -> String -> Conversation -> IO (Bool, Conversation)
runInput interruptFlag LlmMode text conversation =
  runLlmInput interruptFlag conversation text
runInput interruptFlag TermMode text conversation =
  runTerminalInput interruptFlag conversation text
runInput interruptFlag ToolMode text conversation =
  runTerminalInput interruptFlag conversation text

-- | Run a prompt through the model, executing any shell tools it requests and
-- feeding the results back until it produces a final answer.
runLlmInput :: InterruptFlag -> Conversation -> String -> IO (Bool, Conversation)
runLlmInput interruptFlag conversation prompt = do
  result <- runLlmWithTools interruptFlag conversation prompt
  continueLlm
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

continueLlm :: InterruptFlag -> Conversation -> LlmResult -> Int -> IO (Bool, Conversation)
continueLlm interruptFlag conversation result rounds
  | null (llmToolCalls result) = pure (True, conversation)
  | rounds >= maxToolRounds = do
      putStrLn "llm: stopped after the maximum number of tool calls"
      pure (True, rejectToolCalls conversation (llmToolCalls result))
  | otherwise = do
      conversation' <- runToolCalls interruptFlag conversation (llmToolCalls result)
      next <- runLlmContinuation interruptFlag conversation'
      continueLlm
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
      termPrompt <- currentTermPrompt
      renderToolCommand termPrompt command
      output <- runShellCommandOutput interruptFlag command
      pure (appendToolResult (toolCallId call) output conversation)

currentTermPrompt :: IO String
currentTermPrompt = do
  home <- fromMaybe "/" <$> lookupEnv "HOME"
  cwd <- getCurrentDirectory
  pure (displayPath home cwd ++ " ")

runTerminalInput :: InterruptFlag -> Conversation -> String -> IO (Bool, Conversation)
runTerminalInput interruptFlag conversation text =
  case classifyTerminalInput text of
    NoTerminalInput -> pure (True, conversation)
    SingleLineInput line -> runTerminalLine interruptFlag conversation line
    MultiLineInput script -> do
      withGrayOutput termOutputBackground (runShellCommand interruptFlag script)
      pure (True, appendTerminal script conversation)

runTerminalLine :: InterruptFlag -> Conversation -> String -> IO (Bool, Conversation)
runTerminalLine interruptFlag conversation line =
  case parseCommandLine line of
    CommandEmpty -> pure (True, conversation)
    CommandExit -> pure (False, conversation)
    CommandHelp ->
      withGrayOutput termOutputBackground printHelp
        >> pure (True, appendTerminal line conversation)
    CommandPrintDirectory -> do
      withGrayOutput termOutputBackground (putStrLn =<< getCurrentDirectory)
      pure (True, appendTerminal line conversation)
    CommandChangeDirectory target -> do
      withGrayOutput termOutputBackground (changeDirectory target)
      pure (True, appendTerminal line conversation)
    CommandLlm Nothing -> do
      withGrayOutput termOutputBackground (putStrLn "Press Ctrl+X to switch to llm mode.")
      pure (True, appendTerminal line conversation)
    CommandLlm (Just prompt) ->
      runLlmInput interruptFlag conversation prompt
    CommandShell commandLine -> do
      withGrayOutput termOutputBackground (runShellCommand interruptFlag commandLine)
      pure (True, appendTerminal line conversation)

printHelp :: IO ()
printHelp =
  mapM_
    putStrLn
    [ "The interactive input has two modes:"
    , "  llm mode   send text to llama.cpp (purple background)"
    , "  term mode  run a shell command (dark blue background)"
    , "Ctrl+X toggles between the modes; each mode keeps its own input."
    , "The app starts in llm mode."
    , "In llm mode the model may call the run_shell_command tool. Tool"
    , "commands are shown with a dark red background and a bright red path."
    , "Term-mode command output is gray on a slightly darker blue background;"
    , "tool-call output is gray on a slightly darker red background."
    , ""
    , "Term-mode builtins:"
    , "  cd [dir]   change directory (defaults to $HOME)"
    , "  pwd        print the current directory"
    , "  llm [msg]  send a one-off prompt to llama.cpp"
    , "  help       show this help"
    , "  exit       leave the shell"
    , "Everything else is executed with /bin/sh."
    , "Tab completes commands from PATH and filesystem paths; ambiguous"
    , "matches are listed below the input."
    , "Enter runs a command in term mode and adds a line in llm mode."
    , "Up/Down recall history; trailing \\ or open quotes continue a command."
    , "Ctrl+D submits llm input; Enter runs term commands."
    , "Ctrl+C clears the current input."
    ]

termHistoryFile, llmHistoryFile :: FilePath
termHistoryFile = "/tmp/hutian-shell-history"
llmHistoryFile = "/tmp/hutian-llm-history"
