{-# LANGUAGE ScopedTypeVariables #-}

-- | Entry point and interactive shell for the hutian sandbox.
module Main (main) where

import Control.Exception (finally)
import Control.Monad (foldM)
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
  , toolCommand
  )
import Hutian.Output (printOutputLabel, withGrayOutput, withRespondingCursor)
import Hutian.Shell
  ( executableCommands
  , runShellCommand
  , runShellCommandOutput
  )
import Hutian.Terminal
  ( promptForInput
  , renderToolCommand
  , resetTerminal
  , withoutEcho
  )
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO

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
runInput settings interruptFlag LlmMode text conversation =
  runLlmInput settings interruptFlag conversation text
runInput _ _ TermMode text conversation =
  runTerminalInput conversation text
runInput _ _ ToolMode text conversation =
  runTerminalInput conversation text

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
      0

continueLlm :: LlmSettings -> InterruptFlag -> Conversation -> LlmResult -> Int -> IO Conversation
continueLlm settings interruptFlag conversation result rounds
  | null (llmToolCalls result) = pure conversation
  | otherwise = do
      conversation' <- runToolCalls conversation (llmToolCalls result)
      next <- runLlmContinuation interruptFlag settings conversation'
      continueLlm
        settings
        interruptFlag
        (appendResult Nothing (llmResponseText next) (llmToolCalls next) conversation')
        next
        (rounds + 1)

runToolCalls :: Conversation -> [ToolCall] -> IO Conversation
runToolCalls = foldM runToolCall

runToolCall :: Conversation -> ToolCall -> IO Conversation
runToolCall conversation call =
  case toolCommand call of
    Left message ->
      pure (appendToolResult (toolCallId call) ("tool error: " ++ message) conversation)
    Right command -> do
      renderToolCommand (toolCallName call) command
      printOutputLabel ToolMode
      output <- runShellCommandOutput command
      pure (appendToolResult (toolCallId call) output conversation)

runTerminalInput :: Conversation -> String -> IO Conversation
runTerminalInput conversation text =
  case classifyTerminalInput text of
    NoTerminalInput -> pure conversation
    SingleLineInput line -> runTerminalLine conversation line
    MultiLineInput script -> do
      printOutputLabel TermMode
      withGrayOutput (runShellCommand script)
      pure (appendTerminal script conversation)

runTerminalLine :: Conversation -> String -> IO Conversation
runTerminalLine conversation line =
  case parseCommandLine line of
    CommandEmpty -> pure conversation
    CommandHelp ->
      withGrayOutput printHelp
        >> pure (appendTerminal line conversation)
    CommandShell commandLine -> do
      printOutputLabel TermMode
      withGrayOutput (runShellCommand commandLine)
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
    , "Shell output is preceded by an 'output:' line; tool output by a"
    , "'tool output:' line. Output is shown in gray."
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

termHistoryFile, llmHistoryFile :: HistoryFile
termHistoryFile = HistoryFile "/tmp/hutian-shell-history"
llmHistoryFile = HistoryFile "/tmp/hutian-llm-history"
