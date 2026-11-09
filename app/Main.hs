{-# LANGUAGE ScopedTypeVariables #-}

-- | Entry point and interactive shell for the hutian sandbox.
module Main (main) where

import Control.Exception (IOException, catch, finally)
import Control.Monad (when)
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
  , appendExchange
  , appendTerminal
  , emptyConversation
  )
import Hutian.History
  ( InputDestination (..)
  , appendHistoryFile
  , inputDestination
  , loadHistoryFile
  )
import Hutian.Interrupt (InterruptFlag, clearInterrupt, newInterruptFlag)
import Hutian.Llm (runLlm)
import Hutian.Paths (displayPath)
import Hutian.Shell (changeDirectory, executableCommands, runShellCommand)
import Hutian.Terminal
  ( PromptBuffers
  , PromptMode (..)
  , initialPromptBuffers
  , promptForInput
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
runInput interruptFlag LlmMode text conversation = do
  reply <- runLlm interruptFlag conversation text
  pure (True, appendExchange text reply conversation)
runInput interruptFlag TermMode text conversation =
  runTerminalInput interruptFlag conversation text

runTerminalInput :: InterruptFlag -> Conversation -> String -> IO (Bool, Conversation)
runTerminalInput interruptFlag conversation text =
  case classifyTerminalInput text of
    NoTerminalInput -> pure (True, conversation)
    SingleLineInput line -> runTerminalLine interruptFlag conversation line
    MultiLineInput script -> do
      runShellCommand interruptFlag script
      pure (True, appendTerminal script conversation)

runTerminalLine :: InterruptFlag -> Conversation -> String -> IO (Bool, Conversation)
runTerminalLine interruptFlag conversation line =
  case parseCommandLine line of
    CommandEmpty -> pure (True, conversation)
    CommandExit -> pure (False, conversation)
    CommandHelp -> printHelp >> pure (True, appendTerminal line conversation)
    CommandPrintDirectory -> do
      putStrLn =<< getCurrentDirectory
      pure (True, appendTerminal line conversation)
    CommandChangeDirectory target -> do
      changeDirectory target
      pure (True, appendTerminal line conversation)
    CommandLlm Nothing -> do
      putStrLn "Press Ctrl+X to switch to llm mode."
      pure (True, appendTerminal line conversation)
    CommandLlm (Just prompt) -> do
      reply <- runLlm interruptFlag conversation prompt
      pure (True, appendExchange prompt reply conversation)
    CommandShell commandLine -> do
      runShellCommand interruptFlag commandLine
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
