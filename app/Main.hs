{-# LANGUAGE ScopedTypeVariables #-}

-- | Entry point and interactive shell for the hutian sandbox.
module Main (main) where

import Control.Exception (IOException, catch, finally)
import Control.Monad (when)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import GHC.IO.Encoding (setLocaleEncoding)
import Hutian.Conversation
  ( Conversation
  , appendExchange
  , appendTerminal
  , emptyConversation
  )
import Hutian.History (appendHistoryFile, loadHistoryFile)
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
  shellLoop
    commands
    termHistory
    llmHistory
    interruptFlag
    LlmMode
    initialPromptBuffers
    emptyConversation
    `finally` resetTerminal

shellLoop :: [String] -> [String] -> [String] -> InterruptFlag -> PromptMode -> PromptBuffers -> Conversation -> IO ()
shellLoop commands termHistory llmHistory interruptFlag mode buffers conversation = do
  home <- fromMaybe "/" <$> lookupEnv "HOME"
  cwd <- getCurrentDirectory
  let termPrompt = displayPath home cwd ++ " "
  (nextMode, input, nextBuffers) <-
    promptForInput commands termPrompt mode termHistory llmHistory buffers
  case input of
    Nothing ->
      shellLoop
        commands
        termHistory
        llmHistory
        interruptFlag
        nextMode
        nextBuffers
        conversation
    Just text -> do
      (termHistory', llmHistory') <-
        rememberInput nextMode text termHistory llmHistory
      clearInterrupt interruptFlag
      (continue, nextConversation) <-
        runInput interruptFlag nextMode text conversation
      when continue $
        shellLoop
          commands
          termHistory'
          llmHistory'
          interruptFlag
          nextMode
          nextBuffers
          nextConversation

rememberInput :: PromptMode -> String -> [String] -> [String] -> IO ([String], [String])
rememberInput mode text termHistory llmHistory
  | all isSpace text = pure (termHistory, llmHistory)
  | mode == TermMode = do
      appendHistoryFile termHistoryFile text
      pure (termHistory ++ [text], llmHistory)
  | otherwise = do
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
  case lines text of
    [] -> pure (True, conversation)
    [line] -> runTerminalLine interruptFlag conversation (words line) line
    _ -> do
      runShellCommand interruptFlag text
      pure (True, appendTerminal text conversation)

runTerminalLine :: InterruptFlag -> Conversation -> [String] -> String -> IO (Bool, Conversation)
runTerminalLine _ conversation [] _ = pure (True, conversation)
runTerminalLine interruptFlag conversation (command : args) line =
  case command of
    "exit" -> pure (False, conversation)
    "quit" -> pure (False, conversation)
    "help" -> printHelp >> pure (True, appendTerminal line conversation)
    "pwd" -> do
      putStrLn =<< getCurrentDirectory
      pure (True, appendTerminal line conversation)
    "cd" -> changeDirectory args >> pure (True, appendTerminal line conversation)
    "llm" -> do
      case args of
        [] -> putStrLn "Press Ctrl+X to switch to llm mode." >> pure (True, appendTerminal line conversation)
        _ -> do
          reply <- runLlm interruptFlag conversation (unwords args)
          pure (True, appendExchange (unwords args) reply conversation)
    _ -> do
      runShellCommand interruptFlag line
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
