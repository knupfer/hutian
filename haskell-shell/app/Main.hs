{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Entry point and interactive shell for the hutian sandbox.
module Main (main) where

import Control.Exception (AsyncException (UserInterrupt), IOException, catch, try)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Catch as MonadCatch
import Data.Char (isSpace)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import GHC.IO.Encoding (setLocaleEncoding)
import Hutian.Interrupt (InterruptFlag, clearInterrupt, newInterruptFlag, withInterruptFlag)
import Hutian.Llm (runLlm)
import Hutian.Terminal (promptForLlm, resetTerminal)
import System.Console.Haskeline
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath (splitSearchPath, (</>))
import System.IO
import System.Process (createProcess, delegate_ctlc, proc, waitForProcess)

data Shell = Shell
  { shellInterrupts :: InterruptFlag
  }

main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  hSetBuffering stdout LineBuffering
  workdir <- fromMaybe "/" <$> lookupEnv "HUTIAN_WORKDIR"
  setCurrentDirectory workdir `catch` \(_ :: IOException) -> pure ()
  interruptFlag <- newInterruptFlag
  commands <- executableCommands
  let settings =
        (defaultSettings :: Settings IO)
          { historyFile = Just "/tmp/hutian-shell-history"
          , autoAddHistory = True
          , complete = shellCompletion commands
          }
      shell = Shell { shellInterrupts = interruptFlag }
  runInputT settings (shellLoop shell)

shellLoop :: Shell -> InputT IO ()
shellLoop shell =
  MonadCatch.handle handleUserInterrupt $
    handleInterrupt
      (liftIO resetTerminal >> shellLoop shell)
      (shellLoopOnce shell)
  where
    handleUserInterrupt exception =
      case exception of
        UserInterrupt -> liftIO resetTerminal >> shellLoop shell
        other -> MonadCatch.throwM other

shellLoopOnce :: Shell -> InputT IO ()
shellLoopOnce shell = do
  cwd <- liftIO getCurrentDirectory
  home <- liftIO (fromMaybe "/" <$> lookupEnv "HOME")
  input <- getInputLine ("hutian:" ++ displayPath home cwd ++ " $ ")
  case input of
    Nothing -> pure ()
    Just line -> dispatch shell line

dispatch :: Shell -> String -> InputT IO ()
dispatch shell line =
  case words line of
    [] -> shellLoop shell
    command : args ->
      case command of
        "exit" -> pure ()
        "quit" -> pure ()
        "help" -> liftIO printHelp >> shellLoop shell
        "pwd" -> liftIO (putStrLn =<< getCurrentDirectory) >> shellLoop shell
        "cd" -> liftIO (changeDirectory args) >> shellLoop shell
        "llm" -> liftIO (clearInterrupt (shellInterrupts shell)) >> llmCommand shell args >> shellLoop shell
        _ -> do
          liftIO (clearInterrupt (shellInterrupts shell))
          liftIO (runShellCommand (shellInterrupts shell) line)
          shellLoop shell

llmCommand :: Shell -> [String] -> InputT IO ()
llmCommand shell args
  | not (null args) = liftIO (runLlm (shellInterrupts shell) (unwords args))
  | otherwise = do
      result <- liftIO promptForLlm `MonadCatch.onException` liftIO resetTerminal
      case result of
        Nothing -> pure ()
        Just prompt -> liftIO (runLlm (shellInterrupts shell) prompt)

printHelp :: IO ()
printHelp = do
  putStrLn "Builtins:"
  putStrLn "  cd [dir]   change directory (defaults to $HOME)"
  putStrLn "  pwd        print the current directory"
  putStrLn "  llm        read a prompt until Ctrl+D and stream a llama.cpp response"
  putStrLn "  help       show this help"
  putStrLn "  exit       leave the shell"
  putStrLn "Everything else is executed with /bin/sh."
  putStrLn "Ctrl+D exits; Ctrl+C cancels the current input or command."

runShellCommand :: InterruptFlag -> String -> IO ()
runShellCommand interruptFlag line =
  withInterruptFlag interruptFlag $ do
    (_, _, _, processHandle) <-
      createProcess
        (proc "/bin/sh" ["-c", line])
          { delegate_ctlc = False
          }
    _ <- waitForProcess processHandle
    pure ()

changeDirectory :: [String] -> IO ()
changeDirectory args = do
  home <- fromMaybe "/" <$> lookupEnv "HOME"
  let target = expandHome home (if null args then home else unwords args)
  result <- try (setCurrentDirectory target) :: IO (Either IOException ())
  case result of
    Left err -> hPutStrLn stderr ("cd: " ++ show err)
    Right () -> pure ()

expandHome :: FilePath -> FilePath -> FilePath
expandHome home target
  | target == "~" = home
  | "~/" `isPrefixOf` target = home </> drop 2 target
  | otherwise = target

displayPath :: FilePath -> FilePath -> FilePath
displayPath home cwd =
  case stripPrefix home cwd of
    Just "" -> "~"
    Just rest -> "~" ++ rest
    Nothing -> cwd

executableCommands :: IO [String]
executableCommands = do
  searchPath <- fromMaybe "" <$> lookupEnv "PATH"
  names <- fmap concat $ forM (splitSearchPath searchPath) $ \dir -> do
    entries <- ignoringIOError [] (listDirectory dir)
    fmap concat $ forM entries $ \entry -> do
      let candidate = dir </> entry
      isDirectory <- ignoringIOError False (doesDirectoryExist candidate)
      permissions <- ignoringIOError emptyPermissions (getPermissions candidate)
      pure $
        if not isDirectory && executable permissions
          then [entry]
          else []
  pure . Set.toList . Set.fromList $ names ++ builtinCommands

builtinCommands :: [String]
builtinCommands = ["cd", "exit", "help", "llm", "pwd", "quit"]

ignoringIOError :: a -> IO a -> IO a
ignoringIOError fallback action = action `catch` \(_ :: IOException) -> pure fallback

shellCompletion :: [String] -> CompletionFunc IO
shellCompletion commands (reversedLeft, right)
  | all isSpace beforeWord && '/' `notElem` word =
      case filter (word `isPrefixOf`) commands of
        [] -> completeFilename (reversedLeft, right)
        matches -> pure (beforeWord, [Completion match match True | match <- matches])
  | otherwise = completeFilename (reversedLeft, right)
  where
    (reversedWord, beforeWord) = break isSpace reversedLeft
    word = reverse reversedWord
