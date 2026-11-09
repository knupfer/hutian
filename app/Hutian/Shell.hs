{-# LANGUAGE ScopedTypeVariables #-}

-- | Shell command execution and command discovery.
module Hutian.Shell
  ( runShellCommand
  , runShellCommandOutput
  , executableCommands
  , changeDirectory
  , resolveDirectoryTarget
  , withBuiltins
  ) where

import Control.Exception (IOException, catch)
import Control.Monad (unless, void)
import Data.List (nub)
import Data.Maybe (catMaybes, fromMaybe)
import Hutian.Interrupt (InterruptFlag, withInterruptFlag)
import Hutian.Output (grayOutputText, toolOutputBackground)
import Hutian.Paths (expandHome)
import System.Directory
  ( doesDirectoryExist
  , emptyPermissions
  , executable
  , getPermissions
  , listDirectory
  , setCurrentDirectory
  )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (splitSearchPath, (</>))
import System.IO (hFlush, hIsTerminalDevice, hPutStr, hPutStrLn, stderr, stdout)
import System.Process
  ( createProcess
  , delegate_ctlc
  , proc
  , readCreateProcessWithExitCode
  , waitForProcess
  )

runShellCommand :: InterruptFlag -> String -> IO ()
runShellCommand interruptFlag line =
  withInterruptFlag interruptFlag $
    ( do
        (_, _, _, processHandle) <-
          createProcess
            (proc "/bin/sh" ["-c", line])
              { delegate_ctlc = False
              }
        void (waitForProcess processHandle)
    )
      `catch` \(err :: IOException) -> hPutStrLn stderr ("sh: " ++ show err)

-- | Run a command and return its combined output, so the result can be sent
-- back to the model. The output is also written to the terminal.
runShellCommandOutput :: InterruptFlag -> String -> IO String
runShellCommandOutput interruptFlag line =
  withInterruptFlag interruptFlag $
    ( do
        (exitCode, output, errors) <-
          readCreateProcessWithExitCode (proc "/bin/sh" ["-c", line]) ""
        outputIsTerminal <- hIsTerminalDevice stdout
        errorsAreTerminal <- hIsTerminalDevice stderr
        putStr (grayOutputText toolOutputBackground outputIsTerminal output)
        hPutStr stderr (grayOutputText toolOutputBackground errorsAreTerminal errors)
        unless (null output || last output == '\n') (putStrLn "")
        hFlush stdout
        hFlush stderr
        pure (output ++ errors ++ exitSuffix exitCode)
    )
      `catch` \(err :: IOException) -> do
        hPutStrLn stderr ("sh: " ++ show err)
        pure ("sh: " ++ show err)
  where
    exitSuffix ExitSuccess = ""
    exitSuffix (ExitFailure code) = "\n[exit " ++ show code ++ "]\n"

changeDirectory :: Maybe String -> IO ()
changeDirectory target = do
  home <- fromMaybe "/" <$> lookupEnv "HOME"
  setCurrentDirectory (resolveDirectoryTarget home target)
    `catch` \(err :: IOException) -> hPutStrLn stderr ("cd: " ++ show err)

-- | Resolve an optional @cd@ target against the home directory.
resolveDirectoryTarget :: FilePath -> Maybe String -> FilePath
resolveDirectoryTarget home Nothing = home
resolveDirectoryTarget home (Just target) = expandHome home target

executableCommands :: IO [String]
executableCommands = do
  searchPath <- fromMaybe "" <$> lookupEnv "PATH"
  names <- concat <$> mapM commandsIn (splitSearchPath searchPath)
  pure (withBuiltins names)
  where
    commandsIn dir = do
      entries <- listDirectory dir `catch` \(_ :: IOException) -> pure []
      catMaybes <$> mapM (executableIn dir) entries

    executableIn dir entry = do
      let candidate = dir </> entry
      isDirectory <- doesDirectoryExist candidate `catch` \(_ :: IOException) -> pure True
      permissions <- getPermissions candidate `catch` \(_ :: IOException) -> pure emptyPermissions
      pure $
        if not isDirectory && executable permissions
          then Just entry
          else Nothing

-- | Add builtins and remove duplicate command names.
withBuiltins :: [String] -> [String]
withBuiltins names = nub (builtinCommands ++ names)

builtinCommands :: [String]
builtinCommands = ["cd", "exit", "help", "llm", "pwd", "quit"]