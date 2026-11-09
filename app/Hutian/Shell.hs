{-# LANGUAGE ScopedTypeVariables #-}

-- | Shell command execution and command discovery.
module Hutian.Shell
  ( runShellCommand
  , executableCommands
  , changeDirectory
  ) where

import Control.Exception (IOException, catch)
import Control.Monad (void)
import Data.List (nub)
import Data.Maybe (catMaybes, fromMaybe)
import Hutian.Interrupt (InterruptFlag, withInterruptFlag)
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
import System.FilePath (splitSearchPath, (</>))
import System.IO (hPutStrLn, stderr)
import System.Process (createProcess, delegate_ctlc, proc, waitForProcess)

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

changeDirectory :: [String] -> IO ()
changeDirectory args = do
  home <- fromMaybe "/" <$> lookupEnv "HOME"
  let target = expandHome home (if null args then home else unwords args)
  setCurrentDirectory target
    `catch` \(err :: IOException) -> hPutStrLn stderr ("cd: " ++ show err)

executableCommands :: IO [String]
executableCommands = do
  searchPath <- fromMaybe "" <$> lookupEnv "PATH"
  names <- concat <$> mapM commandsIn (splitSearchPath searchPath)
  pure (nub (builtinCommands ++ names))
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

builtinCommands :: [String]
builtinCommands = ["cd", "exit", "help", "llm", "pwd", "quit"]