{-# LANGUAGE ScopedTypeVariables #-}

-- | Shell command execution and command discovery. Display decisions are left
-- to the caller; captured output is returned as data.
module Hutian.Shell
  ( CommandResult (..)
  , commandResultText
  , executableCommands
  , runShellCommand
  , runShellCommandOutput
  , shellExitSuffix
  , withBuiltins
  ) where

import Control.Exception (IOException, catch)
import Control.Monad (void)
import Data.List (nub)
import Data.Maybe (catMaybes)
import System.Directory
  ( doesDirectoryExist
  , emptyPermissions
  , executable
  , getPermissions
  , listDirectory
  )
import System.Exit (ExitCode (..))
import System.FilePath (getSearchPath, (</>))
import System.Process
  ( createProcess
  , delegate_ctlc
  , proc
  , readCreateProcessWithExitCode
  , waitForProcess
  )

-- | The result of a command whose output was captured.
data CommandResult = CommandResult
  { commandExitCode :: ExitCode
  , commandStdout :: String
  , commandStderr :: String
  }
  deriving stock (Eq, Show)

-- | Run a command with inherited stdio. Returns an error message only when the
-- process could not be started.
runShellCommand :: String -> IO (Either String ())
runShellCommand line =
  ( do
      (_, _, _, processHandle) <-
        createProcess
          (proc "/bin/sh" ["-c", line])
            { delegate_ctlc = False
            }
      void (waitForProcess processHandle)
      pure (Right ())
  )
    `catch` \(err :: IOException) -> pure (Left ("sh: " ++ show err))

-- | Run a command and capture its output for the model.
runShellCommandOutput :: String -> IO (Either String CommandResult)
runShellCommandOutput line =
  ( do
      (exitCode, output, errors) <-
        readCreateProcessWithExitCode (proc "/bin/sh" ["-c", line]) ""
      pure (Right (CommandResult exitCode output errors))
  )
    `catch` \(err :: IOException) -> pure (Left ("sh: " ++ show err))

-- | The text sent back to the model for a captured command.
commandResultText :: CommandResult -> String
commandResultText result =
  commandStdout result
    ++ commandStderr result
    ++ shellExitSuffix (commandExitCode result)

-- | The trailing text that reports a non-zero command exit status.
shellExitSuffix :: ExitCode -> String
shellExitSuffix ExitSuccess = ""
shellExitSuffix (ExitFailure code) = "\n[exit " ++ show code ++ "]\n"

executableCommands :: IO [String]
executableCommands = withBuiltins . concat <$> (getSearchPath >>= mapM commandsIn)
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
builtinCommands = ["help"]
