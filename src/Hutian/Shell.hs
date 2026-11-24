{-# LANGUAGE ScopedTypeVariables #-}

-- | Shell command execution and command discovery. Display decisions are left
-- to the caller; captured output is returned as data.
module Hutian.Shell
  ( CommandResult (CommandResult, commandStdout, commandStderr)
  , commandResultText
  , executableCommands
  , runShellCommand
  , runShellCommandOutput
  , runShellCommandStreaming
  , shellExitSuffix
  , shellProcess
  ) where

import Control.Concurrent
  ( forkIO
  , newEmptyMVar
  , putMVar
  , takeMVar
  , threadDelay
  )
import Control.Exception (IOException, catch, finally)
import Control.Monad (void)
import qualified Data.ByteString as BS
import Data.List (nub)
import Data.Maybe (catMaybes, isJust)
import System.Directory
  ( doesDirectoryExist
  , emptyPermissions
  , executable
  , getPermissions
  , listDirectory
  )
import System.Exit (ExitCode (..))
import System.FilePath (getSearchPath, (</>))
import System.IO (Handle, hClose, hFlush)
import System.Process
  ( CreateProcess (..)
  , ProcessHandle
  , StdStream (CreatePipe)
  , createProcess
  , delegate_ctlc
  , getProcessExitCode
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
runShellCommand :: FilePath -> String -> IO (Either String ())
runShellCommand workingDirectory line =
  shellFailure $ do
    (_, _, _, processHandle) <-
      createProcess
        (shellProcess workingDirectory line)
          { delegate_ctlc = False
          }
    void (waitForProcess processHandle)
    pure (Right ())

-- | Run a command with captured stdio and stream each chunk through the given
-- byte transform to the supplied handles. Callers use this when output is not
-- a terminal: escape sequences can then be neutralized without giving up
-- streaming.
runShellCommandStreaming
  :: (BS.ByteString -> BS.ByteString)
  -> Handle
  -> Handle
  -> FilePath
  -> String
  -> IO (Either String ())
runShellCommandStreaming transformBytes outputDestination errorDestination workingDirectory line =
  shellFailure $ do
    (_, maybeOutput, maybeError, processHandle) <-
      createProcess
        ( (shellProcess workingDirectory line)
            { std_out = CreatePipe
            , std_err = CreatePipe
            , delegate_ctlc = False
            }
        )
    case (maybeOutput, maybeError) of
      (Just outputHandle, Just errorHandle) -> do
        outputDone <- newEmptyMVar
        errorDone <- newEmptyMVar
        _ <-
          forkIO $
            copyPipe processHandle transformBytes outputHandle outputDestination
              `finally` putMVar outputDone ()
        _ <-
          forkIO $
            copyPipe processHandle transformBytes errorHandle errorDestination
              `finally` putMVar errorDone ()
        _ <- waitForProcess processHandle
        takeMVar outputDone
        takeMVar errorDone
        pure (Right ())
      _ -> do
        _ <- waitForProcess processHandle
        pure (Left "sh: could not capture output")

-- | Drain one pipe until the child has exited and no more output is waiting.
-- End-of-file alone is not enough: a background process can inherit the pipe
-- and keep it open long after the command itself returns.
copyPipe
  :: ProcessHandle
  -> (BS.ByteString -> BS.ByteString)
  -> Handle
  -> Handle
  -> IO ()
copyPipe processHandle transformBytes source destination =
  go `catch` \(_ :: IOException) -> pure ()
  where
    go = do
      chunk <- BS.hGetNonBlocking source copyChunkBytes
      if not (BS.null chunk)
        then do
          BS.hPut destination (transformBytes chunk)
          hFlush destination
          go
        else do
          exited <- isJust <$> getProcessExitCode processHandle
          if exited
            then hClose source
            else threadDelay copyPollMicros >> go

copyChunkBytes :: Int
copyChunkBytes = 4096

-- | How often a pipe with no data checks whether the child has exited.
copyPollMicros :: Int
copyPollMicros = 20000

-- | Run a command and capture its output for the model.
runShellCommandOutput :: FilePath -> String -> IO (Either String CommandResult)
runShellCommandOutput workingDirectory line =
  shellFailure $ do
    (exitCode, output, errors) <-
      readCreateProcessWithExitCode (shellProcess workingDirectory line) ""
    pure (Right (CommandResult exitCode output errors))

-- | Run a shell action, turning a synchronous IO failure into a message for
-- the model. Every shell entry point reports failures the same way.
shellFailure :: IO (Either String a) -> IO (Either String a)
shellFailure action =
  action `catch` \(err :: IOException) -> pure (Left ("sh: " ++ show err))

-- | Commands start in the session directory, but the long-lived shell must not
-- keep its own working directory there. gVisor caches the directory listing of
-- a mount while a process is inside it, even with the dentry cache disabled, so
-- a shell that stayed in @/source@ would hide files created on the host after
-- startup. Each command gets its own fresh lookup instead.
shellProcess :: FilePath -> String -> CreateProcess
shellProcess workingDirectory line =
  (proc "/bin/sh" ["-c", line]) { cwd = Just workingDirectory }

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
executableCommands = nub . concat <$> (getSearchPath >>= mapM commandsIn)
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
