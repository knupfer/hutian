{-# LANGUAGE ScopedTypeVariables #-}

-- | Shell command execution and command discovery.
module Hutian.Shell
  ( runShellCommand
  , runShellCommandOutput
  , executableCommands
  , withBuiltins
  , shellExitSuffix
  ) where

import Control.Exception (IOException, catch)
import Control.Monad (unless, void)
import Data.List (nub)
import Data.Maybe (catMaybes)
import Hutian.Interrupt (InterruptFlag, withInterruptFlag)
import Hutian.Output (grayOutputText)
import System.Directory
  ( doesDirectoryExist
  , emptyPermissions
  , executable
  , getPermissions
  , listDirectory
  )
import System.Exit (ExitCode (..))
import System.FilePath (getSearchPath, (</>))
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
        putStr (grayOutputText outputIsTerminal output)
        hPutStr stderr (grayOutputText errorsAreTerminal errors)
        unless (null output || last output == '\n') (putStrLn "")
        hFlush stdout
        hFlush stderr
        pure (output ++ errors ++ shellExitSuffix exitCode)
    )
      `catch` \(err :: IOException) -> do
        hPutStrLn stderr ("sh: " ++ show err)
        pure ("sh: " ++ show err)

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
