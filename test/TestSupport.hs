-- | Shared helpers for filesystem, environment, socket, and output tests.
module TestSupport
  ( withEnv
  , temporaryFilePath
  , withTemporaryDirectory
  , withPseudoTerminalOutput
  , stdoutLlmOutput
  , stripAnsi
  , captureStdout
  , captureStderr
  , awaitMVar
  , assertInfix
  , assertNotInfix
  , testJobId
  ) where

import Control.Concurrent.MVar (MVar, newMVar, takeMVar, withMVar)
import Control.Exception (IOException, finally, try)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Hutian.Jobs (JobId, jobIdFromInt)
import Hutian.Json (decodeUtf8Lenient)
import Hutian.Llm (LlmOutput (..))
import Hutian.Context (TurnContext (MainContext))
import Hutian.Verbosity (Verbosity (..))
import System.Directory
  ( createDirectory
  , getTemporaryDirectory
  , removeDirectoryRecursive
  , removeFile
  )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO (Handle, hClose, hFlush, hPutStrLn, openTempFile, stderr, stdout)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.IO
  ( FdOption (..)
  , closeFd
  , createPipe
  , dup
  , dupTo
  , fdToHandle
  , setFdOption
  , stdError
  , stdOutput
  )
import qualified System.Posix.IO.ByteString as PosixBS
import System.Posix.Terminal (openPseudoTerminal)
import System.Posix.Types (Fd)
import System.Timeout (timeout)
import Test.Tasty.HUnit (assertBool)

withEnv :: String -> String -> IO a -> IO a
withEnv name value action = do
  original <- lookupEnv name
  setEnv name value
  action `finally` maybe (unsetEnv name) (setEnv name) original

temporaryFilePath :: String -> IO FilePath
temporaryFilePath template = do
  directory <- getTemporaryDirectory
  (path, handle) <- openTempFile directory template
  hClose handle
  removeFile path
  pure path

withTemporaryDirectory :: String -> (FilePath -> IO a) -> IO a
withTemporaryDirectory template action = do
  path <- temporaryFilePath template
  createDirectory path
  action path `finally` removeDirectoryRecursive path

stripAnsi :: String -> String
stripAnsi [] = []
stripAnsi ('\ESC' : '[' : rest) = stripAnsi (dropSequence rest)
  where
    dropSequence [] = []
    dropSequence (character : characters)
      | character >= '@' && character <= '~' = characters
      | otherwise = dropSequence characters
stripAnsi (character : rest) = character : stripAnsi rest

-- | Stream LLM output to the test process's own stdout and stderr, so the
-- capture helpers can observe it.
stdoutLlmOutput :: LlmOutput
stdoutLlmOutput =
  LlmOutput
    { llmOutputWrite = putStr
    , llmOutputErrorLine = hPutStrLn stderr
    , llmOutputContext = MainContext
    , llmOutputVerbosity = VerbosityHigh
    , llmOutputTranscript = Nothing
    }

-- | Captures install a process-wide redirection, so two of them must never be
-- active at once: output from one test would otherwise land in the pipe that
-- another test is reading. The reporter is deferred until all tests are done
-- (see "TestReporter"), so it cannot interleave either.
{-# NOINLINE captureLock #-}
captureLock :: MVar ()
captureLock = unsafePerformIO (newMVar ())

withCaptureLock :: IO a -> IO a
withCaptureLock action = withMVar captureLock (const action)

-- | Capture everything written to stdout while an action runs. Used by the LLM
-- streaming tests because the shell renders model output directly.
captureStdout :: IO a -> IO (a, String)
captureStdout action = withCaptureLock (captureOutput stdout stdOutput action)

-- | Capture everything written to stderr while an action runs.
captureStderr :: IO a -> IO (a, String)
captureStderr action = withCaptureLock (captureOutput stderr stdError action)

captureOutput :: Handle -> Fd -> IO a -> IO (a, String)
captureOutput handle outputFd action = do
  (readFd, writeFd) <- createPipe
  savedFd <- dup outputFd
  -- Flush before redirecting: output buffered in the handle must not land in
  -- the capture.
  hFlush handle
  _ <- dupTo writeFd outputFd
  closeFd writeFd
  result <-
    action
      `finally` do
        hFlush handle
        _ <- dupTo savedFd outputFd
        closeFd savedFd
  hFlush handle
  output <- readPipe readFd
  pure (result, output)

readPipe :: Fd -> IO String
readPipe readFd = do
  handle <- fdToHandle readFd
  contents <- BS.hGetContents handle
  hClose handle
  pure (decodeUtf8Lenient contents)

-- | Run an action with stdout and stderr attached to one pseudo-terminal, so
-- terminal-only presentation paths can be observed. The callback reads
-- everything written to the terminal while the action runs.
withPseudoTerminalOutput :: (IO String -> IO a) -> IO a
withPseudoTerminalOutput action = withCaptureLock $ do
  (master, slave) <- openPseudoTerminal
  setFdOption master NonBlockingRead True
  savedOutput <- dup stdOutput
  savedError <- dup stdError
  -- Flush before redirecting: output buffered in the handles must not land on
  -- the pseudo-terminal.
  hFlush stdout
  hFlush stderr
  _ <- dupTo slave stdOutput
  _ <- dupTo slave stdError
  closeFd slave
  action (readAvailable master)
    `finally` do
      hFlush stdout
      hFlush stderr
      _ <- dupTo savedOutput stdOutput
      _ <- dupTo savedError stdError
      closeFd savedOutput
      closeFd savedError
  where
    readAvailable master = go []
      where
        go chunks = do
          chunk <- try (PosixBS.fdRead master 4096) :: IO (Either IOException BS.ByteString)
          case chunk of
            Right bytes | not (BS.null bytes) -> go (bytes : chunks)
            _ -> pure (decodeUtf8Lenient (BS.concat (reverse chunks)))

-- | Take a value, failing the test instead of hanging if it never arrives.
awaitMVar :: String -> MVar a -> IO a
awaitMVar message variable = do
  result <- timeout 3000000 (takeMVar variable)
  maybe (ioError (userError message)) pure result

assertInfix :: BS.ByteString -> BS.ByteString -> IO ()
assertInfix needle haystack =
  assertBool
    ("expected " ++ show needle ++ " in " ++ show haystack)
    (needle `BS.isInfixOf` haystack)

assertNotInfix :: BS.ByteString -> BS.ByteString -> IO ()
assertNotInfix needle haystack =
  assertBool
    ("did not expect " ++ show needle ++ " in " ++ show haystack)
    (not (needle `BS.isInfixOf` haystack))

-- | A positive job id for tests that compare model or runtime output.
testJobId :: Int -> JobId
testJobId value = fromMaybe (error "test job id must be positive") (jobIdFromInt value)
