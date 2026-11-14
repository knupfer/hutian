-- | Shared helpers for filesystem, environment, socket, and output tests.
module TestSupport
  ( withEnv
  , temporaryFilePath
  , withTemporaryDirectory
  , stripAnsi
  , captureStdout
  , captureStderr
  , awaitMVar
  , assertInfix
  , assertNotInfix
  ) where

import Control.Concurrent.MVar (MVar, takeMVar)
import Control.Exception (finally)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import System.Directory
  ( createDirectory
  , getTemporaryDirectory
  , removeDirectoryRecursive
  , removeFile
  )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO (Handle, hClose, hFlush, openTempFile, stderr, stdout)
import System.Posix.IO
  ( closeFd
  , createPipe
  , dup
  , dupTo
  , fdToHandle
  , stdError
  , stdOutput
  )
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

-- | Capture everything written to stdout while an action runs. Used by the LLM
-- streaming tests because the shell renders model output directly.
captureStdout :: IO a -> IO (a, String)
captureStdout = captureOutput stdout stdOutput

-- | Capture everything written to stderr while an action runs.
captureStderr :: IO a -> IO (a, String)
captureStderr = captureOutput stderr stdError

captureOutput :: Handle -> Fd -> IO a -> IO (a, String)
captureOutput handle outputFd action = do
  (readFd, writeFd) <- createPipe
  savedFd <- dup outputFd
  _ <- dupTo writeFd outputFd
  closeFd writeFd
  hFlush handle
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
  pure (BS8.unpack contents)

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
