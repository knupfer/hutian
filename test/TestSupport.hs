-- | Shared helpers for filesystem and environment tests.
module TestSupport
  ( withEnv
  , temporaryFilePath
  , withTemporaryDirectory
  ) where

import Control.Exception (finally)
import System.Directory
  ( createDirectory
  , getTemporaryDirectory
  , removeDirectoryRecursive
  , removeFile
  )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO (hClose, openTempFile)

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
