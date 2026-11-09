module ShellSpec (tests) where

import Control.Exception (finally)
import Hutian.Interrupt (newInterruptFlag)
import Hutian.Shell (changeDirectory, executableCommands, runShellCommand)
import System.Directory
  ( Permissions (..)
  , createDirectory
  , getCurrentDirectory
  , getPermissions
  , getTemporaryDirectory
  , removeDirectoryRecursive
  , removeFile
  , setCurrentDirectory
  , setPermissions
  )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "shell"
    [ testCase "runShellCommand executes through /bin/sh" $ do
        flag <- newInterruptFlag
        path <- temporaryFilePath "hutian-shell-output"
        runShellCommand flag ("printf hello > " ++ shellQuote path)
        contents <- readFile path
        contents @?= "hello"
        removeFile path
    , testCase "executableCommands includes PATH entries and builtins" $
        withTemporaryDirectory "hutian-shell-path" $ \directory -> do
          let script = directory </> "test-command"
          writeFile script "#!/bin/sh\n"
          permissions <- getPermissions script
          setPermissions script (permissions { executable = True })
          withEnv "PATH" directory $ do
            commands <- executableCommands
            assertBool "custom command missing" ("test-command" `elem` commands)
            assertBool "builtin missing" ("pwd" `elem` commands)
    , testCase "changeDirectory changes the working directory" $
        withTemporaryDirectory "hutian-shell-cd" $ \directory -> do
          original <- getCurrentDirectory
          changed <-
            (changeDirectory [directory] >> getCurrentDirectory)
              `finally` setCurrentDirectory original
          changed @?= directory
    , testCase "changeDirectory with no arguments uses HOME" $ do
        originalHome <- lookupEnv "HOME"
        originalDirectory <- getCurrentDirectory
        changed <-
          ( do
              setEnv "HOME" originalDirectory
              changeDirectory []
              getCurrentDirectory
          )
            `finally` do
              maybe (unsetEnv "HOME") (setEnv "HOME") originalHome
              setCurrentDirectory originalDirectory
        changed @?= originalDirectory
    ]

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

withEnv :: String -> String -> IO a -> IO a
withEnv name value action = do
  original <- lookupEnv name
  setEnv name value
  action `finally` maybe (unsetEnv name) (setEnv name) original

shellQuote :: String -> String
shellQuote value =
  "'" ++ concatMap quote value ++ "'"
  where
    quote '\'' = "'\\''"
    quote character = [character]