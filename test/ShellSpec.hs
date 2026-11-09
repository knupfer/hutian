module ShellSpec (tests) where

import Control.Exception (finally)
import Data.List (isInfixOf)
import Hutian.Interrupt (newInterruptFlag)
import Hutian.Shell
  ( changeDirectory
  , executableCommands
  , resolveDirectoryTarget
  , runShellCommand
  , runShellCommandOutput
  , withBuiltins
  )
import System.Directory
  ( Permissions (..)
  , getCurrentDirectory
  , getPermissions
  , removeFile
  , setCurrentDirectory
  , setPermissions
  )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit
import TestSupport (temporaryFilePath, withEnv, withTemporaryDirectory)

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
    , testCase "runShellCommandOutput captures output and exit status" $ do
        flag <- newInterruptFlag
        output <- runShellCommandOutput flag "printf hello"
        output @?= "hello"
        failed <- runShellCommandOutput flag "printf bad >&2; exit 3"
        assertBool "stderr should be captured" ("bad" `isInfixOf` failed)
        assertBool "exit status should be reported" ("[exit 3]" `isInfixOf` failed)
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
            (changeDirectory (Just directory) >> getCurrentDirectory)
              `finally` setCurrentDirectory original
          changed @?= directory
    , testCase "changeDirectory with no arguments uses HOME" $ do
        originalHome <- lookupEnv "HOME"
        originalDirectory <- getCurrentDirectory
        changed <-
          ( do
              setEnv "HOME" originalDirectory
              changeDirectory Nothing
              getCurrentDirectory
          )
            `finally` do
              maybe (unsetEnv "HOME") (setEnv "HOME") originalHome
              setCurrentDirectory originalDirectory
        changed @?= originalDirectory
    , testCase "resolveDirectoryTarget expands home and defaults to it" $ do
        resolveDirectoryTarget "/home/user" Nothing @?= "/home/user"
        resolveDirectoryTarget "/home/user" (Just "~/project") @?= "/home/user/project"
        resolveDirectoryTarget "/home/user" (Just "/tmp") @?= "/tmp"
        resolveDirectoryTarget "/home/user" (Just "a b") @?= "a b"
    , testCase "withBuiltins removes duplicates and keeps builtins" $
        withBuiltins ["pwd", "ls", "ls"]
          @?= ["cd", "exit", "help", "llm", "pwd", "quit", "ls"]
    ]

shellQuote :: String -> String
shellQuote value =
  "'" ++ concatMap quote value ++ "'"
  where
    quote '\'' = "'\\''"
    quote character = [character]