module ShellSpec (tests) where

import Data.List (isInfixOf)
import Hutian.Shell
  ( CommandResult (..)
  , commandResultText
  , executableCommands
  , runShellCommand
  , runShellCommandOutput
  , shellExitSuffix
  )
import System.Directory
  ( Permissions (..)
  , getCurrentDirectory
  , getPermissions
  , removeFile
  , setPermissions
  )
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit
import TestSupport (temporaryFilePath, withEnv, withTemporaryDirectory)

tests :: TestTree
tests =
  testGroup
    "shell"
    [ testCase "runShellCommand executes through /bin/sh" $ do
        path <- temporaryFilePath "hutian-shell-output"
        failure <- runShellCommand ("printf hello > " ++ shellQuote path)
        failure @?= Right ()
        contents <- readFile path
        contents @?= "hello"
        removeFile path
    , testCase "runShellCommandOutput captures output and exit status" $ do
        hello <- runShellCommandOutput "printf hello"
        case hello of
          Left message -> assertFailure message
          Right output -> commandResultText output @?= "hello"
        failed <- runShellCommandOutput "printf bad >&2; exit 3"
        case failed of
          Left message -> assertFailure message
          Right output -> do
            assertBool "stderr should be captured" ("bad" `isInfixOf` commandResultText output)
            assertBool "exit status should be reported" ("[exit 3]" `isInfixOf` commandResultText output)
    , testCase "commandResultText appends stderr and exit status in order" $ do
        commandResultText (CommandResult ExitSuccess "out" "err") @?= "outerr"
        commandResultText (CommandResult (ExitFailure 3) "out" "err")
          @?= "outerr\n[exit 3]\n"
    , testCase "commands cannot change the parent working directory" $
        withTemporaryDirectory "hutian-shell-cd" $ \directory -> do
          original <- getCurrentDirectory
          _ <- runShellCommand ("cd " ++ directory)
          output <- runShellCommandOutput ("cd " ++ directory)
          changed <- getCurrentDirectory
          case output of
            Left message -> assertFailure message
            Right result -> commandResultText result @?= ""
          changed @?= original
    , testCase "executableCommands includes PATH entries without duplicates" $
        withTemporaryDirectory "hutian-shell-path" $ \directory -> do
          let script = directory </> "test-command"
          writeFile script "#!/bin/sh\n"
          permissions <- getPermissions script
          setPermissions script (permissions { executable = True })
          withEnv "PATH" directory $ do
            commands <- executableCommands
            assertBool "custom command missing" ("test-command" `elem` commands)
            commands @?= ["test-command"]
    , testCase "shellExitSuffix reports only failures" $ do
        shellExitSuffix ExitSuccess @?= ""
        shellExitSuffix (ExitFailure 3) @?= "\n[exit 3]\n"
    ]

shellQuote :: String -> String
shellQuote value =
  "'" ++ concatMap quote value ++ "'"
  where
    quote '\'' = "'\\''"
    quote character = [character]
