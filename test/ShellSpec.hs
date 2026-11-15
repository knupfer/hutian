module ShellSpec (tests) where

import Data.List (isInfixOf, nub)
import Hutian.Shell
  ( CommandResult (..)
  , commandResultText
  , executableCommands
  , runShellCommand
  , runShellCommandOutput
  , shellExitSuffix
  , withBuiltins
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
import Test.QuickCheck (Property, (.&&.), (===), counterexample, property)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)
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
    , testCase "executableCommands includes PATH entries and builtins" $
        withTemporaryDirectory "hutian-shell-path" $ \directory -> do
          let script = directory </> "test-command"
          writeFile script "#!/bin/sh\n"
          permissions <- getPermissions script
          setPermissions script (permissions { executable = True })
          withEnv "PATH" directory $ do
            commands <- executableCommands
            assertBool "custom command missing" ("test-command" `elem` commands)
            assertBool "builtin missing" ("help" `elem` commands)
    , testCase "withBuiltins removes duplicates and keeps builtins" $
        withBuiltins ["pwd", "ls", "ls"]
          @?= ["help", "jobs", "job", "pwd", "ls"]
    , testCase "shellExitSuffix reports only failures" $ do
        shellExitSuffix ExitSuccess @?= ""
        shellExitSuffix (ExitFailure 3) @?= "\n[exit 3]\n"
    , testProperty "withBuiltins is idempotent and keeps every builtin first" prop_withBuiltins
    ]

prop_withBuiltins :: [String] -> Property
prop_withBuiltins names =
  counterexample (show (once, twice)) $
    once === twice
      .&&. property (nub once === once)
      .&&. property (take 3 once === ["help", "jobs", "job"])
  where
    once = withBuiltins names
    twice = withBuiltins once

shellQuote :: String -> String
shellQuote value =
  "'" ++ concatMap quote value ++ "'"
  where
    quote '\'' = "'\\''"
    quote character = [character]
