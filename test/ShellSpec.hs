module ShellSpec (tests) where

import Data.List (isInfixOf, nub)
import Hutian.Interrupt (newInterruptFlag)
import Hutian.Shell
  ( executableCommands
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
    , testCase "commands cannot change the parent working directory" $
        withTemporaryDirectory "hutian-shell-cd" $ \directory -> do
          original <- getCurrentDirectory
          flag <- newInterruptFlag
          runShellCommand flag ("cd " ++ directory)
          output <- runShellCommandOutput flag ("cd " ++ directory)
          changed <- getCurrentDirectory
          output @?= ""
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
          @?= ["help", "pwd", "ls"]
    , testCase "shellExitSuffix reports only failures" $ do
        shellExitSuffix ExitSuccess @?= ""
        shellExitSuffix (ExitFailure 3) @?= "\n[exit 3]\n"
    , testProperty "withBuiltins is idempotent and keeps help first" prop_withBuiltins
    ]

prop_withBuiltins :: [String] -> Property
prop_withBuiltins names =
  counterexample (show (once, twice)) $
    once === twice
      .&&. property (nub once === once)
      .&&. property ("help" `elem` once)
  where
    once = withBuiltins names
    twice = withBuiltins once

shellQuote :: String -> String
shellQuote value =
  "'" ++ concatMap quote value ++ "'"
  where
    quote '\'' = "'\\''"
    quote character = [character]
