module CommandSpec (tests) where

import Data.Char (isSpace)
import Hutian.Command
import Hutian.Jobs (JobId (..), defaultJobOutputLines)
import Test.QuickCheck (Property, property)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "command"
    [ testCase "empty and whitespace-only lines are empty commands" $ do
        parseCommandLine "" @?= CommandEmpty
        parseCommandLine "   " @?= CommandEmpty
    , testCase "help, jobs, and job are term-mode builtins" $ do
        parseCommandLine "help" @?= CommandHelp
        parseCommandLine "  help  " @?= CommandHelp
        parseCommandLine "jobs" @?= CommandJobs
        parseCommandLine "  jobs  " @?= CommandJobs
    , testCase "job output takes an id and an optional line count" $ do
        parseCommandLine "job 1"
          @?= CommandJobOutput (JobId 1) defaultJobOutputLines
        parseCommandLine "  job  2  20  " @?= CommandJobOutput (JobId 2) 20
        parseCommandLine "job 3 500" @?= CommandJobOutput (JobId 3) 500
    , testCase "job kill and job start are parsed" $ do
        parseCommandLine "job kill 4" @?= CommandJobKill (JobId 4)
        parseCommandLine "  job   kill   5 " @?= CommandJobKill (JobId 5)
        parseCommandLine "job start sleep 10" @?= CommandJobStart "sleep 10"
        parseCommandLine "  job   start   echo hi  " @?= CommandJobStart "echo hi"
    , testCase "malformed job commands show usage" $ do
        parseCommandLine "job" @?= CommandJobUsage
        parseCommandLine "job 0" @?= CommandJobUsage
        parseCommandLine "job -1" @?= CommandJobUsage
        parseCommandLine "job nope" @?= CommandJobUsage
        parseCommandLine "job listing" @?= CommandJobUsage
        parseCommandLine "job startx" @?= CommandJobUsage
        parseCommandLine "job 1 nope" @?= CommandJobUsage
        parseCommandLine "job kill" @?= CommandJobUsage
        parseCommandLine "job kill 0" @?= CommandJobUsage
        parseCommandLine "job start" @?= CommandJobUsage
        parseCommandLine "job start   " @?= CommandJobUsage
    , testCase "everything else is passed to the shell unchanged" $ do
        parseCommandLine "help me" @?= CommandShell "help me"
        parseCommandLine "jobs now" @?= CommandShell "jobs now"
        parseCommandLine "exit" @?= CommandShell "exit"
        parseCommandLine "quit" @?= CommandShell "quit"
        parseCommandLine "llm explain this" @?= CommandShell "llm explain this"
        parseCommandLine "cd /tmp" @?= CommandShell "cd /tmp"
        parseCommandLine "pwd" @?= CommandShell "pwd"
        parseCommandLine "ls -la" @?= CommandShell "ls -la"
        parseCommandLine "  printf 'hi'\t" @?= CommandShell "  printf 'hi'\t"
        parseCommandLine "exitcode" @?= CommandShell "exitcode"
    , testCase "terminal input is classified by line count" $ do
        classifyTerminalInput "" @?= NoTerminalInput
        classifyTerminalInput "echo hi" @?= SingleLineInput "echo hi"
        classifyTerminalInput "echo hi\n" @?= SingleLineInput "echo hi"
        classifyTerminalInput "echo hi\npwd" @?= MultiLineInput "echo hi\npwd"
        classifyTerminalInput "\n" @?= SingleLineInput ""
    , testProperty "parseCommandLine preserves shell input and classifies builtins" prop_parseCommandLine
    ]

prop_parseCommandLine :: String -> Property
prop_parseCommandLine text =
  case parseCommandLine text of
    CommandShell line -> property (line == text)
    CommandEmpty -> property (all isSpace text)
    _ -> property (builtinFirstWord (words text))
  where
    builtinFirstWord ("help" : _) = True
    builtinFirstWord ["jobs"] = True
    builtinFirstWord ("job" : _) = True
    builtinFirstWord _ = False
