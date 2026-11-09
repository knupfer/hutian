module CommandSpec (tests) where

import Hutian.Command
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "command"
    [ testCase "empty and whitespace-only lines are empty commands" $ do
        parseCommandLine "" @?= CommandEmpty
        parseCommandLine "   " @?= CommandEmpty
    , testCase "exit and quit stop the shell" $ do
        parseCommandLine "exit" @?= CommandExit
        parseCommandLine "quit" @?= CommandExit
        parseCommandLine "exit now" @?= CommandExit
    , testCase "help and pwd are recognized regardless of extra arguments" $ do
        parseCommandLine "help" @?= CommandHelp
        parseCommandLine "help me" @?= CommandHelp
        parseCommandLine "pwd" @?= CommandPrintDirectory
        parseCommandLine "pwd extra" @?= CommandPrintDirectory
    , testCase "cd keeps its optional target" $ do
        parseCommandLine "cd" @?= CommandChangeDirectory Nothing
        parseCommandLine "cd /tmp" @?= CommandChangeDirectory (Just "/tmp")
        parseCommandLine "cd a b" @?= CommandChangeDirectory (Just "a b")
    , testCase "llm keeps its optional prompt" $ do
        parseCommandLine "llm" @?= CommandLlm Nothing
        parseCommandLine "llm explain this" @?= CommandLlm (Just "explain this")
    , testCase "unknown lines are passed to the shell unchanged" $ do
        parseCommandLine "ls -la" @?= CommandShell "ls -la"
        parseCommandLine "  printf 'hi'\t" @?= CommandShell "  printf 'hi'\t"
        parseCommandLine "exitcode" @?= CommandShell "exitcode"
    , testCase "terminal input is classified by line count" $ do
        classifyTerminalInput "" @?= NoTerminalInput
        classifyTerminalInput "echo hi" @?= SingleLineInput "echo hi"
        classifyTerminalInput "echo hi\n" @?= SingleLineInput "echo hi"
        classifyTerminalInput "echo hi\npwd" @?= MultiLineInput "echo hi\npwd"
        classifyTerminalInput "\n" @?= SingleLineInput ""
    ]
