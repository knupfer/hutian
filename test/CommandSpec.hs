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
    , testCase "help is the only term-mode builtin" $ do
        parseCommandLine "help" @?= CommandHelp
        parseCommandLine "  help  " @?= CommandHelp
    , testCase "everything else is passed to the shell unchanged" $ do
        parseCommandLine "help me" @?= CommandShell "help me"
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
    ]
