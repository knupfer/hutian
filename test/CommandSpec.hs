module CommandSpec (tests) where

import Data.Char (isSpace)
import Hutian.Command
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
    , testProperty "parseCommandLine preserves shell input and classifies builtins" prop_parseCommandLine
    ]

prop_parseCommandLine :: String -> Property
prop_parseCommandLine text =
  case parseCommandLine text of
    CommandShell line -> property (line == text)
    CommandEmpty -> property (all isSpace text)
    CommandHelp -> property (words text == ["help"])
