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
    [ testCase "blank input is ignored" $ do
        classifyTerminalInput "" @?= NoTerminalInput
        classifyTerminalInput "   " @?= NoTerminalInput
        classifyTerminalInput "\n" @?= NoTerminalInput
        classifyTerminalInput " \t\n" @?= NoTerminalInput
    , testCase "a single line is recorded without its trailing newline" $ do
        classifyTerminalInput "echo hi" @?= SingleLineInput "echo hi"
        classifyTerminalInput "echo hi\n" @?= SingleLineInput "echo hi"
    , testCase "multiple lines are recorded exactly as submitted" $ do
        classifyTerminalInput "echo hi\npwd" @?= MultiLineInput "echo hi\npwd"
        classifyTerminalInput "echo hi\npwd\n" @?= MultiLineInput "echo hi\npwd\n"
    , testProperty "classification agrees with the submitted lines" prop_classify
    ]

prop_classify :: String -> Property
prop_classify text =
  case classifyTerminalInput text of
    NoTerminalInput -> property (all isSpace text)
    SingleLineInput line -> property (not (all isSpace text) && lines text == [line])
    MultiLineInput script ->
      property (not (all isSpace text) && length (lines text) > 1 && script == text)
