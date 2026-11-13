module SyntaxSpec (tests) where

import Data.List (isInfixOf)
import Hutian.Syntax (highlightShellLine)
import Test.Tasty
import Test.Tasty.HUnit
import TestSupport (stripAnsi)

tests :: TestTree
tests =
  testGroup
    "syntax"
    [ testCase "shell commands gain ANSI styles without changing their text" $ do
        let rendered = highlightShellLine "if true; then echo \"hi\"; fi"
        assertBool "expected a syntax style" ("\ESC[38;2;" `isInfixOf` rendered)
        stripAnsi rendered @?= "if true; then echo \"hi\"; fi"
    , testCase "empty rows stay empty" $
        highlightShellLine "" @?= ""
    , testCase "control characters are neutralized" $
        stripAnsi (highlightShellLine "a\ESC[31mb") @?= "a [31mb"
    , testCase "tabs are preserved" $
        stripAnsi (highlightShellLine "echo\tvalue") @?= "echo\tvalue"
    ]
