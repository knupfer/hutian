module HighlightSpec (tests) where

import Data.List (isInfixOf)
import Hutian.Highlight (highlightShellLine, sanitizeOutput)
import Test.Tasty
import Test.Tasty.HUnit
import TestSupport (stripAnsi)

tests :: TestTree
tests =
  testGroup
    "highlight"
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
    , testCase "multi-line output keeps newlines but removes escapes" $
        sanitizeOutput "a\ESC[31mb\nc\td\r" @?= "a [31mb\nc\td "
    ]
