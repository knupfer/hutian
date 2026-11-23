{-# LANGUAGE OverloadedStrings #-}

module HighlightSpec (tests) where

import Data.List (isInfixOf)
import Hutian.Highlight (highlightJsonText, highlightShellLine, sanitizeOutput, sanitizeOutputBytes)
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
    , testCase "byte sanitization preserves UTF-8 and layout" $
        sanitizeOutputBytes "a\ESC[31mb\nc\td\195\169" @?= "a [31mb\nc\td\195\169"
    , testCase "JSON tool arguments are highlighted without changing their text" $ do
        let source = "{\"query\":\"haskell\",\"limit\":2}"
            rendered = highlightJsonText source
        assertBool "expected a syntax style" ("\ESC[38;2;" `isInfixOf` rendered)
        stripAnsi rendered @?= source
    , testCase "multi-line JSON keeps its newlines" $ do
        let source = "{\n  \"query\": \"haskell\"\n}"
        stripAnsi (highlightJsonText source) @?= source
    , testCase "invalid JSON falls back to sanitized text" $
        stripAnsi (highlightJsonText "{\"query\":\ESC[31m") @?= "{\"query\": [31m"
    ]
