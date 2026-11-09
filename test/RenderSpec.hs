module RenderSpec (tests) where

import Data.List (isInfixOf)
import Hutian.Editor (PromptMode (..), cursorPosition, initialPrompt, setPromptText)
import Hutian.Render
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "render"
    [ testCase "term prompt includes a prefix, background, and cursor position" $ do
        let prompt = setPromptText "abc" initialPrompt
            (output, position) = promptRenderText "~ $ " TermMode 0 80 prompt
        assertBool "term background" ("\ESC[48;2;0;0;40m" `isInfixOf` output)
        assertBool "teal prefix" ("\ESC[38;2;0;255;200m" `isInfixOf` output)
        assertBool "prefix text" ("~ $ " `isInfixOf` output)
        assertBool "content text" ("abc" `isInfixOf` output)
        position @?= cursorPosition 80 "~ $ " prompt
        position @?= (0, 7)
    , testCase "llm prompt omits the term prefix" $ do
        let prompt = setPromptText "hello" initialPrompt
            (output, _) = promptRenderText "~ $ " LlmMode 0 80 prompt
        assertBool "llm background" ("\ESC[48;2;24;0;24m" `isInfixOf` output)
        assertBool "no term prefix" (not ("~ $ " `isInfixOf` output))
    , testCase "rendering moves up by the previous cursor row" $ do
        let prompt = setPromptText "abc" initialPrompt
            (output, _) = promptRenderText "" TermMode 2 80 prompt
        assertBool "cursor up sequence" ("\ESC[2A" `isInfixOf` output)
    , testCase "moveCursorText emits relative movement" $ do
        moveCursorText (3, 0) (1, 4) @?= "\ESC[2A\r\ESC[4C"
        moveCursorText (1, 0) (3, 2) @?= "\ESC[2B\r\ESC[2C"
        moveCursorText (2, 0) (2, 0) @?= "\r"
        moveCursorText (2, 0) (2, 1) @?= "\r\ESC[1C"
    , testCase "startPromptText sets the mode background and clears the line" $ do
        startPromptText TermMode @?= "\ESC[48;2;0;0;40m\ESC[K"
        startPromptText LlmMode @?= "\ESC[48;2;24;0;24m\ESC[K"
    ]
