module RenderSpec (tests) where

import Data.List (isInfixOf, isSuffixOf)
import Hutian.Editor (PromptMode (..), cursorPosition, initialPrompt, setPromptText)
import Hutian.Output (grayOutputText)
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
        startPromptText ToolMode @?= "\ESC[48;2;40;0;0m\ESC[K"
    , testCase "tool prompts use a dark red background and bright red path" $ do
        let prompt = setPromptText "ls" initialPrompt
            (output, _) = promptRenderText "~ $ " ToolMode 0 80 prompt
        assertBool "tool background" ("\ESC[48;2;40;0;0m" `isInfixOf` output)
        assertBool "bright red path" ("\ESC[38;2;255;0;0m" `isInfixOf` output)
        assertBool "path text" ("~ $ " `isInfixOf` output)
        assertBool "command text" ("ls" `isInfixOf` output)
    , testCase "toolPromptText renders a command and resets colors" $ do
        let output = toolPromptText "~ $ " 80 "echo hi"
        assertBool "tool background" ("\ESC[48;2;40;0;0m" `isInfixOf` output)
        assertBool "bright red path" ("\ESC[38;2;255;0;0m" `isInfixOf` output)
        assertBool "command text" ("echo hi" `isInfixOf` output)
        assertBool "resets colors" ("\ESC[0m" `isInfixOf` output)
    , testCase "toolPromptText resets the background before the final newline" $ do
        let output = toolPromptText "~ $ " 80 "echo hi"
            (beforeNewline, afterNewline) = splitLastNewline output
        assertBool
          "reset must come before the newline"
          (resetSequence `isSuffixOf` beforeNewline)
        assertBool
          "nothing after the newline may change colors"
          (not ("\ESC" `isInfixOf` afterNewline))
    , testCase "multiline toolPromptText resets before the final newline" $ do
        let output = toolPromptText "~ $ " 80 "echo one\necho two"
            (beforeNewline, afterNewline) = splitLastNewline output
        assertBool
          "reset must come before the final newline"
          (resetSequence `isSuffixOf` beforeNewline)
        assertBool
          "nothing after the final newline may change colors"
          (not ("\ESC" `isInfixOf` afterNewline))
    , testCase "grayOutputText wraps terminal output and resets" $
        grayOutputText True "hello" @?= grayForeground ++ "hello" ++ reset
    , testCase "grayOutputText leaves non-terminal output unchanged" $
        grayOutputText False "hello" @?= "hello"
    , testCase "grayOutputText leaves empty output unchanged" $
        grayOutputText True "" @?= ""
    ]

resetSequence :: String
resetSequence = "\ESC[0m"

splitLastNewline :: String -> (String, String)
splitLastNewline text =
  case break (== '\n') (reverse text) of
    (afterNewline, '\n' : beforeNewline) -> (reverse beforeNewline, reverse afterNewline)
    _ -> (text, "")
