module RenderSpec (tests) where

import Data.List (isInfixOf, isSuffixOf)
import Hutian.Ansi (darkenColour, rgb)
import qualified Hutian.Ansi as Ansi
import Hutian.Editor (PromptMode (..), cursorPosition, initialPrompt, setPromptText)
import Hutian.Output (grayOutputText, outputLabelText)
import Hutian.Render
import Test.Tasty
import Test.Tasty.HUnit
import TestSupport (stripAnsi)

tests :: TestTree
tests =
  testGroup
    "render"
    [ testCase "term prompt puts the shell label on its own line" $ do
        let prompt = setPromptText "abc" initialPrompt
            (output, position) = promptRenderText termLine TermMode 0 80 prompt
        assertBool "shell label colour matches ask" (boldVibrantPink `isInfixOf` output)
        assertBool "label text" ("shell:" `isInfixOf` output)
        assertBool "default colour resumes after the label" (resetSequence `isInfixOf` output)
        assertBool "label sits above the input" ("shell:\nabc" `isInfixOf` stripAnsi output)
        position @?= cursorPosition 80 termLine prompt
        position @?= (1, 3)
    , testCase "llm prompt shows a bold pink ask label on its own line" $ do
        let prompt = setPromptText "hello" initialPrompt
            (output, position) = promptRenderText termLine LlmMode 0 80 prompt
        assertBool "bold pink ask label" ((boldVibrantPink ++ "ask:") `isInfixOf` output)
        assertBool "no shell label" (not ("shell:" `isInfixOf` output))
        assertBool "label sits above the input" ("ask:\nhello" `isInfixOf` stripAnsi output)
        position @?= cursorPosition 80 "ask:\n" prompt
        position @?= (1, 5)
    , testCase "llm prompt highlights markdown while typing" $ do
        let source = "# Heading\nUse **bold** and `code`.\n- item"
            prompt = setPromptText source initialPrompt
            (output, position) = promptRenderText termLine LlmMode 0 80 prompt
            stripped = stripAnsi output
        assertBool "heading style" ("\ESC[1;38;2;137;180;250m" `isInfixOf` output)
        assertBool "bold style" ("\ESC[1;38;2;249;226;175m" `isInfixOf` output)
        assertBool "code style" ("\ESC[38;2;166;227;161m" `isInfixOf` output)
        assertBool "list marker style" ("\ESC[1;38;2;250;179;135m" `isInfixOf` output)
        assertBool "preserves the markdown text" (all (`isInfixOf` stripped) (lines source))
        position @?= cursorPosition 80 "ask:\n" prompt
        position @?= (3, 6)
    , testCase "llm prompt highlights fenced code while typing" $ do
        let source = "```\nmain = putStrLn \"hi\"\n```"
            prompt = setPromptText source initialPrompt
            (output, _) = promptRenderText termLine LlmMode 0 80 prompt
        assertBool "fence style" ("\ESC[38;2;166;227;161m" `isInfixOf` output)
        assertBool "code style" ("\ESC[38;2;166;227;161m" `isInfixOf` output)
        assertBool "preserves the markdown text" (all (`isInfixOf` stripAnsi output) (lines source))
    , testCase "term prompt does not apply markdown highlighting" $ do
        let prompt = setPromptText "# not markdown" initialPrompt
            (output, _) = promptRenderText termLine TermMode 0 80 prompt
        assertBool "no heading style" (not ("\ESC[1;38;2;137;180;250m" `isInfixOf` output))
    , testCase "prompt rendering uses no background colors" $ do
        let termPrompt = setPromptText "abc" initialPrompt
            (termOutput, _) = promptRenderText termLine TermMode 0 80 termPrompt
            llmPrompt = setPromptText "abc" initialPrompt
            (llmOutput, _) = promptRenderText termLine LlmMode 0 80 llmPrompt
            toolPrompt = setPromptText "ls" initialPrompt
            (toolOutput, _) = promptRenderText toolLine ToolMode 0 80 toolPrompt
        assertBool "term has no background" (not ("\ESC[48;" `isInfixOf` termOutput))
        assertBool "llm has no background" (not ("\ESC[48;" `isInfixOf` llmOutput))
        assertBool "tool has no background" (not ("\ESC[48;" `isInfixOf` toolOutput))
    , testCase "prompt rows are not padded with spaces" $ do
        let prompt = setPromptText "abc" initialPrompt
            (output, _) = promptRenderText termLine TermMode 0 80 prompt
        assertBool "no trailing space padding" (not ("abc " `isInfixOf` output))
        assertBool "erases the rest of the line instead" ("abc\ESC[K" `isInfixOf` output)
    , testCase "llm prompt rows are not padded with spaces" $ do
        let prompt = setPromptText "hello" initialPrompt
            (output, _) = promptRenderText termLine LlmMode 0 80 prompt
        assertBool "no trailing space padding" (not ("hello " `isInfixOf` output))
        assertBool "erases the rest of the line instead" ("hello\ESC[K" `isInfixOf` output)
    , testCase "prompt clears below the rendered rows" $ do
        let prompt = setPromptText "abc" initialPrompt
            (output, _) = promptRenderText termLine TermMode 0 80 prompt
        assertBool "clears below the prompt" ("\ESC[0J" `isInfixOf` output)
    , testCase "rendering moves up by the previous cursor row" $ do
        let prompt = setPromptText "abc" initialPrompt
            (output, _) = promptRenderText "" TermMode 2 80 prompt
        assertBool "cursor up sequence" ("\ESC[2A" `isInfixOf` output)
    , testCase "moveCursorText emits relative movement" $ do
        moveCursorText (3, 0) (1, 4) @?= "\ESC[2A\r\ESC[4C"
        moveCursorText (1, 0) (3, 2) @?= "\ESC[2B\r\ESC[2C"
        moveCursorText (2, 0) (2, 0) @?= "\r"
        moveCursorText (2, 0) (2, 1) @?= "\r\ESC[1C"
    , testCase "startPromptText clears the line" $
        startPromptText @?= "\ESC[K"
    , testCase "tool prompts use the answer teal on its own line" $ do
        let prompt = setPromptText "ls" initialPrompt
            (output, position) = promptRenderText toolLine ToolMode 0 80 prompt
        assertBool "teal prefix" ((boldTeal ++ "tool shell:") `isInfixOf` output)
        assertBool "label text" ("tool shell:" `isInfixOf` output)
        assertBool "label sits above the command" ("tool shell:\nls" `isInfixOf` stripAnsi output)
        position @?= cursorPosition 80 toolLine prompt
        position @?= (1, 2)
    , testCase "toolPromptText renders a command with the tool label" $ do
        let output = toolPromptText "shell" 80 "echo hi"
        assertBool "teal prefix" ((boldTeal ++ "tool shell:") `isInfixOf` output)
        assertBool "label text" ("tool shell:" `isInfixOf` output)
        assertBool "command text" ("echo hi" `isInfixOf` output)
        assertBool "label sits above the command" ("tool shell:\necho hi" `isInfixOf` stripAnsi output)
        assertBool "resets colors" (resetSequence `isInfixOf` output)
    , testCase "toolPromptText resets before the final newline" $ do
        let output = toolPromptText "shell" 80 "echo hi"
            (beforeNewline, afterNewline) = splitLastNewline output
        assertBool
          "reset must come before the newline"
          (resetSequence `isSuffixOf` beforeNewline)
        assertBool
          "nothing after the newline may change colors"
          (not ("\ESC" `isInfixOf` afterNewline))
    , testCase "multiline toolPromptText resets before the final newline" $ do
        let output = toolPromptText "shell" 80 "echo one\necho two"
            (beforeNewline, afterNewline) = splitLastNewline output
        assertBool
          "reset must come before the newline"
          (resetSequence `isSuffixOf` beforeNewline)
        assertBool
          "nothing after the final newline may change colors"
          (not ("\ESC" `isInfixOf` afterNewline))
    , testCase "output labels are dimmed versions of the mode colours" $ do
        outputForeground TermMode @?= "\ESC[1;38;2;153;12;88m"
        outputForeground ToolMode @?= "\ESC[1;38;2;0;153;120m"
        outputLabelText TermMode True
          @?= outputForeground TermMode ++ "output:" ++ reset
        outputLabelText ToolMode True
          @?= outputForeground ToolMode ++ "tool output:" ++ reset
        assertBool
          "shell and tool labels differ"
          (outputForeground TermMode /= outputForeground ToolMode)
    , testCase "output labels are plain when not interactive" $ do
        outputLabelText TermMode False @?= "output:"
        outputLabelText ToolMode False @?= "tool output:"
    , testCase "grayOutputText wraps terminal output and resets" $
        grayOutputText True "hello"
          @?= grayForeground ++ "hello" ++ reset
    , testCase "grayOutputText leaves non-terminal output unchanged" $
        grayOutputText False "hello" @?= "hello"
    , testCase "grayOutputText leaves empty output unchanged" $
        grayOutputText True "" @?= ""
    , testCase "darkenColour scales RGB channels" $ do
        darkenColour 0.6 (rgb 255 20 147) @?= rgb 153 12 88
        darkenColour 1 (rgb 1 2 3) @?= rgb 1 2 3
    , testCase "darkenStyle scales colours but keeps attributes" $
        Ansi.styleCode
          (Ansi.darkenStyle 0.6 (Ansi.bold <> Ansi.foreground (rgb 255 20 147)))
          @?= Ansi.styleCode
            (Ansi.bold <> Ansi.foreground (rgb 153 12 88))
    , testCase "user cursor is pink and steady" $
        cursorStyleText UserCursor @?= "\ESC]12;#FF1493\a\ESC[2 q"
    , testCase "responding cursor is teal and steady" $
        cursorStyleText RespondingCursor @?= "\ESC]12;#00FFC8\a\ESC[2 q"
    , testCase "cursor blink uses visibility sequences" $ do
        hideCursorText @?= "\ESC[?25l"
        showCursorText @?= "\ESC[?25h"
    , testCase "resetCursorText restores the default cursor" $
        resetCursorText @?= "\ESC[?25h\ESC]112\a\ESC[0 q"
    ]

termLine :: String
termLine = "shell:\n"

toolLine :: String
toolLine = "tool shell:\n"

resetSequence :: String
resetSequence = "\ESC[0m"

splitLastNewline :: String -> (String, String)
splitLastNewline text =
  case break (== '\n') (reverse text) of
    (afterNewline, '\n' : beforeNewline) -> (reverse beforeNewline, reverse afterNewline)
    _ -> (text, "")
