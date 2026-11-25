module ConsoleSpec (tests) where

import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Hutian.Console
  ( consoleWriteErrorLine
  , consoleWriteGrayLine
  , consoleWriteLine
  , consoleWriteHandlerPromptIn
  , consoleWriteSubagentPrompt
  , consoleWriteToolDefinitions
  , withGrayOutput
  )
import Hutian.Context (TurnContext (MainContext))
import Hutian.Render
  ( ToolApproval (..)
  , approvalDecisionText
  , approvalForKey
  , approvePromptText
  , boldRed
  , boldVibrantPink
  , dimTealForeground
  , grayForeground
  , reset
  )
import System.Console.ANSI (clearFromCursorToScreenEndCode, cursorUpCode)
import Test.Tasty
import Test.Tasty.HUnit
import TestSupport (stripAnsi, withPseudoTerminalOutput)

tests :: TestTree
tests =
  testGroup
    "console"
    [ testCase "approval accepts only y and n" $ do
        approvalForKey 'y' @?= Just ToolApproved
        approvalForKey 'Y' @?= Just ToolApproved
        approvalForKey 'n' @?= Just ToolDenied
        approvalForKey 'N' @?= Just ToolDenied
    , testCase "other keys are ignored instead of denying" $
        mapM_
          (\key -> approvalForKey key @?= Nothing)
          ['a', 'q', ' ', '\n', '\ESC']
    , testCase "Ctrl+C aborts instead of denying" $
        approvalForKey '\ETX' @?= Just ToolAborted
    , testCase "approval prompt puts the label and choices on separate lines" $ do
        approvePromptText
          @?= boldVibrantPink ++ "approve" ++ reset ++ "\n[y] Yes\n[n] No\n"
        assertBool "Yes should be on its own line" ("\n[y] Yes\n" `isInfixOf` approvePromptText)
        assertBool "No should be on its own line" ("\n[n] No\n" `isInfixOf` approvePromptText)
    , testCase "a decision replaces both option lines" $ do
        approvalDecisionText "Yes"
          @?= cursorUpCode 2 ++ "\r" ++ clearFromCursorToScreenEndCode ++ "Yes\n"
        assertBool "a negative decision ends with No" ("No\n" `isSuffixOf` approvalDecisionText "No")
    , testCase "a gray line is written as one complete escape sequence" $
        withPseudoTerminalOutput $ \readOutput -> do
          withGrayOutput (consoleWriteLine "job 1 started")
          output <- readOutput
          output @?= grayForeground ++ "job 1 started\r\n" ++ reset
    , testCase "a job start notice prints only the job line" $
        withPseudoTerminalOutput $ \readOutput -> do
          consoleWriteGrayLine "job 1 started"
          output <- readOutput
          output @?= grayForeground ++ "job 1 started" ++ reset ++ "\r\n"
          stripAnsi output @?= "job 1 started\r\n"
          assertBool
            "the colour sequence must not leak into the text"
            (not ("38;2;153;153;153m" `isInfixOf` stripAnsi output))
          assertBool
            "the job line must appear exactly once"
            (countOccurrences "job 1 started" output == 1)
    , testCase "gray lines neutralize terminal escape sequences" $
        withPseudoTerminalOutput $ \readOutput -> do
          consoleWriteGrayLine "job\ESC[2J started"
          output <- readOutput
          stripAnsi output @?= "job [2J started\r\n"
    , testCase "error lines are bold red on a terminal" $
        withPseudoTerminalOutput $ \readOutput -> do
          consoleWriteErrorLine "tool error"
          output <- readOutput
          output @?= boldRed ++ "tool error" ++ reset ++ "\r\n"
    , testCase "tool summaries print under a tools label, one per line" $
        withPseudoTerminalOutput $ \readOutput -> do
          consoleWriteToolDefinitions
            MainContext
            [ "# shell(command: str)"
            , "Run a shell command."
            , "# jobs()"
            , "List background jobs."
            ]
          output <- readOutput
          let plain = filter (/= '\r') (stripAnsi output)
          assertBool "tools label" ("tools\n" `isInfixOf` plain)
          assertBool
            "one summary per line"
            ( "# shell(command: str)\nRun a shell command.\n# jobs()\nList background jobs."
                `isInfixOf` plain
            )
          assertBool
            "headings use the dim Markdown palette"
            ("38;2;82;108;150" `isInfixOf` output)
    , testCase "a subagent prompt keeps the task's line structure" $
        withPseudoTerminalOutput $ \readOutput -> do
          consoleWriteSubagentPrompt "cheap" "line one\nline two"
          output <- readOutput
          let plain = filter (/= '\r') (stripAnsi output)
          assertBool
            "the label sits above the task"
            ("cheap\nline one\nline two" `isInfixOf` plain)
    , testCase "a result prompt uses the tool-output style" $
        withPseudoTerminalOutput $ \readOutput -> do
          consoleWriteHandlerPromptIn
            MainContext
            "mcp.demo.search.handler"
            True
            "Tool: mcp.demo.search\nResult:\nFound results\n"
          output <- readOutput
          let plain = filter (/= '\r') (stripAnsi output)
          assertBool
            "the label sits above the prompt"
            ("mcp.demo.search.handler\nTool: mcp.demo.search" `isInfixOf` plain)
          assertBool
            "the label uses the dim tool-output colour"
            ((dimTealForeground ++ "mcp.demo.search.handler" ++ reset) `isInfixOf` output)
          assertBool
            "the prompt body is gray"
            ((grayForeground ++ "Tool: mcp.demo.search") `isInfixOf` output)
    , testCase "a result prompt hides its body when output is label-only" $
        withPseudoTerminalOutput $ \readOutput -> do
          consoleWriteHandlerPromptIn
            MainContext
            "mcp.demo.search.handler"
            False
            "Tool: mcp.demo.search\nResult:\nFound results\n"
          output <- readOutput
          filter (/= '\r') (stripAnsi output) @?= "mcp.demo.search.handler\n"
    , testCase "an empty tool catalog prints no tools block" $
        withPseudoTerminalOutput $ \readOutput -> do
          consoleWriteToolDefinitions MainContext []
          output <- readOutput
          output @?= ""
    ]

countOccurrences :: String -> String -> Int
countOccurrences needle = go
  where
    go "" = 0
    go haystack
      | needle `isPrefixOf` haystack = 1 + go (drop (length needle) haystack)
      | otherwise = go (drop 1 haystack)

