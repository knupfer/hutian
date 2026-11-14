module ToolSpec (tests) where

import Hutian.Conversation (ToolCall (..))
import Hutian.Policy (ModelShell (..))
import Hutian.Tool
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "tool"
    [ testCase "allowed policies run a valid shell command" $
        planTool ModelShellAllowed validCall @?= PlanRun "ls -la"
    , testCase "ask policies request approval for a valid command" $
        planTool ModelShellAsk validCall @?= PlanApprove "ls -la"
    , testCase "deny policies refuse without reaching the shell" $
        planTool ModelShellDenied validCall
          @?= PlanRefuse "tool error: the model shell tool is disabled"
    , testCase "unknown tools are refused" $
        planTool
          ModelShellAllowed
          (ToolCall "call-1" "other" "{\"command\":\"ls\"}")
          @?= PlanRefuse "tool error: unknown tool: other"
    , testCase "malformed arguments are refused" $
        planTool ModelShellAllowed (ToolCall "call-1" "shell" "not-json")
          @?= PlanRefuse "tool error: tool arguments are not a JSON object"
    , testCase "a missing command argument is refused" $
        planTool ModelShellAllowed (ToolCall "call-1" "shell" "{}")
          @?= PlanRefuse "tool error: missing string argument: command"
    , testCase "empty and whitespace-only commands are refused" $ do
        planTool ModelShellAllowed (ToolCall "call-1" "shell" "{\"command\":\"\"}")
          @?= PlanRefuse "tool error: the shell command is empty"
        planTool ModelShellAllowed (ToolCall "call-1" "shell" "{\"command\":\"  \\n\\t\"}")
          @?= PlanRefuse "tool error: the shell command is empty"
    , testCase "command text is preserved exactly" $
        planTool
          ModelShellAllowed
          (ToolCall "call-1" "shell" "{\"command\":\"  printf 'hi'  \"}")
          @?= PlanRun "  printf 'hi'  "
    ]

validCall :: ToolCall
validCall = ToolCall "call-1" "shell" "{\"command\":\"ls -la\"}"
