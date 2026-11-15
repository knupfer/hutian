{-# LANGUAGE OverloadedStrings #-}

module ToolSpec (tests) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as Text
import Hutian.Conversation (ToolCall (..))
import Hutian.Jobs (JobId (..), defaultJobOutputLines)
import Hutian.Policy (ModelShell (..))
import Hutian.Tool
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "tool"
    [ testCase "allowed policies run a valid shell command" $
        planTool ModelShellAllowed shellCall @?= PlanRun Foreground "ls -la"
    , testCase "ask policies request approval for a valid command" $
        planTool ModelShellAsk shellCall @?= PlanApprove Foreground "ls -la"
    , testCase "allowed policies start a background command" $
        planTool ModelShellAllowed backgroundCall
          @?= PlanRun Background "sleep 10"
    , testCase "ask policies request approval for a background command" $
        planTool ModelShellAsk backgroundCall
          @?= PlanApprove Background "sleep 10"
    , testCase "deny policies refuse every shell tool without reaching the shell" $ do
        planTool ModelShellDenied shellCall
          @?= PlanRefuse "tool error: the model shell tools are disabled"
        planTool ModelShellDenied backgroundCall
          @?= PlanRefuse "tool error: the model shell tools are disabled"
        planTool ModelShellDenied jobsCall
          @?= PlanRefuse "tool error: the model shell tools are disabled"
    , testCase "job inspection runs without approval in ask mode" $ do
        planTool ModelShellAsk jobsCall @?= PlanListJobs
        planTool ModelShellAsk (jobOutputCall 2 "{\"job_id\":2}")
          @?= PlanJobOutput (JobId 2) defaultJobOutputLines
        planTool ModelShellAsk (jobKillCall 3 "{\"job_id\":3}")
          @?= PlanJobKill (JobId 3)
    , testCase "job output accepts an explicit line count" $
        planTool
          ModelShellAllowed
          (jobOutputCall 2 "{\"job_id\":2,\"lines\":20}")
          @?= PlanJobOutput (JobId 2) 20
    , testCase "job ids must be positive integers" $ do
        planTool ModelShellAllowed (jobOutputCall 0 "{\"job_id\":0}")
          @?= PlanRefuse "tool error: job_id must be a positive integer"
        planTool ModelShellAllowed (jobKillCall 1 "{\"job_id\":-1}")
          @?= PlanRefuse "tool error: job_id must be a positive integer"
        planTool ModelShellAllowed (jobKillCall 1 "{\"job_id\":\"1\"}")
          @?= PlanRefuse "tool error: job_id must be a positive integer"
    , testCase "a missing job id is refused" $
        planTool ModelShellAllowed (jobKillCall 1 "{}")
          @?= PlanRefuse "tool error: missing integer argument: job_id"
    , testCase "an invalid line count is refused" $
        planTool
          ModelShellAllowed
          (jobOutputCall 1 "{\"job_id\":1,\"lines\":0}")
          @?= PlanRefuse "tool error: lines must be a positive integer"
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
    , testCase "empty background commands are refused" $
        planTool ModelShellAllowed (ToolCall "call-1" "shell_background" "{\"command\":\" \"}")
          @?= PlanRefuse "tool error: the shell command is empty"
    , testCase "command text is preserved exactly" $
        planTool
          ModelShellAllowed
          (ToolCall "call-1" "shell" "{\"command\":\"  printf 'hi'  \"}")
          @?= PlanRun Foreground "  printf 'hi'  "
    , testCase "tool definitions describe every advertised tool" $
        map toolNameOf toolDefinitions
          @?= ["shell", "shell_background", "jobs", "job_output", "job_kill"]
    ]

shellCall :: ToolCall
shellCall = ToolCall "call-1" "shell" "{\"command\":\"ls -la\"}"

backgroundCall :: ToolCall
backgroundCall = ToolCall "call-1" "shell_background" "{\"command\":\"sleep 10\"}"

jobsCall :: ToolCall
jobsCall = ToolCall "call-1" "jobs" "{}"

jobOutputCall :: Int -> String -> ToolCall
jobOutputCall index = ToolCall ("call-" ++ show index) "job_output"

jobKillCall :: Int -> String -> ToolCall
jobKillCall index = ToolCall ("call-" ++ show index) "job_kill"

-- The definition shape is tested through the LLM request body; this small
-- helper keeps the name order check readable.
toolNameOf :: Value -> String
toolNameOf (Object tool) =
  case KeyMap.lookup "function" tool of
    Just (Object functionObject) ->
      case KeyMap.lookup "name" functionObject of
        Just (String name) -> Text.unpack name
        _ -> ""
    _ -> ""
toolNameOf _ = ""
