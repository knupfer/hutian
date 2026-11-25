{-# LANGUAGE OverloadedStrings #-}

module ToolSpec (tests) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Map.Strict as Map
import Data.List (isInfixOf, isSuffixOf)
import qualified Data.Text as Text
import Hutian.Conversation
  ( ConversationEntry (..)
  , ToolCall (..)
  , abortToolCalls
  , abortedToolMessage
  , emptyConversation
  )
import Hutian.Jobs
  ( defaultJobOutputLines
  , lineCount
  , maxJobOutputLines
  )
import Hutian.Jobs.Runtime (withJobRuntime)
import Hutian.Json (jsonText)
import Hutian.Mcp (newMcpSessionCache)
import Hutian.Context (TurnContext (..))
import Hutian.Policy (ToolPolicy (..))
import Hutian.Tool
import Hutian.Tool.Catalog
  ( McpServer (..)
  , McpTool (..)
  , ToolCatalog
  , toolCatalog
  , withMcpServers
  )
import Hutian.Tool.Config
  ( AgentConfig (..)
  , HandlerConfigs (..)
  , McpServerConfig (..)
  , SubagentConfig (..)
  , ToolConfig (..)
  , defaultToolConfig
  )
import Hutian.Tool.Exec (ToolRuntime (..), runToolCalls, runToolCallsWith)
import Hutian.Tool.Summary (toolSummaryLines)
import Hutian.Verbosity (Verbosity (..))
import Test.Tasty
import Test.Tasty.HUnit
import TestSupport (stripAnsi, testJobId, withPseudoTerminalOutput, withTemporaryDirectory)

tests :: TestTree
tests =
  testGroup
    "tool"
    [ testCase "allowed policies run a valid shell command" $
        planTool ToolAllow shellCall @?= PlanExecute RunNow (RunShell Foreground "ls -la")
    , testCase "ask policies request approval for a valid command" $
        planTool ToolAsk shellCall @?= PlanExecute AskUser (RunShell Foreground "ls -la")
    , testCase "allowed policies start a background command" $
        planTool ToolAllow backgroundCall
          @?= PlanExecute RunNow (RunShell Background "sleep 10")
    , testCase "ask policies request approval for a background command" $
        planTool ToolAsk backgroundCall
          @?= PlanExecute AskUser (RunShell Background "sleep 10")
    , testCase "deny policies refuse every shell tool without reaching the shell" $ do
        planTool ToolDeny shellCall
          @?= PlanRefuse "tool error: the model shell tools are disabled"
        planTool ToolDeny backgroundCall
          @?= PlanRefuse "tool error: the model shell tools are disabled"
        planTool ToolDeny jobsCall
          @?= PlanRefuse "tool error: the model shell tools are disabled"
        planTool ToolDeny (ToolCall "call-1" "other" "not-json")
          @?= PlanRefuse "tool error: the model shell tools are disabled"
    , testCase "job reads run without approval and kill asks in ask mode" $ do
        planTool ToolAsk jobsCall @?= PlanExecute RunNow ListJobs
        planTool ToolAsk (jobOutputCall 2 "{\"job_id\":2}")
          @?= PlanExecute RunNow (ReadJobOutput (testJobId 2) defaultJobOutputLines)
        planTool ToolAsk (jobKillCall 3 "{\"job_id\":3}")
          @?= PlanExecute AskUser (KillJob (testJobId 3))
    , testCase "job output accepts an explicit line count" $
        planTool
          ToolAllow
          (jobOutputCall 2 "{\"job_id\":2,\"lines\":20}")
          @?= PlanExecute RunNow (ReadJobOutput (testJobId 2) (lineCount 20))
    , testCase "job ids must be positive integers" $ do
        planTool ToolAllow (jobOutputCall 0 "{\"job_id\":0}")
          @?= PlanRefuse "tool error: job_id must be a positive integer"
        planTool ToolAllow (jobKillCall 1 "{\"job_id\":-1}")
          @?= PlanRefuse "tool error: job_id must be a positive integer"
        planTool ToolAllow (jobKillCall 1 "{\"job_id\":\"1\"}")
          @?= PlanRefuse "tool error: job_id must be a positive integer"
    , testCase "a missing job id is refused" $
        planTool ToolAllow (jobKillCall 1 "{}")
          @?= PlanRefuse "tool error: missing integer argument: job_id"
    , testCase "job output line counts are clamped" $ do
        planTool
          ToolAllow
          (jobOutputCall 1 "{\"job_id\":1,\"lines\":0}")
          @?= PlanExecute RunNow (ReadJobOutput (testJobId 1) (lineCount 0))
        planTool
          ToolAllow
          (jobOutputCall 1 ("{\"job_id\":1,\"lines\":" ++ show (maxJobOutputLines + 1) ++ "}"))
          @?= PlanExecute RunNow (ReadJobOutput (testJobId 1) (lineCount maxJobOutputLines))
    , testCase "a non-integer line count is refused" $
        planTool
          ToolAllow
          (jobOutputCall 1 "{\"job_id\":1,\"lines\":\"10\"}")
          @?= PlanRefuse "tool error: lines must be an integer"
    , testCase "unknown tools are refused" $
        planTool
          ToolAllow
          (ToolCall "call-1" "other" "{\"command\":\"ls\"}")
          @?= PlanRefuse "tool error: unknown tool: other"
    , testCase "malformed arguments are refused" $
        planTool ToolAllow (ToolCall "call-1" "shell" "not-json")
          @?= PlanRefuse "tool error: tool arguments are not a JSON object"
    , testCase "a missing command argument is refused" $
        planTool ToolAllow (ToolCall "call-1" "shell" "{}")
          @?= PlanRefuse "tool error: missing string argument: command"
    , testCase "empty and whitespace-only commands are refused" $ do
        planTool ToolAllow (ToolCall "call-1" "shell" "{\"command\":\"\"}")
          @?= PlanRefuse "tool error: the shell command is empty"
        planTool ToolAllow (ToolCall "call-1" "shell" "{\"command\":\"  \\n\\t\"}")
          @?= PlanRefuse "tool error: the shell command is empty"
    , testCase "empty background commands are refused" $
        planTool ToolAllow (ToolCall "call-1" "shell_background" "{\"command\":\" \"}")
          @?= PlanRefuse "tool error: the shell command is empty"
    , testCase "command text is preserved exactly" $
        planTool
          ToolAllow
          (ToolCall "call-1" "shell" "{\"command\":\"  printf 'hi'  \"}")
          @?= PlanExecute RunNow (RunShell Foreground "  printf 'hi'  ")
    , testCase "tool definitions describe every advertised tool" $
        map toolNameOf toolDefinitions
          @?= ["shell", "shell_background", "jobs", "job_output", "job_kill"]
    , testCase "the job output description states the applied line limits" $
        assertBool
          "expected the plain numeric limits"
          ("defaults to 50 and is capped at 500" `isInfixOf` show toolDefinitions)
    , testCase "configured tools are advertised together" $
        map toolNameOf (toolDefinitionsFor configuredTools)
          @?= [ "shell"
              , "shell_background"
              , "jobs"
              , "job_output"
              , "job_kill"
              , "webfetch"
              , "subagent"
              ]
    , testCase "tool summaries put the full description below a # name line" $ do
        let summaries = toolSummaryLines (toolDefinitionsFor configuredTools)
            shellDescription =
              "Run a shell command with /bin/sh -c in the sandbox. "
                ++ "Use it to inspect files, run builds and tests, and modify the workspace. "
                ++ "Each command starts in the sandbox working directory in a fresh shell, "
                ++ "so cd and exported variables do not persist between calls; use absolute "
                ++ "paths or combine steps such as cd dir && cmd in one command."
        assertBool "name line" ("# shell(command: str)" `elem` summaries)
        assertBool
          "the description follows on the next line"
          (["# shell(command: str)", shellDescription] `isInfixOf` summaries)
        assertBool "a tool without parameters" ("# jobs()" `elem` summaries)
        assertBool
          "required parameters precede optional ones"
          ("# job_output(job_id: int, lines?: int)" `elem` summaries)
        assertBool
          "enum values are inlined"
          ("# subagent(name: cheap, prompt: str)" `elem` summaries)
    , testCase "tool summaries keep long descriptions complete" $ do
        let summaries = toolSummaryLines (toolDefinitionsFor configuredTools)
        assertBool
          "the end of a long description is present"
          (any (isSuffixOf "block the turn.") summaries)
        assertBool
          "no summary line is truncated"
          (not (any ("…" `isSuffixOf`) summaries))
    , testCase "tool summaries keep description line breaks" $ do
        let summaries = toolSummaryLines (toolDefinitionsFor mcpToolsConfig)
        assertBool
          "the description keeps its own line"
          ( [ "# mcp__demo__search(query?: str)"
            , "Search the web."
            ]
              `isInfixOf` summaries
          )
    , testCase "webfetch plans follow the configured policy" $ do
        planToolWith
          (toolCatalog (defaultToolConfig {toolWebfetch = ToolAllow}))
          webfetchCall
          @?= PlanExecute RunNow (FetchUrl "https://example.test/page")
        planToolWith
          (toolCatalog (defaultToolConfig {toolWebfetch = ToolAsk}))
          webfetchCall
          @?= PlanExecute AskUser (FetchUrl "https://example.test/page")
        planToolWith (toolCatalog defaultToolConfig) webfetchCall
          @?= PlanRefuse "tool error: the model webfetch tool is disabled"
    , testCase "webfetch rejects non-http URLs" $
        planToolWith
          (toolCatalog (defaultToolConfig {toolWebfetch = ToolAllow}))
          (ToolCall "call-1" "webfetch" "{\"url\":\"file:///etc/passwd\"}")
          @?= PlanRefuse "tool error: only http and https URLs are supported"
    , testCase "subagent plans select a configured selector" $ do
        planToolWith configuredTools subagentCall
          @?= PlanExecute RunNow (RunSubagent cheapAgent "summarize this")
        planToolWith
          configuredTools
          (ToolCall "call-1" "subagent" "{\"name\":\"missing\",\"prompt\":\"hi\"}")
          @?= PlanRefuse "tool error: unknown subagent: missing"
        planToolWith
          configuredTools
          (ToolCall "call-1" "subagent" "{\"name\":\"cheap\",\"prompt\":\"  \"}")
          @?= PlanRefuse "tool error: the subagent prompt is empty"
        planToolWith (toolCatalog defaultToolConfig) subagentCall
          @?= PlanRefuse "tool error: the model subagent tool is disabled"
    , testCase "MCP tools are namespaced and planned with validated arguments" $ do
        map toolNameOf (toolDefinitionsFor mcpToolsConfig)
          @?= ["mcp__demo__search"]
        planToolWith mcpToolsConfig mcpCall
          @?= PlanExecute RunNow (CallMcp demoServer demoSearch (object ["query" .= ("haskell" :: String)]))
        planToolWith
          (mcpToolsWith ToolAsk)
          mcpCall
          @?= PlanExecute AskUser (CallMcp (demoServer {mcpServerConfig = demoServerConfig {mcpServerPolicy = ToolAsk}}) demoSearch (object ["query" .= ("haskell" :: String)]))
        planToolWith
          (mcpToolsWith ToolDeny)
          mcpCall
          @?= PlanRefuse "tool error: the MCP server demo is disabled"
        planToolWith mcpToolsConfig (ToolCall "call-1" "mcp__demo__search" "not-json")
          @?= PlanRefuse "tool error: the MCP tool arguments are not valid JSON"
        planToolWith mcpToolsConfig (ToolCall "call-1" "mcp__demo__search" "[]")
          @?= PlanRefuse "tool error: the MCP tool arguments must be a JSON object"
        planToolWith mcpToolsConfig (ToolCall "call-1" "mcp__demo__search" "")
          @?= PlanExecute RunNow (CallMcp demoServer demoSearch (object []))
    , testCase "MCP arguments keep non-ASCII text when displayed" $
        jsonText (object ["path" .= ("R\225quira" :: String)])
          @?= "{\"path\":\"R\225quira\"}"
    , testCase "MCP tool definitions use only the tool description" $ do
        let definitions =
              LBS8.unpack (Aeson.encode (toolDefinitionsFor mcpToolsConfig))
        assertBool
          "namespaced tool"
          ("\"name\":\"mcp__demo__search\"" `isInfixOf` definitions)
        assertBool
          "no separate server line"
          (not ("MCP server:" `isInfixOf` definitions))
    , testCase "the subagent tool advertises its selectors and descriptions" $ do
        let definitions =
              LBS8.unpack (Aeson.encode (toolDefinitionsFor configuredTools))
        assertBool
          "the selector enum"
          ("\"enum\":[\"cheap\"]" `isInfixOf` definitions)
        assertBool
          "the configured description"
          ("cheap: Summarize pages when current information is needed." `isInfixOf` definitions)
        assertBool
          "the prompt guidance"
          ("cannot see this conversation" `isInfixOf` definitions)
    , testCase "aborting tool calls records a result for every call" $
        abortToolCalls
          emptyConversation
          [shellCall, ToolCall "call-2" "jobs" "{}"]
          @?= [ ToolResultEntry "call-1" abortedToolMessage
              , ToolResultEntry "call-2" abortedToolMessage
              ]
    , testCase "tool output terminates stdout and stderr separately" $
        withTemporaryDirectory "hutian-tool-output" $ \directory ->
          withJobRuntime directory directory $ \jobs ->
            withPseudoTerminalOutput $ \readOutput -> do
              _ <-
                runToolCalls
                  jobs
                  directory
                  ToolAllow
                  emptyConversation
                  [ToolCall "call-1" "shell" "{\"command\":\"printf out; printf err >&2\"}"]
              output <- readOutput
              let plain = stripAnsi output
              assertBool "stdout must end its own line" ("out\r\n" `isInfixOf` plain)
              assertBool "stderr must end its own line" ("err\r\n" `isInfixOf` plain)
    , testCase "normal verbosity does not present a subagent as a generic tool call" $
        withTemporaryDirectory "hutian-verbosity" $ \directory ->
          withJobRuntime directory directory $ \jobs ->
            withPseudoTerminalOutput $ \readOutput -> do
              mcpSessions <- newMcpSessionCache
              let runtime =
                    ToolRuntime
                      { toolRuntimeJobs = jobs
                      , toolRuntimeWorkingDirectory = directory
                      , toolRuntimeContext = MainContext
                      , toolRuntimeVerbosity = VerbosityNormal
                      , toolRuntimeMcpSessions = mcpSessions
                      , toolRuntimeSubagent = \_ _ -> pure (Right "final answer\n")
                      , toolRuntimeHandler = \_ _ _ _ -> pure (Left "not configured")
                      }
                  catalog =
                    toolCatalog
                      (defaultToolConfig {toolSubagent = [cheapAgent]})
              _ <-
                runToolCallsWith
                  runtime
                  catalog
                  emptyConversation
                  [subagentCall]
              output <- readOutput
              let plain = stripAnsi output
              assertBool
                "the subagent is not shown as a generic tool call"
                (not ("subagent" `isInfixOf` plain))
    , testCase "result handler descriptions are appended to MCP tool definitions" $ do
        let handlerConfig =
              AgentConfig
                { agentModel = "cheap"
                , agentDescription = "Summarize the search results."
                , agentSystemPrompt = "Summarize."
                , agentTools = defaultToolConfig
                }
            serverConfig =
              demoServerConfig
                { mcpHandlers = HandlerConfigs (Map.fromList [("search", handlerConfig)])
                }
            catalog =
              withMcpServers [demoServer {mcpServerConfig = serverConfig}]
                (toolCatalog (defaultToolConfig {toolMcp = [serverConfig]}))
            definitions =
              LBS8.unpack (Aeson.encode (toolDefinitionsFor catalog))
        assertBool
          "handler description"
          ("Summarize the search results." `isInfixOf` definitions)
        assertBool
          "the description follows the tool description"
          ("Search the web.\\nSummarize the search results." `isInfixOf` definitions)
        assertBool
          "no handler wrapper text"
          (not ("Results from this tool" `isInfixOf` definitions))
    ]

shellCall :: ToolCall
shellCall = ToolCall "call-1" "shell" "{\"command\":\"ls -la\"}"

webfetchCall :: ToolCall
webfetchCall = ToolCall "call-1" "webfetch" "{\"url\":\"https://example.test/page\"}"

subagentCall :: ToolCall
subagentCall = ToolCall "call-1" "subagent" "{\"name\":\"cheap\",\"prompt\":\"summarize this\"}"

cheapAgent :: SubagentConfig
cheapAgent =
  SubagentConfig
    { subagentName = "cheap"
    , subagentAgent =
        AgentConfig
          { agentModel = "cheap-upstream"
          , agentDescription = "Summarize pages when current information is needed."
          , agentSystemPrompt = "Summarize pages."
          , agentTools = defaultToolConfig
          }
    }

demoServerConfig :: McpServerConfig
demoServerConfig =
  McpServerConfig
    { mcpServerName = "demo"
    , mcpServerUrl = "https://mcp.example.test/mcp"
    , mcpServerPolicy = ToolAllow
    , mcpHandlers = HandlerConfigs Map.empty
    }

demoServer :: McpServer
demoServer =
  McpServer
    { mcpServerConfig = demoServerConfig
    , mcpServerTools = [demoSearch]
    , mcpServerSession = Nothing
    }

demoSearch :: McpTool
demoSearch =
  McpTool
    { mcpToolName = "search"
    , mcpToolDescription = "Search the web."
    , mcpToolInputSchema =
        object
          [ "type" .= ("object" :: String)
          , "properties"
              .= object ["query" .= object ["type" .= ("string" :: String)]]
          ]
    }

mcpToolsConfig :: ToolCatalog
mcpToolsConfig =
  withMcpServers [demoServer] (toolCatalog (defaultToolConfig {toolMcp = [demoServerConfig]}))

mcpToolsWith :: ToolPolicy -> ToolCatalog
mcpToolsWith policy =
  withMcpServers
    [demoServer {mcpServerConfig = demoServerConfig {mcpServerPolicy = policy}}]
    (toolCatalog (defaultToolConfig {toolMcp = [demoServerConfig {mcpServerPolicy = policy}]}))

mcpCall :: ToolCall
mcpCall =
  ToolCall
    "call-1"
    "mcp__demo__search"
    "{\"query\":\"haskell\"}"

configuredTools :: ToolCatalog
configuredTools =
  toolCatalog
    ( defaultToolConfig
        { toolShell = ToolAllow
        , toolWebfetch = ToolAllow
        , toolSubagent = [cheapAgent]
        }
    )

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
