{-# LANGUAGE OverloadedStrings #-}

-- | Pure tool protocol for the model: the advertised tool schemas, decoding a
-- call into a typed request, and turning policy plus request into a plan. The
-- terminal loop in "Main" performs the IO that the plan asks for, so policy
-- decisions stay testable without a terminal or a shell.
module Hutian.Tool
  ( CommandMode (..)
  , ToolPlan (..)
  , toolDefinitions
  , toolDefinitionsFor
  , planTool
  , planToolWith
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Result (..))
import Data.Char (isControl, isSpace)
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (listToMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Hutian.Conversation (ToolCall (..))
import Hutian.Jobs
  ( JobId (..)
  , LineCount
  , defaultJobOutputLines
  , lineCount
  , lineCountValue
  , maxJobOutputLines
  )
import Hutian.Mcp.Protocol (mcpToolDefinition, mcpToolFunctionName)
import Hutian.Policy (ToolPolicy (..), toolPolicyAdvertised)
import Hutian.Tool.Catalog
  ( McpServer (..)
  , McpTool (..)
  , ToolCatalog (..)
  , toolCatalog
  )
import Hutian.Tool.Config
  ( McpServerConfig (..)
  , SubagentConfig (..)
  , ToolConfig (..)
  , defaultToolConfig
  )

-- | Whether a shell command should block the current turn or run as a job.
data CommandMode
  = Foreground
  | Background
  deriving stock (Eq, Show)

-- | A tool call after its arguments have been decoded. Reading job state needs
-- no command, so it is a request of its own rather than an empty command.
data ToolRequest
  = RequestShell String
  | RequestShellBackground String
  | RequestJobs
  | RequestJobOutput JobId LineCount
  | RequestJobKill JobId
  | RequestWebfetch String
  | RequestSubagent String String
  deriving stock (Eq, Show)

-- | What the caller should do with one requested tool call.
data ToolPlan
  = -- | Run the command without asking.
    PlanRun CommandMode String
  | -- | Show the command and ask the user before running it.
    PlanApprove CommandMode String
  | -- | Send the message back to the model without running anything.
    PlanRefuse String
  | -- | List the session's background jobs.
    PlanListJobs
  | -- | Read the end of a job log.
    PlanJobOutput JobId LineCount
  | -- | Stop a background job.
    PlanJobKill JobId
  | -- | Fetch a URL without asking.
    PlanRunWebfetch String
  | -- | Show a URL and ask the user before fetching it.
    PlanApproveWebfetch String
  | -- | Run one configured subagent serially and return its answer.
    PlanSubagent SubagentConfig String
  | -- | Call an MCP tool without asking.
    PlanRunMcp McpServer McpTool Value
  | -- | Show an MCP tool call and ask the user before running it.
    PlanApproveMcp McpServer McpTool Value
  deriving stock (Eq, Show)

-- | The policy-aware planners. 'planTool' covers the shell-only entry point;
-- 'planToolWith' resolves every configured tool.
-- | The shell tools, advertised when a caller only supplies a shell policy.
-- Request bodies use 'toolDefinitionsFor' with the full configuration.
toolDefinitions :: [Value]
toolDefinitions =
  toolDefinitionsFor (toolCatalog (defaultToolConfig {toolShell = ToolAllow}))

-- | The tools advertised to the model. They stay together in one module so the
-- request body and the call planner cannot drift apart.
toolDefinitionsFor :: ToolCatalog -> [Value]
toolDefinitionsFor catalog =
  shellTools
    ++ webfetchTools
    ++ subagentTools
    ++ mcpTools
  where
    config = catalogConfig catalog
    shellTools
      | toolPolicyAdvertised (toolShell config) =
          [ commandTool "shell" shellDescription
          , commandTool "shell_background" backgroundDescription
          , jobsTool
          , jobOutputTool
          , jobKillTool
          ]
      | otherwise = []
    webfetchTools
      | toolPolicyAdvertised (toolWebfetch config) = [webfetchTool]
      | otherwise = []
    subagentTools
      | null (toolSubagent config) = []
      | otherwise = [subagentTool (toolSubagent config)]
    mcpTools = concatMap serverTools (catalogMcp catalog)
    serverTools server
      | toolPolicyAdvertised (mcpServerPolicy (mcpServerConfig server)) =
          map
            (mcpToolDefinition (mcpServerName (mcpServerConfig server)))
            (mcpServerTools server)
      | otherwise = []

shellDescription :: String
shellDescription =
  "Run a shell command with /bin/sh -c in the sandbox. "
    ++ "Use it to inspect files, run builds and tests, and modify the workspace. "
    ++ "Each command starts in the sandbox working directory in a fresh shell, "
    ++ "so cd and exported variables do not persist between calls; use absolute "
    ++ "paths or combine steps such as cd dir && cmd in one command."

backgroundDescription :: String
backgroundDescription =
  "Start a shell command in the background with /bin/sh -c and return a job id "
    ++ "immediately. stdout and stderr are appended to the job's log. "
    ++ "Use jobs to list jobs, job_output to read a log, and job_kill to stop a "
    ++ "job. The command runs in its own process group and in a fresh shell; it "
    ++ "keeps running until it exits or is stopped, and is stopped when the shell "
    ++ "exits. Use this for servers, watchers, and long builds that should not "
    ++ "block the turn."

jobsTool :: Value
jobsTool =
  functionTool
    "jobs"
    ( "List the background jobs started in this session, including status, pid, "
        ++ "and command. Finished jobs stay listed until the shell exits."
    )
    (object [ "type" .= ("object" :: String)])

jobOutputTool :: Value
jobOutputTool =
  functionTool
    "job_output"
    ( "Read the end of a background job's log. The optional lines argument "
        ++ "controls how many lines are returned; it defaults to "
        ++ show (lineCountValue defaultJobOutputLines)
        ++ " and is capped at "
        ++ show maxJobOutputLines
        ++ "."
    )
    ( object
        [ "type" .= ("object" :: String)
        , "properties" .=
            object
              [ "job_id" .= integerProperty "The background job id."
              , "lines" .= integerProperty "How many trailing lines to read."
              ]
        , "required" .= ["job_id" :: String]
        ]
    )

jobKillTool :: Value
jobKillTool =
  functionTool
    "job_kill"
    ( "Stop a background job by sending SIGTERM to its process group, then "
        ++ "SIGKILL if it does not exit."
    )
    ( object
        [ "type" .= ("object" :: String)
        , "properties" .=
            object
              ["job_id" .= integerProperty "The background job id."]
        , "required" .= ["job_id" :: String]
        ]
    )

webfetchTool :: Value
webfetchTool =
  functionTool
    "webfetch"
    ( "Fetch an http or https URL and return its response body. "
        ++ "Use it to read documentation, issues, or pages the user points to. "
        ++ "The response is bounded and non-success statuses are reported in the result."
    )
    ( object
        [ "type" .= ("object" :: String)
        , "properties" .=
            object
              ["url" .= stringProperty "The http or https URL to fetch."]
        , "required" .= ["url" :: String]
        ]
    )

subagentTool :: [SubagentConfig] -> Value
subagentTool agents =
  functionTool
    "subagent"
    ( "Delegate a self-contained task to one configured subagent and return only "
        ++ "its final answer. Subagents cannot see this conversation, so include "
        ++ "all relevant context in the prompt and state the final output you "
        ++ "need. They run one at a time, use their own system prompt and tools, "
        ++ "and share this model API.\n\nConfigured subagents:\n"
        ++ subagentCatalogue agents
    )
    ( object
        [ "type" .= ("object" :: String)
        , "properties" .=
            object
              [ "name"
                  .= enumProperty
                    "The configured subagent name."
                    (map subagentName agents)
              , "prompt"
                  .= stringProperty
                    "A self-contained task. The subagent cannot see this conversation, so include all relevant context and state the final output you need."
              ]
        , "required" .= ["name" :: String, "prompt" :: String]
        ]
    )

-- | List the configured selectors with the descriptions their owners supplied.
-- The main model sees this in the tool description, which is how it learns when
-- a subagent is appropriate and what each one can do.
subagentCatalogue :: [SubagentConfig] -> String
subagentCatalogue = intercalate "\n" . map entry
  where
    entry agent = "- " ++ subagentName agent ++ ": " ++ subagentDescription agent

enumProperty :: String -> [String] -> Value
enumProperty description values =
  object
    [ "type" .= ("string" :: String)
    , "description" .= (description :: String)
    , "enum" .= values
    ]

commandTool :: String -> String -> Value
commandTool name description =
  functionTool
    name
    description
    ( object
        [ "type" .= ("object" :: String)
        , "properties" .=
            object
              ["command" .= stringProperty "The shell command to run."]
        , "required" .= ["command" :: String]
        ]
    )

functionTool :: String -> String -> Value -> Value
functionTool name description parameters =
  object
    [ "type" .= ("function" :: String)
    , "function" .=
        object
          [ "name" .= (name :: String)
          , "description" .= (description :: String)
          , "parameters" .= parameters
          ]
    ]

stringProperty :: String -> Value
stringProperty description =
  object
    [ "type" .= ("string" :: String)
    , "description" .= (description :: String)
    ]

integerProperty :: String -> Value
integerProperty description =
  object
    [ "type" .= ("integer" :: String)
    , "description" .= (description :: String)
    ]

-- | Decode a tool call into a typed request. Unknown tools and malformed
-- arguments become a message that is sent back to the model.
toolRequest :: ToolCall -> Either String ToolRequest
toolRequest call =
  case toolCallName call of
    "shell" -> RequestShell <$> commandArgument call
    "shell_background" -> RequestShellBackground <$> commandArgument call
    "jobs" -> Right RequestJobs
    "job_output" -> do
      arguments <- argumentsObject call
      RequestJobOutput <$> jobIdFrom arguments <*> linesFrom arguments
    "job_kill" -> RequestJobKill <$> (argumentsObject call >>= jobIdFrom)
    "webfetch" -> RequestWebfetch <$> stringArgument "url" call
    "subagent" -> do
      arguments <- argumentsObject call
      name <- stringFrom "name" arguments
      prompt <- stringFrom "prompt" arguments
      pure (RequestSubagent name prompt)
    name -> Left ("unknown tool: " ++ name)

-- | Resolve every configured tool and the request into an action. Unknown
-- tools, malformed arguments, empty values, and a denied policy all become a
-- refusal message that is fed back to the model instead of reaching the tool.
-- In ask mode, only the action itself needs approval; checking job state and
-- running a configured subagent do not.
planToolWith :: ToolCatalog -> ToolCall -> ToolPlan
planToolWith catalog call
  | Just (server, tool) <- findMcpTool (catalogMcp catalog) (toolCallName call) =
      mcpPlan server tool
  | otherwise =
      case toolCallName call of
        "shell" -> shellPlan (toolShell config) Foreground
        "shell_background" -> shellPlan (toolShell config) Background
        "jobs" -> shellInspectionPlan (toolShell config)
        "job_output" -> shellInspectionPlan (toolShell config)
        "job_kill" -> shellInspectionPlan (toolShell config)
        "webfetch" -> webfetchPlan (toolWebfetch config)
        "subagent" -> subagentPlan (toolSubagent config)
        name -> PlanRefuse ("tool error: unknown tool: " ++ name)
  where
    config = catalogConfig catalog
    mcpPlan server tool =
      case mcpServerPolicy (mcpServerConfig server) of
        ToolDeny ->
          PlanRefuse
            ( "tool error: the MCP server "
                ++ mcpServerName (mcpServerConfig server)
                ++ " is disabled"
            )
        ToolAsk -> planned (PlanApproveMcp server tool)
        ToolAllow -> planned (PlanRunMcp server tool)
      where
        planned makePlan =
          case mcpArguments call of
            Left message -> PlanRefuse ("tool error: " ++ message)
            Right arguments -> makePlan arguments

    shellPlan policy mode =
      case policy of
        -- Refuse before inspecting the call, so a denied model cannot probe
        -- which tool names or argument shapes exist.
        ToolDeny -> PlanRefuse "tool error: the model shell tools are disabled"
        ToolAllow -> runShell mode
        ToolAsk -> approveShell mode

    runShell mode =
      case toolRequest call of
        Left message -> PlanRefuse ("tool error: " ++ message)
        Right (RequestShell command)
          | all isSpace command -> PlanRefuse "tool error: the shell command is empty"
          | otherwise -> PlanRun mode command
        Right (RequestShellBackground command)
          | all isSpace command -> PlanRefuse "tool error: the shell command is empty"
          | otherwise -> PlanRun mode command
        Right _ -> PlanRefuse "tool error: the shell command is missing"

    approveShell mode =
      case toolRequest call of
        Left message -> PlanRefuse ("tool error: " ++ message)
        Right (RequestShell command)
          | all isSpace command -> PlanRefuse "tool error: the shell command is empty"
          | otherwise -> PlanApprove mode command
        Right (RequestShellBackground command)
          | all isSpace command -> PlanRefuse "tool error: the shell command is empty"
          | otherwise -> PlanApprove mode command
        Right _ -> PlanRefuse "tool error: the shell command is missing"

    shellInspectionPlan policy =
      case policy of
        ToolDeny -> PlanRefuse "tool error: the model shell tools are disabled"
        _ ->
          case toolRequest call of
            Left message -> PlanRefuse ("tool error: " ++ message)
            Right RequestJobs -> PlanListJobs
            Right (RequestJobOutput target requestedLines) ->
              PlanJobOutput target requestedLines
            Right (RequestJobKill target) -> PlanJobKill target
            Right _ -> PlanRefuse "tool error: unknown shell inspection tool"

    webfetchPlan policy =
      case policy of
        ToolDeny -> PlanRefuse "tool error: the model webfetch tool is disabled"
        _ ->
          case toolRequest call of
            Left message -> PlanRefuse ("tool error: " ++ message)
            Right (RequestWebfetch url) ->
              case webUrl url of
                Left message -> PlanRefuse ("tool error: " ++ message)
                Right validUrl
                  | policy == ToolAsk -> PlanApproveWebfetch validUrl
                  | otherwise -> PlanRunWebfetch validUrl
            Right _ -> PlanRefuse "tool error: the webfetch request is missing"

    subagentPlan agents
      | null agents = PlanRefuse "tool error: the model subagent tool is disabled"
      | otherwise =
          case toolRequest call of
            Left message -> PlanRefuse ("tool error: " ++ message)
            Right (RequestSubagent name prompt)
              | all isSpace prompt ->
                  PlanRefuse "tool error: the subagent prompt is empty"
              | otherwise ->
                  case filter ((== name) . subagentName) agents of
                    agent : _ -> PlanSubagent agent prompt
                    [] -> PlanRefuse ("tool error: unknown subagent: " ++ name)
            Right _ -> PlanRefuse "tool error: the subagent request is missing"

-- | Shell-only planning, kept for callers that have no full tool configuration.
planTool :: ToolPolicy -> ToolCall -> ToolPlan
planTool policy call =
  case policy of
    ToolDeny -> PlanRefuse "tool error: the model shell tools are disabled"
    ToolAllow -> planned (run Foreground) (run Background)
    ToolAsk -> planned (approve Foreground) (approve Background)
  where
    planned shellCommand background =
      case toolRequest call of
        Left message -> PlanRefuse ("tool error: " ++ message)
        Right request -> resolve shellCommand background request

    resolve shell background request =
      case request of
        RequestJobs -> PlanListJobs
        RequestJobOutput target requestedLines -> PlanJobOutput target requestedLines
        RequestJobKill target -> PlanJobKill target
        RequestShell command -> shell command
        RequestShellBackground command -> background command
        RequestWebfetch _ -> PlanRefuse "tool error: the model webfetch tool is disabled"
        RequestSubagent _ _ -> PlanRefuse "tool error: the model subagent tool is disabled"

    run mode command
      | all isSpace command = PlanRefuse "tool error: the shell command is empty"
      | otherwise = PlanRun mode command

    approve mode command
      | all isSpace command = PlanRefuse "tool error: the shell command is empty"
      | otherwise = PlanApprove mode command

findMcpTool :: [McpServer] -> String -> Maybe (McpServer, McpTool)
findMcpTool servers name =
  listToMaybe
    [ (server, tool)
    | server <- servers
    , tool <- mcpServerTools server
    , mcpToolFunctionName (mcpServerName (mcpServerConfig server)) (mcpToolName tool) == name
    ]

mcpArguments :: ToolCall -> Either String Value
mcpArguments call
  | all isSpace (toolCallArguments call) = Right (object [])
  | otherwise =
      case
          Aeson.decodeStrict'
            (TextEncoding.encodeUtf8 (Text.pack (toolCallArguments call)))
        of
          Just value@(Object _) -> Right value
          Just Null -> Right (object [])
          Just _ -> Left "the MCP tool arguments must be a JSON object"
          Nothing -> Left "the MCP tool arguments are not valid JSON"

-- | Reject non-http URLs and whitespace before the HTTP client sees them.
webUrl :: String -> Either String String
webUrl url
  | all isSpace url = Left "the URL is empty"
  | any isControl url || any isSpace url = Left "the URL contains whitespace or control characters"
  | "http://" `isPrefixOf` url = Right url
  | "https://" `isPrefixOf` url = Right url
  | otherwise = Left "only http and https URLs are supported"

stringArgument :: String -> ToolCall -> Either String String
stringArgument name call = argumentsObject call >>= stringFrom name

stringFrom :: String -> KeyMap Value -> Either String String
stringFrom name arguments =
  case KeyMap.lookup (AesonKey.fromText (Text.pack name)) arguments of
    Just (String value) -> Right (Text.unpack value)
    _ -> Left ("missing string argument: " ++ name)

commandArgument :: ToolCall -> Either String String
commandArgument = stringArgument "command"

argumentsObject :: ToolCall -> Either String (KeyMap Value)
argumentsObject call =
  case Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 (Text.pack (toolCallArguments call))) of
    Right (Object arguments) -> Right arguments
    _ -> Left "tool arguments are not a JSON object"

jobIdFrom :: KeyMap Value -> Either String JobId
jobIdFrom arguments =
  case KeyMap.lookup "job_id" arguments of
    Nothing -> Left "missing integer argument: job_id"
    Just value ->
      case (Aeson.fromJSON value :: Result Int) of
        Success identifier
          | identifier > 0 -> Right (JobId identifier)
        _ -> Left "job_id must be a positive integer"

linesFrom :: KeyMap Value -> Either String LineCount
linesFrom arguments =
  case KeyMap.lookup "lines" arguments of
    Nothing -> Right defaultJobOutputLines
    Just value ->
      case (Aeson.fromJSON value :: Result Int) of
        Success requested -> Right (lineCount requested)
        _ -> Left "lines must be an integer"
