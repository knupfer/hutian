{-# LANGUAGE OverloadedStrings #-}

-- | Pure tool protocol for the model: the advertised tool schemas, decoding a
-- call into a typed action, and turning policy plus action into a plan. The
-- terminal loop in "Hutian.Tool.Exec" performs the IO that the plan asks for,
-- so policy decisions stay testable without a terminal or a shell.
module Hutian.Tool
  ( CommandMode (..)
  , BuiltinTool (..)
  , ToolRef (..)
  , ToolAction (..)
  , Approval (..)
  , ToolPlan (..)
  , ToolRun (..)
  , toolDefinitions
  , toolDefinitionsFor
  , planTool
  , planToolWith
  , resolveTool
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
  ( JobId
  , LineCount
  , defaultJobOutputLines
  , jobIdFromInt
  , lineCount
  , lineCountValue
  , maxJobOutputLines
  )
import Hutian.Mcp.Protocol (mcpHandlerName, mcpToolDefinition, mcpToolFunctionName)
import Hutian.Policy (ToolPolicy (..), toolPolicyAdvertised)
import Hutian.Tool.Catalog
  ( McpServer (..)
  , McpTool (..)
  , ToolCatalog
  , catalogMcp
  , catalogShell
  , catalogSubagents
  , catalogWebfetch
  , mcpServerHandlersOf
  , mcpServerNameOf
  , mcpServerPolicyOf
  , shellOnlyCatalog
  )
import Hutian.Tool.Config
  ( HandlerConfigs
  , SubagentConfig (..)
  , agentDescription
  , handlerConfigFor
  )

-- | Whether a shell command should block the current turn or run as a job.
data CommandMode
  = Foreground
  | Background
  deriving stock (Eq, Show)

-- | The tools built into Hutian. MCP tools are resolved separately because
-- they only exist after discovery.
data BuiltinTool
  = BuiltinShell
  | BuiltinShellBackground
  | BuiltinJobs
  | BuiltinJobOutput
  | BuiltinJobKill
  | BuiltinWebfetch
  | BuiltinSubagent
  deriving stock (Eq, Show, Enum, Bounded)

-- | A resolved tool name. An MCP tool carries the server that advertised it,
-- so planning never has to match names a second time.
data ToolRef
  = BuiltinRef BuiltinTool
  | McpRef McpServer McpTool
  deriving stock (Eq, Show)

-- | What a tool call asks to do, after its arguments have been decoded.
data ToolAction
  = RunShell CommandMode String
  | ListJobs
  | ReadJobOutput JobId LineCount
  | KillJob JobId
  | FetchUrl String
  | RunSubagent SubagentConfig String
  | CallMcp McpServer McpTool Value
  deriving stock (Eq, Show)

-- | Whether the action runs immediately or needs the user's approval.
data Approval
  = RunNow
  | AskUser
  deriving stock (Eq, Show)

-- | What the caller should do with one requested tool call.
data ToolPlan
  = -- | Send the message back to the model without running anything.
    PlanRefuse String
  | -- | Run, or ask first, the decoded action.
    PlanExecute Approval ToolAction
  deriving stock (Eq, Show)

-- | Whether the turn can continue after a batch of tool calls. Kept in the pure
-- protocol module so the model loop does not depend on the terminal executor.
data ToolRun
  = ToolsCompleted
  | ToolsAborted
  deriving stock (Eq, Show)

-- | The shell tools, advertised when a caller only supplies a shell policy.
-- Request bodies use 'toolDefinitionsFor' with the full configuration.
toolDefinitions :: [Value]
toolDefinitions = toolDefinitionsFor (shellOnlyCatalog ToolAllow)

-- | The tools advertised to the model. They stay together in one module so the
-- request body and the call planner cannot drift apart.
toolDefinitionsFor :: ToolCatalog -> [Value]
toolDefinitionsFor catalog =
  shellTools
    ++ webfetchTools
    ++ subagentDefinitions
    ++ mcpTools
  where
    shellTools
      | toolPolicyAdvertised (catalogShell catalog) =
          [ commandTool "shell" shellDescription
          , commandTool "shell_background" backgroundDescription
          , jobsTool
          , jobOutputTool
          , jobKillTool
          ]
      | otherwise = []
    webfetchTools
      | toolPolicyAdvertised (catalogWebfetch catalog) = [webfetchTool]
      | otherwise = []
    subagentDefinitions
      | null (catalogSubagents catalog) = []
      | otherwise = [subagentTool (catalogSubagents catalog)]
    mcpTools = concatMap serverTools (catalogMcp catalog)
    serverTools server
      | toolPolicyAdvertised (mcpServerPolicyOf server) =
          [ appendHandlerDescription
              (mcpServerNameOf server)
              (mcpToolName tool)
              (mcpServerHandlersOf server)
              (mcpToolDefinition (mcpServerNameOf server) tool)
          | tool <- mcpServerTools server
          ]
      | otherwise = []

-- | Append the result handler's description to a tool definition. The suffix
-- tells the main model that the raw result is processed before it sees it.
appendHandlerDescription :: String -> String -> HandlerConfigs -> Value -> Value
appendHandlerDescription serverName toolName handlers definition =
  case handlerConfigFor toolName handlers of
    Nothing -> definition
    Just config -> mapFunctionDescription (\description -> description ++ suffix config) definition
  where
    suffix config =
      "\n\nResults from this tool are processed by the `"
        ++ mcpHandlerName serverName toolName
        ++ "` subagent: "
        ++ agentDescription config

mapFunctionDescription :: (String -> String) -> Value -> Value
mapFunctionDescription f (Object fields) =
  case KeyMap.lookup "function" fields of
    Just (Object function) ->
      case KeyMap.lookup "description" function of
        Just (String description) ->
          Object
            ( KeyMap.insert
                "function"
                ( Object
                    ( KeyMap.insert
                        "description"
                        (String (Text.pack (f (Text.unpack description))))
                        function
                    )
                )
                fields
            )
        _ -> Object fields
    _ -> Object fields
mapFunctionDescription _ value = value

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
    entry agent = "- " ++ subagentName agent ++ ": " ++ agentDescription (subagentAgent agent)

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

-- | Resolve a model tool name against the catalog. Unknown names return
-- 'Nothing'; MCP names are matched against every discovered server.
resolveTool :: ToolCatalog -> String -> Maybe ToolRef
resolveTool catalog name =
  case builtinToolFromName name of
    Just tool -> Just (BuiltinRef tool)
    Nothing -> uncurry McpRef <$> findMcpTool (catalogMcp catalog) name

builtinToolFromName :: String -> Maybe BuiltinTool
builtinToolFromName name =
  listToMaybe
    [ tool
    | tool <- [minBound .. maxBound]
    , builtinToolName tool == name
    ]

builtinToolName :: BuiltinTool -> String
builtinToolName BuiltinShell = "shell"
builtinToolName BuiltinShellBackground = "shell_background"
builtinToolName BuiltinJobs = "jobs"
builtinToolName BuiltinJobOutput = "job_output"
builtinToolName BuiltinJobKill = "job_kill"
builtinToolName BuiltinWebfetch = "webfetch"
builtinToolName BuiltinSubagent = "subagent"

-- | Resolve the policy for a tool. 'Nothing' means the tool is not configured
-- and must be refused before its arguments are inspected.
policyFor :: ToolCatalog -> ToolRef -> Maybe ToolPolicy
policyFor catalog (BuiltinRef tool) =
  case tool of
    BuiltinWebfetch -> Just (catalogWebfetch catalog)
    BuiltinSubagent
      | null (catalogSubagents catalog) -> Nothing
      | otherwise -> Just ToolAllow
    _ -> Just (catalogShell catalog)
policyFor _ (McpRef server _) = Just (mcpServerPolicyOf server)

disabledText :: ToolRef -> String
disabledText (BuiltinRef tool) =
  case tool of
    BuiltinWebfetch -> "the model webfetch tool is disabled"
    BuiltinSubagent -> "the model subagent tool is disabled"
    _ -> "the model shell tools are disabled"
disabledText (McpRef server _) =
  "the MCP server " ++ mcpServerNameOf server ++ " is disabled"

-- | Resolve every configured tool and the request into an action. Unknown
-- tools, malformed arguments, empty values, and a denied policy all become a
-- refusal message that is fed back to the model instead of reaching the tool.
-- The policy check comes first, so a denied tool cannot probe argument shapes.
planToolWith :: ToolCatalog -> ToolCall -> ToolPlan
planToolWith catalog call =
  case resolveTool catalog (toolCallName call) of
    Nothing -> PlanRefuse ("tool error: unknown tool: " ++ toolCallName call)
    Just ref ->
      case policyFor catalog ref of
        Just policy
          | policy /= ToolDeny ->
              case decodeToolAction catalog ref call of
                Left message -> PlanRefuse ("tool error: " ++ message)
                Right action -> PlanExecute (approvalFor policy action) action
        _ -> PlanRefuse ("tool error: " ++ disabledText ref)

-- | Shell-only planning, kept for callers that have no full tool configuration.
-- A denied shell refuses before resolving any name, so the model cannot probe
-- which tool names exist.
planTool :: ToolPolicy -> ToolCall -> ToolPlan
planTool ToolDeny _ = PlanRefuse "tool error: the model shell tools are disabled"
planTool policy call = planToolWith (shellOnlyCatalog policy) call

approvalFor :: ToolPolicy -> ToolAction -> Approval
approvalFor ToolAsk action =
  case action of
    RunShell {} -> AskUser
    FetchUrl {} -> AskUser
    KillJob {} -> AskUser
    CallMcp {} -> AskUser
    ListJobs -> RunNow
    ReadJobOutput {} -> RunNow
    RunSubagent {} -> RunNow
approvalFor _ _ = RunNow

decodeToolAction :: ToolCatalog -> ToolRef -> ToolCall -> Either String ToolAction
decodeToolAction catalog ref call =
  case ref of
    BuiltinRef tool ->
      case tool of
        BuiltinShell -> RunShell Foreground <$> nonEmptyShellCommand call
        BuiltinShellBackground -> RunShell Background <$> nonEmptyShellCommand call
        BuiltinJobs -> Right ListJobs
        BuiltinJobOutput -> do
          arguments <- argumentsObject call
          ReadJobOutput <$> jobIdFrom arguments <*> linesFrom arguments
        BuiltinJobKill -> KillJob <$> (argumentsObject call >>= jobIdFrom)
        BuiltinWebfetch -> FetchUrl <$> (stringArgument "url" call >>= webUrl)
        BuiltinSubagent -> do
          arguments <- argumentsObject call
          name <- stringFrom "name" arguments
          prompt <- stringFrom "prompt" arguments
          if all isSpace prompt
            then Left "the subagent prompt is empty"
            else
              case filter ((== name) . subagentName) (catalogSubagents catalog) of
                agent : _ -> Right (RunSubagent agent prompt)
                [] -> Left ("unknown subagent: " ++ name)
    McpRef server tool -> CallMcp server tool <$> mcpArguments call

nonEmptyShellCommand :: ToolCall -> Either String String
nonEmptyShellCommand call = do
  command <- stringArgument "command" call
  if all isSpace command
    then Left "the shell command is empty"
    else Right command

findMcpTool :: [McpServer] -> String -> Maybe (McpServer, McpTool)
findMcpTool servers name =
  listToMaybe
    [ (server, tool)
    | server <- servers
    , tool <- mcpServerTools server
    , mcpToolFunctionName (mcpServerNameOf server) (mcpToolName tool) == name
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
        Success identifier ->
          case jobIdFromInt identifier of
            Just jobId -> Right jobId
            Nothing -> Left "job_id must be a positive integer"
        _ -> Left "job_id must be a positive integer"

linesFrom :: KeyMap Value -> Either String LineCount
linesFrom arguments =
  case KeyMap.lookup "lines" arguments of
    Nothing -> Right defaultJobOutputLines
    Just value ->
      case (Aeson.fromJSON value :: Result Int) of
        Success requested -> Right (lineCount requested)
        _ -> Left "lines must be an integer"
