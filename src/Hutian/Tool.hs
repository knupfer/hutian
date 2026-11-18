{-# LANGUAGE OverloadedStrings #-}

-- | Pure tool protocol for the model: the advertised tool schemas, decoding a
-- call into a typed request, and turning policy plus request into a plan. The
-- terminal loop in "Main" performs the IO that the plan asks for, so policy
-- decisions stay testable without a terminal or a shell.
module Hutian.Tool
  ( CommandMode (..)
  , ToolRequest (..)
  , ToolPlan (..)
  , toolDefinitions
  , toolRequest
  , planTool
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Result (..))
import Data.Char (isSpace)
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
import Hutian.Policy (ModelShell (..))

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
  deriving stock (Eq, Show)

-- | The tools advertised to the model. They stay together in one module so the
-- request body and the call planner cannot drift apart.
toolDefinitions :: [Value]
toolDefinitions =
  [ commandTool "shell" shellDescription
  , commandTool "shell_background" backgroundDescription
  , jobsTool
  , jobOutputTool
  , jobKillTool
  ]

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
    name -> Left ("unknown tool: " ++ name)

-- | Resolve the policy and the request into an action. Unknown tools,
-- malformed arguments, empty commands, and a denied policy all become a
-- refusal message that is fed back to the model instead of reaching the shell.
-- In ask mode, only commands need approval; reading job state does not.
planTool :: ModelShell -> ToolCall -> ToolPlan
planTool policy call =
  case policy of
    -- Refuse before inspecting the call, so a denied model cannot probe which
    -- tool names or argument shapes exist.
    ModelShellDenied -> PlanRefuse "tool error: the model shell tools are disabled"
    ModelShellAllowed -> planned (run Foreground) (run Background)
    ModelShellAsk -> planned (approve Foreground) (approve Background)
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

    run mode command
      | all isSpace command = PlanRefuse "tool error: the shell command is empty"
      | otherwise = PlanRun mode command

    approve mode command
      | all isSpace command = PlanRefuse "tool error: the shell command is empty"
      | otherwise = PlanApprove mode command

commandArgument :: ToolCall -> Either String String
commandArgument call = do
  arguments <- argumentsObject call
  case KeyMap.lookup "command" arguments of
    Just (String command) -> Right (Text.unpack command)
    _ -> Left "missing string argument: command"

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
