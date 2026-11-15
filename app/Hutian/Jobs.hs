-- | Pure background-job model, identifiers, status, and text shared by the
-- terminal and the model-facing tools.
module Hutian.Jobs
  ( JobId (..)
  , JobStatus (..)
  , Job (..)
  , parseJobId
  , jobIdText
  , jobStatusText
  , jobSummaryLine
  , jobStartedLine
  , jobListResult
  , jobOutputResult
  , jobOutputTruncationNote
  , jobKillResult
  , defaultJobOutputLines
  , maxJobOutputLines
  ) where

import Data.List (intercalate)
import System.Exit (ExitCode (..))
import Text.Read (readMaybe)

-- | A job id is allocated per shell session and is always positive.
newtype JobId = JobId Int
  deriving stock (Eq, Ord, Show)

data JobStatus
  = JobRunning
  | JobExited ExitCode
  deriving stock (Eq, Show)

-- | The stable information retained for a background job. The process handle
-- lives in the runtime so this value can be inspected and rendered purely.
data Job = Job
  { jobId :: JobId
  , jobCommand :: String
  , jobPid :: Maybe Int
  , jobLogPath :: FilePath
  , jobStatus :: JobStatus
  }
  deriving stock (Eq, Show)

-- | Parse a positive job id from user or model text.
parseJobId :: String -> Maybe JobId
parseJobId text = do
  value <- readMaybe text
  if value > 0 then Just (JobId value) else Nothing

jobIdText :: JobId -> String
jobIdText (JobId value) = show value

-- | A short, human-readable status. A negative exit code means the process was
-- terminated by a signal, which the process library reports as @-signum@.
jobStatusText :: JobStatus -> String
jobStatusText JobRunning = "running"
jobStatusText (JobExited ExitSuccess) = "exit 0"
jobStatusText (JobExited (ExitFailure code))
  | code < 0 = "signal " ++ show (negate code)
  | otherwise = "exit " ++ show code

-- | One line for the @jobs@ list.
jobSummaryLine :: Job -> String
jobSummaryLine job =
  "job "
    ++ jobIdText (jobId job)
    ++ "  "
    ++ jobStatusText (jobStatus job)
    ++ pidSuffix
    ++ "  "
    ++ singleLine (jobCommand job)
  where
    pidSuffix =
      case jobStatus job of
        JobRunning -> maybe "" (\pid -> "  pid " ++ show pid) (jobPid job)
        JobExited _ -> ""

-- | The line shown when a job starts. The command and process details are
-- already visible in the request or the @jobs@ list, so the notice stays to a
-- single line.
jobStartedLine :: Job -> String
jobStartedLine job = "job " ++ jobIdText (jobId job) ++ " started"

jobListResult :: [Job] -> String
jobListResult [] = "no background jobs"
jobListResult jobs = intercalate "\n" (map jobSummaryLine jobs)

jobOutputResult :: Job -> String -> Bool -> String
jobOutputResult job output truncated =
  "job "
    ++ jobIdText (jobId job)
    ++ " "
    ++ jobStatusText (jobStatus job)
    ++ "\n"
    ++ truncation
    ++ body
  where
    truncation = if truncated then jobOutputTruncationNote ++ "\n" else ""
    body
      | null output = "(no output)\n"
      | last output == '\n' = output
      | otherwise = output ++ "\n"

jobOutputTruncationNote :: String
jobOutputTruncationNote = "[output truncated; showing the most recent lines]"

-- | Text describing the result of a stop request. @wasRunning@ distinguishes a
-- job that was already finished from one that the signal actually stopped.
jobKillResult :: Bool -> Job -> String
jobKillResult wasRunning job
  | not wasRunning = alreadyFinished
  | JobRunning <- jobStatus job = "job " ++ jobIdText (jobId job) ++ " is still running"
  | otherwise = "job " ++ jobIdText (jobId job) ++ " stopped: " ++ jobStatusText (jobStatus job)
  where
    alreadyFinished =
      "job "
        ++ jobIdText (jobId job)
        ++ " already finished: "
        ++ jobStatusText (jobStatus job)

defaultJobOutputLines :: Int
defaultJobOutputLines = 50

maxJobOutputLines :: Int
maxJobOutputLines = 500

-- | Keep a multi-line command from breaking the one-line job presentation.
singleLine :: String -> String
singleLine = map replace
  where
    replace '\n' = ' '
    replace '\r' = ' '
    replace character = character
