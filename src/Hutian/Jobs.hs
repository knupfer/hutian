-- | Pure background-job model, identifiers, status, and text used by the
-- model-facing tools.
module Hutian.Jobs
  ( JobId (..)
  , JobStatus (..)
  , jobStatusFromExit
  , Job (..)
  , jobIdText
  , jobStatusText
  , jobSummaryLine
  , jobStartedLine
  , jobListResult
  , jobOutputResult
  , jobOutputTruncationNote
  , jobKillResult
  , LineCount
  , lineCount
  , lineCountValue
  , defaultJobOutputLines
  , maxJobOutputLines
  ) where

import Data.List (intercalate)
import System.Exit (ExitCode (..))

-- | A job id is allocated per shell session and is always positive.
newtype JobId = JobId Int
  deriving stock (Eq, Ord, Show)

-- | A job either runs or has stopped with an explicit outcome. Keeping the
-- exit number and the signal number separate avoids the @ExitFailure (-n)@
-- convention leaking into presentation code.
data JobStatus
  = JobRunning
  | JobExited Int
  | JobSignalled Int
  deriving stock (Eq, Show)

{-@ measure isRunning :: JobStatus -> Bool
        isRunning JobRunning = True
        isRunning (JobExited _) = False
        isRunning (JobSignalled _) = False @-}

-- | Convert the process library's exit code to a job status. A negative
-- failure code means the process was terminated by a signal.
{-@ jobStatusFromExit :: ExitCode -> {v:JobStatus | not (isRunning v)} @-}
jobStatusFromExit :: ExitCode -> JobStatus
jobStatusFromExit ExitSuccess = JobExited 0
jobStatusFromExit (ExitFailure code)
  | code < 0 = JobSignalled (negate code)
  | otherwise = JobExited code

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

jobIdText :: JobId -> String
jobIdText (JobId value) = show value

-- | A short, human-readable status.
jobStatusText :: JobStatus -> String
jobStatusText JobRunning = "running"
jobStatusText (JobExited 0) = "exit 0"
jobStatusText (JobExited code) = "exit " ++ show code
jobStatusText (JobSignalled signal) = "signal " ++ show signal

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
        _ -> ""

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

-- | A bounded, positive number of log lines. Constructing one is the only way
-- to request a tail size, so runtime code never needs to clamp a raw 'Int'.
--
-- This stays a @data@ declaration because LiquidHaskell only enforces data
-- invariants on @data@ types, not on @newtype@s.
{- HLINT ignore "Use newtype instead of data" -}
{-@ data LineCount = LineCount { lineCountValue :: {v:Int | 1 <= v && v <= 500} } @-}
data LineCount = LineCount { lineCountValue :: Int }
  deriving stock (Eq, Ord, Show)

-- | Clamp a requested tail size into the supported range. Constructing one is
-- the only way to request a tail size, so runtime code never sees a raw 'Int'
-- and callers do not need their own bounds handling.
lineCount :: Int -> LineCount
lineCount = LineCount . max 1 . min maxJobOutputLines

defaultJobOutputLines :: LineCount
defaultJobOutputLines = LineCount 50

{-@ maxJobOutputLines :: {v:Int | v == 500} @-}
maxJobOutputLines :: Int
maxJobOutputLines = 500

-- | Keep a multi-line command from breaking the one-line job presentation.
{-@ singleLine :: command:String -> {v:String | len v == len command} @-}
singleLine :: String -> String
singleLine = map replace
  where
    replace '\n' = ' '
    replace '\r' = ' '
    replace character = character
