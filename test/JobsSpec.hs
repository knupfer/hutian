module JobsSpec (tests) where

import Control.Concurrent (threadDelay)
import Data.List (find)
import Hutian.Jobs
import Hutian.Jobs.Runtime
  ( JobRuntime
  , killJob
  , listJobs
  , readJobOutput
  , startJob
  , withJobRuntime
  )
import Hutian.Render (grayForeground, grayOutputText, reset)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit
import TestSupport (stripAnsi, testJobId, withTemporaryDirectory)

tests :: TestTree
tests =
  testGroup
    "jobs"
    [ pureTests
    , runtimeTests
    ]

pureTests :: TestTree
pureTests =
  testGroup
    "model"
    [ testCase "status text distinguishes success, failure, and signals" $ do
        jobStatusText JobRunning @?= "running"
        jobStatusText (JobExited 0) @?= "exit 0"
        jobStatusText (JobExited 2) @?= "exit 2"
        jobStatusText (JobSignalled 15) @?= "signal 15"
    , testCase "exit codes map to explicit job statuses" $ do
        jobStatusFromExit ExitSuccess @?= JobExited 0
        jobStatusFromExit (ExitFailure 2) @?= JobExited 2
        jobStatusFromExit (ExitFailure (-15)) @?= JobSignalled 15
    , testCase "output line counts are clamped to the supported range" $ do
        lineCountValue (lineCount 0) @?= 1
        lineCountValue (lineCount (-1)) @?= 1
        lineCountValue (lineCount 1) @?= 1
        lineCountValue (lineCount maxJobOutputLines) @?= maxJobOutputLines
        lineCountValue (lineCount (maxJobOutputLines + 1)) @?= maxJobOutputLines
    , testCase "a summary line shows status, pid, and command" $ do
        jobSummaryLine runningJob @?= "job 1  running  pid 1234  sleep 10"
        jobSummaryLine finishedJob @?= "job 1  signal 15  sleep 10"
    , testCase "a start line is a single compact notice" $
        jobStartedLine runningJob @?= "job 1 started"
    , testCase "a styled start line keeps only the job text" $ do
        let styled = grayOutputText True (jobStartedLine runningJob)
        styled @?= grayForeground ++ "job 1 started" ++ reset
        stripAnsi styled @?= "job 1 started"
    , testCase "a summary line flattens a multi-line command" $
        jobSummaryLine (runningJob {jobCommand = "echo one\necho two"})
          @?= "job 1  running  pid 1234  echo one echo two"
    , testCase "the job list handles empty and populated tables" $ do
        jobListResult [] @?= "no background jobs"
        jobListResult [runningJob, finishedJob]
          @?= "job 1  running  pid 1234  sleep 10\njob 1  signal 15  sleep 10"
    , testCase "an output result reports status and truncation" $ do
        jobOutputResult runningJob "hello\n" OutputComplete @?= "job 1 running\nhello\n"
        jobOutputResult runningJob "hello\n" OutputTruncated
          @?= "job 1 running\n[output truncated; showing the most recent lines]\nhello\n"
        jobOutputResult runningJob "" OutputComplete @?= "job 1 running\n(no output)\n"
    , testCase "a kill result distinguishes stopped, finished, and stubborn jobs" $ do
        jobKillResult JobAlreadyFinished finishedJob @?= "job 1 already finished: signal 15"
        jobKillResult JobWasRunning finishedJob @?= "job 1 stopped: signal 15"
        jobKillResult JobWasRunning runningJob @?= "job 1 is still running"
    , testCase "output defaults and limits are stable" $ do
        lineCountValue defaultJobOutputLines @?= 50
        maxJobOutputLines @?= 500
    ]

runtimeTests :: TestTree
runtimeTests =
  testGroup
    "runtime"
    [ testCase "a job captures output and reports its exit" $
        withTemporaryDirectory "hutian-jobs" $ \directory ->
          withJobRuntime directory directory $ \runtime -> do
            started <- startJob runtime "printf hello"
            job <- startedJob started
            awaitFinished runtime (jobId job)
            current <- findJob runtime (jobId job)
            jobStatus current @?= JobExited 0
            output <- readJobOutput runtime (jobId job) (lineCount 10)
            case output of
              Left message -> assertFailure message
              Right (_, text, truncated) -> do
                text @?= "hello\n"
                truncated @?= OutputComplete
    , testCase "a job runs in the runtime working directory" $
        withTemporaryDirectory "hutian-jobs" $ \directory ->
          withTemporaryDirectory "hutian-jobs-cwd" $ \workingDirectory ->
            withJobRuntime directory workingDirectory $ \runtime -> do
              started <- startJob runtime "pwd > job-wd"
              job <- startedJob started
              awaitFinished runtime (jobId job)
              contents <- readFile (workingDirectory </> "job-wd")
              contents @?= workingDirectory ++ "\n"
    , testCase "an output tail is bounded and marked as truncated" $
        withTemporaryDirectory "hutian-jobs" $ \directory ->
          withJobRuntime directory directory $ \runtime -> do
            started <- startJob runtime "printf 'a\\nb\\nc\\n'"
            job <- startedJob started
            awaitFinished runtime (jobId job)
            output <- readJobOutput runtime (jobId job) (lineCount 1)
            case output of
              Left message -> assertFailure message
              Right (_, text, truncated) -> do
                text @?= "c\n"
                truncated @?= OutputTruncated
    , testCase "killing a stopped job escalates to SIGKILL" $
        withTemporaryDirectory "hutian-jobs" $ \directory ->
          withJobRuntime directory directory $ \runtime -> do
            started <- startJob runtime "kill -STOP $$"
            job <- startedJob started
            threadDelay 100000
            result <- killJob runtime (jobId job)
            case result of
              Left message -> assertFailure message
              Right (finished, activity) -> do
                activity @?= JobWasRunning
                assertBool "the job is no longer running" (jobStatus finished /= JobRunning)
    ]

runningJob :: Job
runningJob = Job (testJobId 1) "sleep 10" (Just 1234) "/tmp/hutian-jobs/1.log" JobRunning

finishedJob :: Job
finishedJob =
  runningJob
    { jobPid = Nothing
    , jobStatus = JobSignalled 15
    }

startedJob :: Either String Job -> IO Job
startedJob (Left message) = assertFailure message
startedJob (Right job) = pure job

findJob :: JobRuntime -> JobId -> IO Job
findJob runtime target = do
  current <- listJobs runtime
  case find ((== target) . jobId) current of
    Just job -> pure job
    Nothing -> assertFailure ("job " ++ jobIdText target ++ " disappeared")

awaitFinished :: JobRuntime -> JobId -> IO ()
awaitFinished runtime target = go (200 :: Int)
  where
    go 0 = pure ()
    go attempts = do
      current <- listJobs runtime
      case find ((== target) . jobId) current of
        Just job | jobStatus job == JobRunning ->
          threadDelay 10000 >> go (attempts - 1)
        _ -> pure ()
