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
import Hutian.Output (grayOutputText)
import Hutian.Render (grayForeground, reset)
import System.Exit (ExitCode (..))
import Test.Tasty
import Test.Tasty.HUnit
import TestSupport (stripAnsi, withTemporaryDirectory)

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
        jobStatusText (JobExited ExitSuccess) @?= "exit 0"
        jobStatusText (JobExited (ExitFailure 2)) @?= "exit 2"
        jobStatusText (JobExited (ExitFailure (-15))) @?= "signal 15"
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
        jobOutputResult runningJob "hello\n" False @?= "job 1 running\nhello\n"
        jobOutputResult runningJob "hello\n" True
          @?= "job 1 running\n[output truncated; showing the most recent lines]\nhello\n"
        jobOutputResult runningJob "" False @?= "job 1 running\n(no output)\n"
    , testCase "a kill result distinguishes stopped, finished, and stubborn jobs" $ do
        jobKillResult False finishedJob @?= "job 1 already finished: signal 15"
        jobKillResult True finishedJob @?= "job 1 stopped: signal 15"
        jobKillResult True runningJob @?= "job 1 is still running"
    , testCase "output defaults and limits are stable" $ do
        defaultJobOutputLines @?= 50
        maxJobOutputLines @?= 500
    ]

runtimeTests :: TestTree
runtimeTests =
  testGroup
    "runtime"
    [ testCase "a job captures output and reports its exit" $
        withTemporaryDirectory "hutian-jobs" $ \directory ->
          withJobRuntime directory $ \runtime -> do
            started <- startJob runtime "printf hello"
            job <- startedJob started
            awaitFinished runtime (jobId job)
            current <- findJob runtime (jobId job)
            jobStatus current @?= JobExited ExitSuccess
            output <- readJobOutput runtime (jobId job) 10
            case output of
              Left message -> assertFailure message
              Right (_, text, truncated) -> do
                text @?= "hello\n"
                truncated @?= False
    , testCase "an output tail is bounded and marked as truncated" $
        withTemporaryDirectory "hutian-jobs" $ \directory ->
          withJobRuntime directory $ \runtime -> do
            started <- startJob runtime "printf 'a\\nb\\nc\\n'"
            job <- startedJob started
            awaitFinished runtime (jobId job)
            output <- readJobOutput runtime (jobId job) 1
            case output of
              Left message -> assertFailure message
              Right (_, text, truncated) -> do
                text @?= "c\n"
                truncated @?= True
    , testCase "killing a stopped job escalates to SIGKILL" $
        withTemporaryDirectory "hutian-jobs" $ \directory ->
          withJobRuntime directory $ \runtime -> do
            started <- startJob runtime "kill -STOP $$"
            job <- startedJob started
            threadDelay 100000
            result <- killJob runtime (jobId job)
            case result of
              Left message -> assertFailure message
              Right (finished, wasRunning) -> do
                assertBool "the job was running" wasRunning
                assertBool "the job is no longer running" (jobStatus finished /= JobRunning)
    ]

runningJob :: Job
runningJob = Job (JobId 1) "sleep 10" (Just 1234) "/tmp/hutian-jobs/1.log" JobRunning

finishedJob :: Job
finishedJob =
  runningJob
    { jobPid = Nothing
    , jobStatus = JobExited (ExitFailure (-15))
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
