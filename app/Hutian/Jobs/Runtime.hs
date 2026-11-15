{-# LANGUAGE ScopedTypeVariables #-}

-- | Runtime for background jobs: starts commands with their output redirected
-- to per-job logs, tracks exits, and stops process groups. Pure job data and
-- text live in "Hutian.Jobs".
module Hutian.Jobs.Runtime
  ( JobRuntime
  , withJobRuntime
  , startJob
  , listJobs
  , readJobOutput
  , killJob
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
  ( MVar
  , modifyMVar
  , modifyMVar_
  , newMVar
  , readMVar
  )
import Control.Exception (IOException, bracket, catch, onException, try)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.Either (fromRight)
import Data.List (find)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncodingError
import Hutian.Jobs
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode)
import System.FilePath ((</>), (<.>))
import System.IO
  ( Handle
  , IOMode (..)
  , SeekMode (..)
  , hClose
  , hFileSize
  , hSeek
  , openFile
  , withBinaryFile
  )
import System.Posix.Signals (Signal, sigKILL, sigTERM, signalProcessGroup)
import System.Process
  ( CreateProcess (..)
  , ProcessHandle
  , StdStream (..)
  , createProcess_
  , getPid
  , proc
  , terminateProcess
  , waitForProcess
  )

-- | The live job table for one shell session.
data JobRuntime = JobRuntime
  { runtimeDirectory :: FilePath
  , runtimeState :: MVar JobState
  }

data JobState = JobState
  { stateNextId :: Int
  , stateJobs :: [RuntimeJob]
  }

data RuntimeJob = RuntimeJob
  { runtimeInfo :: Job
  , runtimeHandle :: ProcessHandle
  }

-- | Create a job runtime for one shell session and stop every remaining job
-- when the action finishes. Like the sandbox itself, jobs do not survive the
-- shell that owns them.
withJobRuntime :: FilePath -> (JobRuntime -> IO a) -> IO a
withJobRuntime directory = bracket (newJobRuntime directory) shutdownJobs

newJobRuntime :: FilePath -> IO JobRuntime
newJobRuntime directory = do
  state <- newMVar (JobState 1 [])
  pure (JobRuntime directory state)

-- | Start a command in the background. The command runs in a new process group
-- with stdin closed and stdout/stderr written to a log. Returns as soon as the
-- process is started; a waiter thread records the exit status.
startJob :: JobRuntime -> String -> IO (Either String Job)
startJob runtime command = do
  createDirectoryIfMissing True (runtimeDirectory runtime)
  result <- modifyMVar (runtimeState runtime) $ \state ->
    if runningCount state >= maxRunningJobs
      then pure (state, Left tooManyJobs)
      else do
        spawned <- spawnJob runtime (stateNextId state) command
        pure $ case spawned of
          Left message -> (state, Left message)
          Right runtimeJob ->
            let state' =
                  state
                    { stateNextId = stateNextId state + 1
                    , stateJobs = stateJobs state ++ [runtimeJob]
                    }
             in (state', Right runtimeJob)
  case result of
    Left message -> pure (Left message)
    Right runtimeJob -> do
      _ <- forkIO (waitForJob runtime runtimeJob)
      pure (Right (runtimeInfo runtimeJob))

-- | Jobs are listed in start order. Finished jobs stay available for output
-- and status reporting until the shell exits.
listJobs :: JobRuntime -> IO [Job]
listJobs runtime = map runtimeInfo <$> listRuntimeJobs runtime

listRuntimeJobs :: JobRuntime -> IO [RuntimeJob]
listRuntimeJobs runtime = stateJobs <$> readMVar (runtimeState runtime)

-- | Read a bounded tail of a job's log. The job and whether the output was
-- truncated are returned so both the terminal and the model can describe it.
readJobOutput :: JobRuntime -> JobId -> Int -> IO (Either String (Job, String, Bool))
readJobOutput runtime target requestedLines = do
  found <- findJob runtime target
  case found of
    Nothing -> pure (noSuchJob target)
    Just job -> do
      let lineCount = clamp 1 maxJobOutputLines requestedLines
      (output, truncated) <- readLogTail (jobLogPath job) lineCount
      pure (Right (job, output, truncated))

-- | Ask a job to stop, then force it if it does not exit. The final job and
-- whether it was still running when the request arrived are returned.
killJob :: JobRuntime -> JobId -> IO (Either String (Job, Bool))
killJob runtime target = do
  found <- findRuntimeJob runtime target
  case found of
    Nothing -> pure (noSuchJob target)
    Just runtimeJob -> do
      let job = runtimeInfo runtimeJob
      if jobStatus job /= JobRunning
        then pure (Right (job, False))
        else do
          signalRuntimeJob runtimeJob sigTERM
          waitForExit runtime target 50
          current <- findJob runtime target
          case current of
            Nothing -> pure (noSuchJob target)
            Just job'
              | jobStatus job' == JobRunning -> do
                  signalRuntimeJob runtimeJob sigKILL
                  waitForExit runtime target 50
                  final <- findJob runtime target
                  pure (Right (fromMaybe job' final, True))
              | otherwise -> pure (Right (job', True))

maxRunningJobs :: Int
maxRunningJobs = 8

tooManyJobs :: String
tooManyJobs = "too many background jobs are already running; stop one first"

runningCount :: JobState -> Int
runningCount state = length (filter isRunning (stateJobs state))

isRunning :: RuntimeJob -> Bool
isRunning runtimeJob = jobStatus (runtimeInfo runtimeJob) == JobRunning

spawnJob :: JobRuntime -> Int -> String -> IO (Either String RuntimeJob)
spawnJob runtime nextId command = do
  let path = logPath runtime nextId
  logHandle <- openFile path AppendMode
  let closeHandle = ignoreIO (hClose logHandle)
  ( do
      (_, _, _, handle) <- createProcess_ "hutian-job" (processSpec logHandle command)
      pid <- getPid handle
      ignoreIO (hClose logHandle)
      pure . Right $
        RuntimeJob
          (Job (JobId nextId) command (fromIntegral <$> pid) path JobRunning)
          handle
    )
    `onException` closeHandle
    `catch` \(exception :: IOException) ->
      pure (Left ("cannot start background job: " ++ show exception))

-- | stdout and stderr share one append handle so the child writes to a single
-- open file description and the interleaving stays in process order.
processSpec :: Handle -> String -> CreateProcess
processSpec logHandle command =
  (proc "/bin/sh" ["-c", command])
    { std_in = NoStream
    , std_out = UseHandle logHandle
    , std_err = UseHandle logHandle
    , create_group = True
    , close_fds = True
    }

logPath :: JobRuntime -> Int -> FilePath
logPath runtime nextId = runtimeDirectory runtime </> show nextId <.> "log"

waitForJob :: JobRuntime -> RuntimeJob -> IO ()
waitForJob runtime runtimeJob = do
  exitCode <- waitForProcess (runtimeHandle runtimeJob)
  finishJob runtime (jobId (runtimeInfo runtimeJob)) exitCode

finishJob :: JobRuntime -> JobId -> ExitCode -> IO ()
finishJob runtime target exitCode =
  modifyMVar_ (runtimeState runtime) $ \state ->
    pure state { stateJobs = map finish (stateJobs state) }
  where
    finish runtimeJob
      | jobId info == target
      , jobStatus info == JobRunning =
          runtimeJob { runtimeInfo = info { jobStatus = JobExited exitCode } }
      | otherwise = runtimeJob
      where
        info = runtimeInfo runtimeJob

findJob :: JobRuntime -> JobId -> IO (Maybe Job)
findJob runtime target = fmap runtimeInfo <$> findRuntimeJob runtime target

noSuchJob :: JobId -> Either String a
noSuchJob target = Left ("no such job: " ++ jobIdText target)

findRuntimeJob :: JobRuntime -> JobId -> IO (Maybe RuntimeJob)
findRuntimeJob runtime target =
  find (\runtimeJob -> jobId (runtimeInfo runtimeJob) == target)
    <$> listRuntimeJobs runtime

signalRuntimeJob :: RuntimeJob -> Signal -> IO ()
signalRuntimeJob runtimeJob signal =
  case jobPid (runtimeInfo runtimeJob) of
    Just pid -> ignoreIO (signalProcessGroup signal (fromIntegral pid))
    Nothing -> ignoreIO (terminateProcess (runtimeHandle runtimeJob))

waitForExit :: JobRuntime -> JobId -> Int -> IO ()
waitForExit _ _ 0 = pure ()
waitForExit runtime target attempts = do
  found <- findJob runtime target
  case found of
    Just job | jobStatus job == JobRunning ->
      threadDelay 20000 >> waitForExit runtime target (attempts - 1)
    _ -> pure ()

shutdownJobs :: JobRuntime -> IO ()
shutdownJobs runtime = do
  running <- filter isRunning <$> listRuntimeJobs runtime
  case running of
    [] -> pure ()
    _ -> do
      forM_ running (`signalRuntimeJob` sigTERM)
      threadDelay 200000
      remaining <- filter isRunning <$> listRuntimeJobs runtime
      forM_ remaining (`signalRuntimeJob` sigKILL)

-- | Read at most @lineLimit@ lines from the end of a file. At most 64 KiB is
-- read, so a chatty job cannot make reading its output unbounded.
readLogTail :: FilePath -> Int -> IO (String, Bool)
readLogTail path lineLimit = do
  result <-
    try (withBinaryFile path ReadMode (readTail (max 1 lineLimit)))
      :: IO (Either IOException (String, Bool))
  pure (fromRight ("", False) result)
  where
    readTail limit handle = do
      size <- hFileSize handle
      let window = 65536 :: Integer
          start = max 0 (size - window)
      hSeek handle AbsoluteSeek start
      bytes <- BS.hGet handle (fromIntegral (size - start))
      let text = Text.unpack (TextEncoding.decodeUtf8With TextEncodingError.lenientDecode bytes)
          withoutPartial = if start > 0 then dropFirstLine text else text
          allLines = lines withoutPartial
          selected = drop (max 0 (length allLines - limit)) allLines
          truncated = start > 0 || length allLines > limit
      pure (unlines selected, truncated)

    dropFirstLine value =
      case break (== '\n') value of
        (_, []) -> ""
        (_, _ : rest) -> rest

clamp :: Int -> Int -> Int -> Int
clamp lower upper = min upper . max lower

ignoreIO :: IO () -> IO ()
ignoreIO action = action `catch` \(_ :: IOException) -> pure ()
