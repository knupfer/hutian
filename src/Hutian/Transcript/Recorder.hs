{-# LANGUAGE OverloadedStrings #-}

-- | IO for transcript recording: file lifecycle, locking, and reasoning
-- buffering. The event model, JSON encoding, and Markdown rendering are pure and
-- live in "Hutian.Transcript".
module Hutian.Transcript.Recorder
  ( Transcript
  , openTranscript
  , closeTranscript
  , recordTranscript
  , recordTranscriptMaybe
  , withTranscript
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (bracket, finally, onException)
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Hutian.Context (TurnContext)
import Hutian.Transcript
  ( TranscriptEvent (..)
  , TranscriptPaths (..)
  , encodeTranscriptEvent
  , renderTranscriptEvent
  , transcriptPaths
  )
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO
  ( BufferMode (LineBuffering)
  , Handle
  , IOMode (WriteMode)
  , hClose
  , hFlush
  , hPutStr
  , hSetBuffering
  , openFile
  )
import System.Posix.Files (setFileMode)

-- | The reasoning chunks that have not reached Markdown yet. JSONL receives
-- each chunk immediately; Markdown waits for one merged block.
data PendingReasoning = PendingReasoning
  { pendingContext :: TurnContext
  , pendingChunks :: [String]
  }

-- | Two open files, the pending Markdown reasoning, and the write lock.
data Transcript = Transcript
  { transcriptJsonHandle :: Handle
  , transcriptMarkdownHandle :: Handle
  , transcriptPendingReasoning :: IORef (Maybe PendingReasoning)
  , transcriptLock :: MVar ()
  }

-- | Open the JSON Lines and Markdown files for a transcript path.
openTranscript :: FilePath -> IO Transcript
openTranscript path = do
  let paths = transcriptPaths path
      jsonPath = transcriptJsonPath paths
      markdownPath = transcriptMarkdownPath paths
  createDirectoryIfMissing True (takeDirectory jsonPath)
  jsonHandle <- openFile jsonPath WriteMode
  markdownHandle <-
    openFile markdownPath WriteMode `onException` hClose jsonHandle
  let closeBoth = hClose markdownHandle `finally` hClose jsonHandle
  ( do
      setFileMode jsonPath 0o600
      setFileMode markdownPath 0o600
      hSetBuffering jsonHandle LineBuffering
      hSetBuffering markdownHandle LineBuffering
    )
    `onException` closeBoth
  pending <- newIORef Nothing
  lock <- newMVar ()
  pure
    Transcript
      { transcriptJsonHandle = jsonHandle
      , transcriptMarkdownHandle = markdownHandle
      , transcriptPendingReasoning = pending
      , transcriptLock = lock
      }

-- | Run an action with an optional transcript and close it afterwards. A
-- relative path is resolved by 'openTranscript' before the action runs.
withTranscript :: Maybe FilePath -> (Maybe Transcript -> IO a) -> IO a
withTranscript Nothing action = action Nothing
withTranscript (Just path) action =
  bracket (openTranscript path) closeTranscript (action . Just)

-- | Flush any buffered reasoning and close both files.
closeTranscript :: Transcript -> IO ()
closeTranscript transcript =
  withMVar (transcriptLock transcript) $
    const (flushAndClose `finally` closeBoth)
  where
    flushAndClose = do
      flushReasoning transcript
      hFlush (transcriptJsonHandle transcript)
      hFlush (transcriptMarkdownHandle transcript)
    closeBoth =
      hClose (transcriptMarkdownHandle transcript)
        `finally` hClose (transcriptJsonHandle transcript)

-- | Record one event. Reasoning chunks reach JSONL immediately, while Markdown
-- buffers them until the next event.
recordTranscript :: Transcript -> TranscriptEvent -> IO ()
recordTranscript transcript event =
  withMVar (transcriptLock transcript) $ \_ ->
    recordTranscriptLocked transcript event

recordTranscriptLocked :: Transcript -> TranscriptEvent -> IO ()
recordTranscriptLocked transcript event =
  case event of
    TranscriptReasoning context text
      | not (null text) -> appendReasoning transcript context text
      | otherwise -> pure ()
    _ -> do
      flushReasoning transcript
      writeEvent transcript event

-- | 'recordTranscript' for an optional recorder.
recordTranscriptMaybe :: Maybe Transcript -> TranscriptEvent -> IO ()
recordTranscriptMaybe Nothing _ = pure ()
recordTranscriptMaybe (Just transcript) event = recordTranscript transcript event

appendReasoning :: Transcript -> TurnContext -> String -> IO ()
appendReasoning transcript context text = do
  pending <- readIORef (transcriptPendingReasoning transcript)
  case pending of
    Just current
      | pendingContext current == context -> do
          writeJsonEvent transcript (TranscriptReasoning context text)
          writePending (current {pendingChunks = text : pendingChunks current})
      | otherwise -> do
          flushReasoning transcript
          writeJsonEvent transcript (TranscriptReasoning context text)
          writePending (PendingReasoning context [text])
    Nothing -> do
      writeJsonEvent transcript (TranscriptReasoning context text)
      writePending (PendingReasoning context [text])
  where
    writePending pending =
      writeIORef (transcriptPendingReasoning transcript) (Just pending)

flushReasoning :: Transcript -> IO ()
flushReasoning transcript = do
  pending <- readIORef (transcriptPendingReasoning transcript)
  case pending of
    Nothing -> pure ()
    Just current -> do
      writeIORef (transcriptPendingReasoning transcript) Nothing
      writeMarkdownEvent
        transcript
        ( TranscriptReasoning
            (pendingContext current)
            (concat (reverse (pendingChunks current)))
        )

writeEvent :: Transcript -> TranscriptEvent -> IO ()
writeEvent transcript event = do
  at <- timestamp
  writeJsonEventAt transcript at event
  writeMarkdownEventAt transcript at event

writeJsonEvent :: Transcript -> TranscriptEvent -> IO ()
writeJsonEvent transcript event =
  timestamp >>= \at -> writeJsonEventAt transcript at event

writeMarkdownEvent :: Transcript -> TranscriptEvent -> IO ()
writeMarkdownEvent transcript event =
  timestamp >>= \at -> writeMarkdownEventAt transcript at event

writeJsonEventAt :: Transcript -> String -> TranscriptEvent -> IO ()
writeJsonEventAt transcript at event = do
  BL.hPut
    (transcriptJsonHandle transcript)
    (encodeTranscriptEvent at event)
  hFlush (transcriptJsonHandle transcript)

writeMarkdownEventAt :: Transcript -> String -> TranscriptEvent -> IO ()
writeMarkdownEventAt transcript at event = do
  hPutStr
    (transcriptMarkdownHandle transcript)
    (renderTranscriptEvent at event)
  hFlush (transcriptMarkdownHandle transcript)

timestamp :: IO String
timestamp =
  formatTime defaultTimeLocale "%FT%T%QZ" <$> getCurrentTime
