{-# LANGUAGE ScopedTypeVariables #-}

-- | The single boundary for terminal IO: writes, attributes, cursor styling,
-- and size. Other modules build strings and call these functions.
module Hutian.Console
  ( configureConsole
  , confirmToolApproval
  , confirmToolCommand
  , confirmToolCommandIn
  , consoleFlush
  , consoleInputReady
  , consoleIsErrorInteractive
  , consoleIsInteractive
  , consoleWidth
  , consoleWrite
  , consoleWriteError
  , consoleWriteErrorLine
  , consoleWriteGrayLine
  , consoleWriteLine
  , consoleWriteSubagentPrompt
  , consoleWriteToolCommand
  , consoleWriteToolCommandIn
  , consoleWriteToolLabel
  , consoleWriteToolLabelIn
  , consoleWriteToolTextIn
  , printOutputLabel
  , printOutputLabelIn
  , resetTerminal
  , startPromptLine
  , withGrayOutput
  , withRawTerminal
  , withRespondingCursor
  , withSubagentCursor
  , withoutEcho
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, withMVar)
import Control.Exception (IOException, catch, finally, mask)
import Control.Monad (when)
import Data.Foldable (for_)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Hutian.Highlight (sanitize, sanitizeOutput)
import Hutian.Render
  ( CursorStyle (..)
  , OutputKind
  , ToolApproval (..)
  , TurnContext (..)
  , approvalDecisionText
  , approvalForKey
  , approvePromptText
  , cursorStyleText
  , grayForeground
  , grayOutputText
  , hideCursorText
  , outputLabelTextIn
  , popCursorStyle
  , pushCursorStyle
  , reset
  , resetCursorText
  , showCursorText
  , startPromptText
  , subagentPromptText
  , toolLabelTextIn
  , toolPromptTextIn
  , toolTextPromptTextIn
  )
import System.Console.Terminal.Size (Window (Window), size)
import System.IO
  ( BufferMode (LineBuffering)
  , hFlush
  , hGetBufNonBlocking
  , hIsTerminalDevice
  , hPutStr
  , hPutStrLn
  , hReady
  , hSetBuffering
  , hSetEncoding
  , stderr
  , stdin
  , stdout
  )
import System.IO.Error (isEOFError)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Files (deviceID, fileID, getFdStatus)
import System.Posix.IO (stdError, stdInput, stdOutput)
import System.Posix.Terminal
  ( TerminalAttributes
  , TerminalMode (..)
  , TerminalState (..)
  , getTerminalAttributes
  , setTerminalAttributes
  , withMinInput
  , withMode
  , withTime
  , withoutMode
  )

-- | Serialize writes that show or hide the cursor. A modal prompt holds this
-- lock while it waits so the responding-cursor blink thread cannot reveal the
-- cursor again.
{-# NOINLINE cursorVisibilityLock #-}
cursorVisibilityLock :: MVar ()
cursorVisibilityLock = unsafePerformIO (newMVar ())

-- | Configure the process's text encoding and buffering before any prompt.
configureConsole :: IO ()
configureConsole = do
  setLocaleEncoding utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  hSetBuffering stdout LineBuffering

consoleWrite :: String -> IO ()
consoleWrite = putStr

consoleWriteLine :: String -> IO ()
consoleWriteLine = putStrLn

consoleWriteError :: String -> IO ()
consoleWriteError = hPutStr stderr

consoleWriteErrorLine :: String -> IO ()
consoleWriteErrorLine message = hPutStrLn stderr (sanitizeOutput message)

consoleFlush :: IO ()
consoleFlush = hFlush stdout

consoleFlushError :: IO ()
consoleFlushError = hFlush stderr

consoleIsInteractive :: IO Bool
consoleIsInteractive = hIsTerminalDevice stdout

-- | Whether input is already buffered, without blocking. Used to coalesce a
-- pasted burst into one redraw. End of input reports @False@ so the key reader
-- can surface it as a normal submit.
consoleInputReady :: IO Bool
consoleInputReady = hReady stdin `catch` ignoreEOF
  where
    ignoreEOF err
      | isEOFError err = pure False
      | otherwise = ioError err

consoleIsErrorInteractive :: IO Bool
consoleIsErrorInteractive = hIsTerminalDevice stderr

consoleWidth :: IO Int
consoleWidth = do
  window <- size
  pure $
    case window of
      Just (Window _ width) | width > 0 -> width
      _ -> 80

-- | Show a command requested by the model with the tool-call prefix.
consoleWriteToolCommand :: String -> String -> IO ()
consoleWriteToolCommand = consoleWriteToolCommandIn MainContext

-- | Like 'consoleWriteToolCommand', but a subagent's tools use the subagent
-- blue instead of the main tool teal.
consoleWriteToolCommandIn :: TurnContext -> String -> String -> IO ()
consoleWriteToolCommandIn context toolName command = do
  interactive <- consoleIsInteractive
  if interactive
    then do
      width <- consoleWidth
      consoleWrite (toolPromptTextIn context (sanitize toolName) width command)
    else consoleWriteLine (sanitizeOutput command)
  consoleFlush

-- | Show a tool call whose argument is data, such as MCP JSON arguments.
consoleWriteToolTextIn :: TurnContext -> String -> String -> IO ()
consoleWriteToolTextIn context toolName text = do
  interactive <- consoleIsInteractive
  if interactive
    then do
      width <- consoleWidth
      consoleWrite (toolTextPromptTextIn context (sanitize toolName) width text)
    else consoleWriteLine (sanitizeOutput text)
  consoleFlush

-- | Show a tool call that has no command of its own.
consoleWriteToolLabel :: String -> IO ()
consoleWriteToolLabel = consoleWriteToolLabelIn MainContext

consoleWriteToolLabelIn :: TurnContext -> String -> IO ()
consoleWriteToolLabelIn context toolName = do
  interactive <- consoleIsInteractive
  let name = sanitize toolName
  consoleWriteLine $
    if interactive
      then toolLabelTextIn context name
      else "tool " ++ name ++ ":"
  consoleFlush

-- | Announce the subagent and the task prompt it was given. The label is blue
-- when output goes to a terminal and plain otherwise.
consoleWriteSubagentPrompt :: String -> String -> IO ()
consoleWriteSubagentPrompt modelName prompt = do
  interactive <- consoleIsInteractive
  let name = sanitize modelName
      task = sanitize prompt
  if interactive
    then do
      width <- consoleWidth
      consoleWrite (subagentPromptText name width task)
    else consoleWrite ("subagent " ++ name ++ ":\n" ++ task ++ taskNewline)
  consoleFlush
  where
    taskNewline
      | null prompt = ""
      | otherwise = "\n"

-- | Show one line of command or job output in gray. Streamed and multi-line
-- output goes through 'withGrayOutput'; a single line is written atomically so
-- the terminal sees exactly one colour sequence and one reset.
consoleWriteGrayLine :: String -> IO ()
consoleWriteGrayLine message = do
  interactive <- consoleIsInteractive
  consoleWriteLine (grayOutputText interactive (sanitize message))
  consoleFlush

-- | The result line shown after the options are removed.
approvalResult :: ToolApproval -> String
approvalResult ToolApproved = "Yes"
approvalResult ToolDenied = "No"
approvalResult ToolAborted = "Aborted"

-- | Show a requested command and ask for single-key approval. Returns
-- 'ToolDenied' without prompting when output is not a terminal. Input typed
-- while the model was streaming is discarded after switching to raw mode,
-- because canonical-mode buffering hides it from 'hReady'.
confirmToolCommand :: String -> String -> IO ToolApproval
confirmToolCommand = confirmToolCommandIn MainContext

confirmToolCommandIn :: TurnContext -> String -> String -> IO ToolApproval
confirmToolCommandIn context toolName command = do
  consoleWriteToolCommandIn context toolName command
  confirmToolApproval

-- | Ask for single-key approval after the caller has shown the requested call.
-- Returns 'ToolDenied' without prompting when output is not a terminal.
confirmToolApproval :: IO ToolApproval
confirmToolApproval = do
  interactive <- consoleIsInteractive
  inputIsTerminal <- hIsTerminalDevice stdin
  if not interactive || not inputIsTerminal
    then pure ToolDenied
    else
      withRawTerminal $ do
        drainPendingInput
        withHiddenCursor $ do
          consoleWrite approvePromptText
          consoleFlush
          approval <- readApproval
          consoleWrite (approvalDecisionText (approvalResult approval))
          consoleFlush
          pure approval
  where
    readApproval = do
      key <- getChar `catch` \(_ :: IOException) -> pure 'n'
      maybe readApproval pure (approvalForKey key)

-- | Keep the cursor hidden until the action finishes, without letting the blink
-- thread reveal it in the meantime.
withHiddenCursor :: IO a -> IO a
withHiddenCursor action =
  withMVar cursorVisibilityLock $ \_ -> do
    consoleWrite hideCursorText
    consoleFlush
    action `finally` do
      consoleWrite showCursorText
      consoleFlush

-- | Discard input that was typed before the approval prompt appeared. Byte
-- reads are used on purpose: 'hReady' and 'getChar' both miss data that the
-- line discipline buffered in canonical mode, and a text read can prefetch a
-- whole burst that the next 'getChar' would then consume as an approval.
drainPendingInput :: IO ()
drainPendingInput =
  allocaBytes drainChunk $ \buffer -> go buffer
  where
    go buffer = do
      count <-
        hGetBufNonBlocking stdin buffer drainChunk
          `catch` \(_ :: IOException) -> pure 0
      when (count > 0) (go buffer)

drainChunk :: Int
drainChunk = 4096

-- | Print the label shown on its own line before shell or tool output.
printOutputLabel :: OutputKind -> IO ()
printOutputLabel = printOutputLabelIn MainContext

printOutputLabelIn :: TurnContext -> OutputKind -> IO ()
printOutputLabelIn context kind = do
  interactive <- consoleIsInteractive
  consoleWriteLine (outputLabelTextIn context kind interactive)
  consoleFlush

-- | Print streamed output in gray, restoring the terminal afterwards. When
-- stdout and stderr are the same terminal, style it only through stdout:
-- writing the sequence to both streams would duplicate it.
withGrayOutput :: IO a -> IO a
withGrayOutput action = do
  styleStdout <- consoleIsInteractive
  stderrIsTerminal <- consoleIsErrorInteractive
  styleStderr <-
    if not stderrIsTerminal
      then pure False
      else
        if styleStdout
          then not <$> sameTerminal
          else pure True
  let writeSequence sequenceText = do
        when styleStdout (consoleWrite sequenceText >> consoleFlush)
        when styleStderr (consoleWriteError sequenceText >> consoleFlushError)
      restore = writeSequence reset
  writeSequence grayForeground
  action `finally` restore

-- | Whether stdout and stderr refer to the same terminal device.
sameTerminal :: IO Bool
sameTerminal = do
  outputStatus <- getFdStatus stdOutput
  errorStatus <- getFdStatus stdError
  pure
    ( deviceID outputStatus == deviceID errorStatus
        && fileID outputStatus == fileID errorStatus
    )

-- | Show a blinking teal cursor while the model streams an answer, then return
-- to the pink user cursor. Scopes nest: a subagent switches the cursor to blue
-- for its turn and restores the parent's colour when it finishes.
withRespondingCursor :: IO a -> IO a
withRespondingCursor = withRespondingCursorIn RespondingCursor

-- | Show the blinking cursor in the subagent blue for the duration of a nested
-- subagent turn.
withSubagentCursor :: IO a -> IO a
withSubagentCursor = withRespondingCursorIn SubagentCursor

withRespondingCursorIn :: CursorStyle -> IO a -> IO a
withRespondingCursorIn style action = do
  interactive <- consoleIsInteractive
  if not interactive
    then action
    else mask $ \restore -> do
      enterRespondingCursor style
      restore action `finally` exitRespondingCursor

-- | Cursor state shared by nested responding scopes. The stack records the
-- colour each scope entered with, so leaving a subagent restores the main
-- turn's teal rather than assuming the user cursor.
data CursorState = CursorState
  { cursorBlinkThread :: Maybe ThreadId
  , cursorStyleStack :: [CursorStyle]
  }

{-# NOINLINE respondingCursorState #-}
respondingCursorState :: MVar CursorState
respondingCursorState = unsafePerformIO (newMVar (CursorState Nothing []))

enterRespondingCursor :: CursorStyle -> IO ()
enterRespondingCursor style =
  modifyMVar_ respondingCursorState $ \state -> do
    blinkThread <-
      case cursorBlinkThread state of
        Just thread -> pure (Just thread)
        Nothing -> Just <$> forkIO blinkCursor
    consoleWrite (showCursorText ++ cursorStyleText style)
    consoleFlush
    pure
      state
        { cursorBlinkThread = blinkThread
        , cursorStyleStack = pushCursorStyle style (cursorStyleStack state)
        }

exitRespondingCursor :: IO ()
exitRespondingCursor =
  modifyMVar_ respondingCursorState $ \state -> do
    let (activeStyle, remaining) = popCursorStyle (cursorStyleStack state)
        state' = state {cursorStyleStack = remaining}
    case remaining of
      [] -> do
        for_ (cursorBlinkThread state') killThread
        consoleWrite (showCursorText ++ cursorStyleText activeStyle)
        consoleFlush
        pure state' {cursorBlinkThread = Nothing}
      _ -> do
        consoleWrite (showCursorText ++ cursorStyleText activeStyle)
        consoleFlush
        pure state'

-- | Toggle the cursor independently of streamed output, starting visible.
-- Cursor writes share 'cursorVisibilityLock' so a modal prompt can keep the
-- cursor hidden while it waits for input.
blinkCursor :: IO ()
blinkCursor = loop True
  where
    loop visible = do
      withMVar cursorVisibilityLock $ \_ -> do
        consoleWrite (if visible then showCursorText else hideCursorText)
        consoleFlush
      threadDelay blinkInterval
      loop (not visible)

blinkInterval :: Int
blinkInterval = 500000

-- | Restore the terminal cursor and colours after the interactive loop.
resetTerminal :: IO ()
resetTerminal = do
  interactive <- consoleIsInteractive
  when interactive (consoleWrite (resetCursorText ++ reset) >> consoleFlush)

-- | Clear the line before the first prompt frame is drawn.
startPromptLine :: IO ()
startPromptLine = do
  consoleWrite (startPromptText ++ cursorStyleText UserCursor)
  consoleFlush

withRawTerminal :: IO a -> IO a
withRawTerminal = withTerminalAttributes rawAttributes

-- | Keep typed-ahead characters out of streamed output. Characters typed while
-- this action runs stay buffered and are rendered by the next prompt instead.
withoutEcho :: IO a -> IO a
withoutEcho = withTerminalAttributes (`withoutMode` EnableEcho)

-- | Run an action with adjusted terminal attributes, restoring the originals
-- afterwards. Non-terminal input is left untouched.
withTerminalAttributes :: (TerminalAttributes -> TerminalAttributes) -> IO a -> IO a
withTerminalAttributes adjust action = do
  isTerminal <- hIsTerminalDevice stdin
  if not isTerminal
    then action
    else do
      originalAttributes <- getTerminalAttributes stdInput
      setTerminalAttributes stdInput (adjust originalAttributes) Immediately
      action `finally` setTerminalAttributes stdInput originalAttributes Immediately

rawAttributes :: TerminalAttributes -> TerminalAttributes
rawAttributes original =
  let withoutInputModes =
        foldl' withoutMode original
          [ ProcessInput
          , EnableEcho
          , EchoErase
          , EchoKill
          , KeyboardInterrupts
          , ExtendedFunctions
          ]
      withOutputModes =
        foldl' withMode withoutInputModes
          [ ProcessOutput
          , MapLFtoCRLF
          ]
   in withTime (withMinInput withOutputModes 1) 0
