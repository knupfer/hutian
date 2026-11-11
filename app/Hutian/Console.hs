{-# LANGUAGE ScopedTypeVariables #-}

-- | The single boundary for terminal IO: writes, attributes, cursor styling,
-- and size. Other modules build strings and call these functions.
module Hutian.Console
  ( configureConsole
  , consoleFlush
  , consoleIsErrorInteractive
  , consoleIsInteractive
  , consoleWidth
  , consoleWrite
  , consoleWriteError
  , consoleWriteErrorLine
  , consoleWriteLine
  , consoleWriteToolCommand
  , printOutputLabel
  , resetTerminal
  , startPromptLine
  , withGrayOutput
  , withRawTerminal
  , withRespondingCursor
  , withoutEcho
  ) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (finally)
import Control.Monad (when)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Hutian.Editor (PromptMode)
import Hutian.Output (outputLabelText)
import Hutian.Render
  ( CursorStyle (..)
  , cursorStyleText
  , grayForeground
  , hideCursorText
  , reset
  , resetCursorText
  , showCursorText
  , startPromptText
  , toolPromptText
  )
import System.Console.Terminal.Size (Window (Window), size)
import System.IO
  ( BufferMode (LineBuffering)
  , hFlush
  , hIsTerminalDevice
  , hPutStr
  , hPutStrLn
  , hSetBuffering
  , hSetEncoding
  , stderr
  , stdin
  , stdout
  )
import System.Posix.IO (stdInput)
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
consoleWriteErrorLine = hPutStrLn stderr

consoleFlush :: IO ()
consoleFlush = hFlush stdout

consoleFlushError :: IO ()
consoleFlushError = hFlush stderr

consoleIsInteractive :: IO Bool
consoleIsInteractive = hIsTerminalDevice stdout

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
consoleWriteToolCommand toolName command = do
  interactive <- consoleIsInteractive
  if interactive
    then do
      width <- consoleWidth
      consoleWrite (toolPromptText toolName width command)
    else consoleWriteLine command
  consoleFlush

-- | Print the label shown on its own line before shell or tool output.
printOutputLabel :: PromptMode -> IO ()
printOutputLabel mode = do
  interactive <- consoleIsInteractive
  consoleWriteLine (outputLabelText mode interactive)
  consoleFlush

-- | Print streamed output in gray, restoring the terminal afterwards.
withGrayOutput :: IO a -> IO a
withGrayOutput action = do
  stdoutIsTerminal <- consoleIsInteractive
  stderrIsTerminal <- consoleIsErrorInteractive
  let restore = do
        when stdoutIsTerminal (consoleWrite reset >> consoleFlush)
        when stderrIsTerminal (consoleWriteError reset >> consoleFlushError)
  when stdoutIsTerminal (consoleWrite grayForeground >> consoleFlush)
  when stderrIsTerminal (consoleWriteError grayForeground >> consoleFlushError)
  action `finally` restore

-- | Show a blinking teal cursor while the model streams an answer, then return
-- to the pink user cursor. The blink is driven here because every streamed
-- write resets the terminal's own cursor blink timer. Non-terminal output is
-- left alone.
withRespondingCursor :: IO a -> IO a
withRespondingCursor action = do
  interactive <- consoleIsInteractive
  if not interactive
    then action
    else do
      consoleWrite (showCursorText ++ cursorStyleText RespondingCursor)
      consoleFlush
      blinkThread <- forkIO blinkCursor
      action `finally` do
        killThread blinkThread
        consoleWrite (showCursorText ++ cursorStyleText UserCursor)
        consoleFlush

-- | Toggle the cursor independently of streamed output, starting visible.
blinkCursor :: IO ()
blinkCursor = loop True
  where
    loop visible = do
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
  when interactive (consoleWrite (resetCursorText ++ "\ESC[0m") >> consoleFlush)

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
