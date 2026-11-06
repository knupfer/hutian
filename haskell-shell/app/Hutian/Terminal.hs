{-# LANGUAGE ScopedTypeVariables #-}

-- | Raw terminal input handling for the LLM prompt.
module Hutian.Terminal
  ( promptForLlm
  , resetTerminal
  ) where

import Control.Exception (IOException, finally, try)
import Control.Monad (when)
import Data.Char (isControl)
import System.IO
import System.IO.Error (isEOFError)
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

-- | Read an LLM prompt as raw terminal input.
--
-- Returns 'Nothing' when the user cancels with @Ctrl+C@ and 'Just' the message
-- when the user submits it with @Ctrl+D@. Editing is intentionally small:
-- printable input, backspace/delete, and newlines are supported. Escape
-- sequences are consumed and ignored.
promptForLlm :: IO (Maybe String)
promptForLlm = withRawTerminal (readPrompt [])

resetTerminal :: IO ()
resetTerminal = do
  isTerminal <- hIsTerminalDevice stdout
  when isTerminal (putStr "\ESC[0m" >> hFlush stdout)

withRawTerminal :: IO a -> IO a
withRawTerminal action = do
  isTerminal <- hIsTerminalDevice stdin
  if not isTerminal
    then action
    else do
      originalAttributes <- getTerminalAttributes stdInput
      setTerminalAttributes stdInput (rawAttributes originalAttributes) Immediately
      action `finally` setTerminalAttributes stdInput originalAttributes Immediately

rawAttributes :: TerminalAttributes -> TerminalAttributes
rawAttributes original =
  let withoutInputModes =
        foldl withoutMode original
          [ ProcessInput
          , EnableEcho
          , EchoErase
          , EchoKill
          , KeyboardInterrupts
          , ExtendedFunctions
          ]
      withOutputModes =
        foldl withMode withoutInputModes
          [ ProcessOutput
          , MapLFtoCRLF
          ]
   in withTime (withMinInput withOutputModes 1) 0

readPrompt :: [String] -> IO (Maybe String)
readPrompt completedLines = do
  startPromptLine
  go completedLines ""
  where
    go completed current = do
      next <- readChar
      case next of
        Left err
          | isEOFError err -> submit completed current
          | otherwise -> ioError err
        Right char -> handleChar completed current char

    handleChar completed current char
      | char == ctrlD = submit completed current
      | char == ctrlC = cancel current
      | char == newline = continue completed current
      | char == carriageReturn = continue completed current
      | char == delete = erase completed current
      | char == backspace = erase completed current
      | char == escape = skipEscape >> go completed current
      | isControl char = go completed current
      | otherwise = do
          putChar char
          hFlush stdout
          go completed (current ++ [char])

    continue completed current = do
      putStr "\n"
      hFlush stdout
      startPromptLine
      go (current : completed) ""

    erase completed current =
      case reverse current of
        [] -> go completed current
        _ : rest -> do
          putStr "\b \b"
          hFlush stdout
          go completed (reverse rest)

    submit completed current = do
      finishPromptLine (not (null current))
      pure (Just (unlines (reverse (prependNonEmpty current completed))))

    cancel current = do
      finishPromptLine (not (null current))
      pure Nothing

readChar :: IO (Either IOException Char)
readChar = try (hGetChar stdin)

skipEscape :: IO ()
skipEscape = do
  next <- readChar
  case next of
    Right '[' -> skipUntilFinal
    Right 'O' -> skipUntilFinal
    _ -> pure ()
  where
    skipUntilFinal = do
      next <- readChar
      case next of
        Right char
          | char >= '@' && char <= '~' -> pure ()
          | otherwise -> skipUntilFinal
        Left _ -> pure ()

prependNonEmpty :: String -> [String] -> [String]
prependNonEmpty value values
  | null value = values
  | otherwise = value : values

startPromptLine :: IO ()
startPromptLine = do
  isTerminal <- hIsTerminalDevice stdout
  when isTerminal (putStr promptBackground >> hFlush stdout)

finishPromptLine :: Bool -> IO ()
finishPromptLine hasPartial = do
  isTerminal <- hIsTerminalDevice stdout
  when isTerminal $ do
    when hasPartial (putStr "\n")
    putStr "\ESC[0m"
  hFlush stdout

promptBackground :: String
promptBackground = "\ESC[48;2;40;0;40m\ESC[K"

ctrlC, ctrlD, newline, carriageReturn, delete, backspace, escape :: Char
ctrlC = '\x03'
ctrlD = '\x04'
newline = '\n'
carriageReturn = '\r'
delete = '\DEL'
backspace = '\b'
escape = '\ESC'
