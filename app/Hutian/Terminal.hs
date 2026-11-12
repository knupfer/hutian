{-# LANGUAGE ScopedTypeVariables #-}

-- | Terminal prompt driver: reads keys, obtains terminal width and filesystem
-- completions, and writes frames through "Hutian.Console". Prompt policy is
-- pure and lives in "Hutian.Prompt".
module Hutian.Terminal
  ( promptForInput
  ) where

import Control.Exception (IOException, catch, try)
import Control.Monad (unless, when)
import Data.List (isPrefixOf)
import Hutian.Completion (CompletionKind (..), chooseCompletion)
import Hutian.Console
  ( consoleFlush
  , consoleIsInteractive
  , consoleWidth
  , consoleWrite
  , consoleWriteLine
  , startPromptLine
  , withRawTerminal
  )
import Hutian.Editor
  ( Prompt
  , PromptBuffers
  , PromptMode (..)
  , activePrompt
  , applyCompletionChoice
  , clearSuggestions
  , commandCompletionTarget
  , completeCommand
  , completionTarget
  , currentLine
  , cursorPosition
  , cursorToEnd
  , promptText
  )
import Hutian.History (History)
import Hutian.Key (Key (..), classifyChar, csiKey)
import Hutian.Paths (formatCompletion, splitPathWord)
import Hutian.Prompt
  ( PromptOutcome (..)
  , PromptState
  , PromptStep (..)
  , acceptCompletion
  , initialPromptState
  , promptBuffers
  , promptMode
  , stepPrompt
  )
import Hutian.Render (moveCursorText, prefixFor, promptRenderText, reset)
import System.Directory (doesDirectoryExist, listDirectory)
import System.IO.Error (isEOFError)

data PromptContext = PromptContext
  { contextCommands :: [String]
  , contextTermPrompt :: String
  , contextHistory :: History
  }

-- | Read one prompt as raw terminal input.
--
-- Returns the mode that was active when the prompt finished, together with
-- 'Nothing' when the user presses @Ctrl+D@ on empty input and 'Just' the
-- submitted text otherwise. @Ctrl+X@ toggles between 'LlmMode' and
-- 'TermMode'. In term mode, Enter executes the command, Up/Down browse
-- history, and a trailing backslash continues the command on the next line.
-- Ctrl+C clears the current input.
promptForInput :: [String] -> String -> PromptMode -> History -> PromptBuffers -> IO (PromptMode, Maybe String, PromptBuffers)
promptForInput commands termPrompt mode history buffers = do
  interactive <- consoleIsInteractive
  let context =
        PromptContext
          { contextCommands = commands
          , contextTermPrompt = termPrompt
          , contextHistory = history
          }
  withRawTerminal $ do
    when interactive startPromptLine
    readPrompt interactive context (initialPromptState mode buffers)

readPrompt :: Bool -> PromptContext -> PromptState -> IO (PromptMode, Maybe String, PromptBuffers)
readPrompt interactive context state = do
  position <- renderPrompt interactive context 0 state
  loop position state
  where
    loop position currentState = do
      key <- readKey
      case stepPrompt (contextHistory context) key currentState of
        StepContinue next -> loop position next
        StepRedraw next -> do
          nextPosition <- renderPrompt interactive context (fst position) next
          loop nextPosition next
        StepMoveCursor prompt next -> do
          nextPosition <- promptPosition (contextTermPrompt context) (promptMode next) prompt
          writeCursorMove position nextPosition
          loop nextPosition next
        StepComplete next -> do
          let current = activePrompt (promptMode next) (promptBuffers next)
          completed <- completeInput (contextCommands context) current
          let next' = acceptCompletion completed next
          nextPosition <- renderPrompt interactive context (fst position) next'
          loop nextPosition next'
        StepFinish outcome prompt next -> do
          finishPrompt interactive context (promptMode next) (fst position) prompt
          case outcome of
            PromptSubmitted ->
              pure (promptMode next, Just (promptText prompt), promptBuffers next)
            PromptExited -> do
              when interactive (consoleWriteLine "" >> consoleFlush)
              pure (promptMode next, Nothing, promptBuffers next)

promptPosition :: String -> PromptMode -> Prompt -> IO (Int, Int)
promptPosition termPrompt mode prompt = do
  width <- consoleWidth
  pure (cursorPosition width (prefixFor termPrompt mode) prompt)

writeCursorMove :: (Int, Int) -> (Int, Int) -> IO ()
writeCursorMove oldPosition newPosition = do
  consoleWrite (moveCursorText oldPosition newPosition)
  consoleFlush

completeInput :: [String] -> Prompt -> IO Prompt
completeInput commands prompt =
  case commandCompletionTarget prompt of
    Just (_, word)
      | '/' `notElem` word -> pure (completeCommand commands prompt)
    _ -> completePath prompt

completePath :: Prompt -> IO Prompt
completePath prompt = do
  let (start, word) = completionTarget prompt
      (directoryPart, base) = splitPathWord word
      directory = if null directoryPart then "." else directoryPart
  entries <- listDirectory directory `catch` \(_ :: IOException) -> pure []
  completions <- mapM (completeEntry directoryPart) (filter (base `isPrefixOf`) entries)
  let afterCursor = drop (start + length word) (currentLine prompt)
      completion = chooseCompletion PathCompletion word afterCursor completions
  pure (applyCompletionChoice start word completion prompt)

completeEntry :: FilePath -> FilePath -> IO FilePath
completeEntry directoryPart entry = do
  let candidate = directoryPart ++ entry
  isDirectory <- doesDirectoryExist candidate
  pure (formatCompletion candidate isDirectory)

readKey :: IO Key
readKey = do
  next <- readChar
  case next of
    Left err
      | isEOFError err -> pure KeySubmit
      | otherwise -> ioError err
    Right char
      | char == escape -> readEscape
      | otherwise -> pure (classifyChar char)

readEscape :: IO Key
readEscape = do
  next <- readChar
  case next of
    Right '[' -> readCsi ""
    Right 'O' -> readSs3
    _ -> pure KeyIgnore

readCsi :: String -> IO Key
readCsi parameters = do
  next <- readChar
  case next of
    Right char
      | char >= '@' && char <= '~' -> pure (csiKey parameters char)
      | otherwise -> readCsi (parameters ++ [char])
    Left _ -> pure KeyIgnore

readSs3 :: IO Key
readSs3 =
  either (const KeyIgnore) (csiKey "") <$> readChar

readChar :: IO (Either IOException Char)
readChar = try getChar

renderPrompt :: Bool -> PromptContext -> Int -> PromptState -> IO (Int, Int)
renderPrompt interactive context previousRow state =
  renderFrame
    interactive
    (contextTermPrompt context)
    (promptMode state)
    previousRow
    (activePrompt (promptMode state) (promptBuffers state))

renderFrame :: Bool -> String -> PromptMode -> Int -> Prompt -> IO (Int, Int)
renderFrame interactive termPrompt mode previousRow prompt
  | not interactive = pure (0, 0)
  | otherwise = do
      width <- consoleWidth
      let (output, position) = promptRenderText termPrompt mode previousRow width prompt
      consoleWrite (syncStart ++ output ++ syncEnd)
      consoleFlush
      pure position

finishPrompt :: Bool -> PromptContext -> PromptMode -> Int -> Prompt -> IO ()
finishPrompt interactive context mode previousRow prompt
  | not interactive = pure ()
  | otherwise = do
      let finished = cursorToEnd (clearSuggestions prompt)
      _ <- renderFrame True (contextTermPrompt context) mode previousRow finished
      consoleWrite reset
      unless (null (currentLine finished)) (consoleWrite "\n")
      consoleFlush

escape :: Char
escape = '\ESC'

-- | Begin a synchronized update, so terminals that support it draw the whole
-- prompt at once instead of flashing partially rendered frames.
syncStart :: String
syncStart = "\ESC[?2026h"

-- | End a synchronized update.
syncEnd :: String
syncEnd = "\ESC[?2026l"
