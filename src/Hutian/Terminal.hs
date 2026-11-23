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
  , consoleInputReady
  , consoleIsInteractive
  , consoleWidth
  , consoleWrite
  , consoleWriteLine
  , startPromptLine
  , withRawTerminal
  )
import Hutian.Editor
  ( InputMode (..)
  , Prompt
  , PromptBuffers
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
import Hutian.Render (PromptKind (..), moveCursorText, prefixFor, promptRenderText, reset)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import System.IO.Error (isEOFError)

data PromptContext = PromptContext
  { contextCommands :: [String]
  , contextHistory :: History
  }

-- | Read one prompt as raw terminal input.
--
-- Returns the mode that was active when the prompt finished, together with
-- 'Nothing' when the user presses @Ctrl+D@ on empty input and 'Just' the
-- submitted text otherwise. @Ctrl+X@ toggles between 'LlmInput' and
-- 'TermInput'. In term mode, Enter executes the command, Up/Down browse
-- history, and a trailing backslash continues the command on the next line.
-- Ctrl+C clears the current input. Term mode is always available, even when
-- the model's shell tool is disabled.
promptForInput
  :: FilePath
  -> [String]
  -> InputMode
  -> History
  -> PromptBuffers
  -> IO (InputMode, Maybe String, PromptBuffers)
promptForInput workingDirectory commands mode history buffers = do
  interactive <- consoleIsInteractive
  let context =
        PromptContext
          { contextCommands = commands
          , contextHistory = history
          }
  withRawTerminal $ do
    when interactive startPromptLine
    readPrompt workingDirectory interactive context (initialPromptState mode buffers)

readPrompt :: FilePath -> Bool -> PromptContext -> PromptState -> IO (InputMode, Maybe String, PromptBuffers)
readPrompt workingDirectory interactive context state = do
  position <- renderPrompt interactive 0 state
  loop position state
  where
    loop position currentState = do
      key <- readKey
      consume position position False currentState key

    -- Apply queued keys before drawing. A paste then costs one frame per
    -- available burst instead of one frame per character.
    consume displayedPosition logicalPosition redraw currentState key =
      case stepPrompt (contextHistory context) key currentState of
        StepContinue next ->
          readNext displayedPosition logicalPosition redraw next
        StepRedraw next ->
          readNext displayedPosition logicalPosition True next
        StepMoveCursor prompt next -> do
          nextPosition <- promptPosition (promptMode next) prompt
          readNext displayedPosition nextPosition redraw next
        StepComplete next -> do
          let current = activePrompt (promptMode next) (promptBuffers next)
          completed <- completeInput workingDirectory (contextCommands context) current
          readNext displayedPosition logicalPosition True (acceptCompletion completed next)
        StepFinish outcome prompt next -> do
          finishPrompt interactive (promptMode next) (fst displayedPosition) prompt
          case outcome of
            PromptSubmitted ->
              pure (promptMode next, Just (promptText prompt), promptBuffers next)
            PromptExited -> do
              when interactive (consoleWriteLine "" >> consoleFlush)
              pure (promptMode next, Nothing, promptBuffers next)

    readNext displayedPosition logicalPosition redraw currentState = do
      ready <- consoleInputReady
      if ready
        then readKey >>= consume displayedPosition logicalPosition redraw currentState
        else
          if redraw
            then do
              nextPosition <- renderPrompt interactive (fst displayedPosition) currentState
              loop nextPosition currentState
            else do
              when (logicalPosition /= displayedPosition) (writeCursorMove displayedPosition logicalPosition)
              loop logicalPosition currentState

promptPosition :: InputMode -> Prompt -> IO (Int, Int)
promptPosition mode prompt = do
  width <- consoleWidth
  pure (cursorPosition width (prefixFor (promptKindFor mode)) prompt)

promptKindFor :: InputMode -> PromptKind
promptKindFor LlmInput = LlmPrompt
promptKindFor TermInput = ShellPrompt

writeCursorMove :: (Int, Int) -> (Int, Int) -> IO ()
writeCursorMove oldPosition newPosition = do
  consoleWrite (moveCursorText oldPosition newPosition)
  consoleFlush

completeInput :: FilePath -> [String] -> Prompt -> IO Prompt
completeInput workingDirectory commands prompt =
  case commandCompletionTarget prompt of
    Just (_, word)
      | '/' `notElem` word -> pure (completeCommand commands prompt)
    _ -> completePath workingDirectory prompt

completePath :: FilePath -> Prompt -> IO Prompt
completePath workingDirectory prompt = do
  let (start, word) = completionTarget prompt
      (directoryPart, base) = splitPathWord word
      directory = if null directoryPart then workingDirectory else workingDirectory </> directoryPart
  entries <- listDirectory directory `catch` \(_ :: IOException) -> pure []
  completions <- mapM (completeEntry workingDirectory directoryPart) (filter (base `isPrefixOf`) entries)
  let afterCursor = drop (start + length word) (currentLine prompt)
      completion = chooseCompletion PathCompletion word afterCursor completions
  pure (applyCompletionChoice start word completion prompt)

completeEntry :: FilePath -> FilePath -> FilePath -> IO FilePath
completeEntry workingDirectory directoryPart entry = do
  let candidate = directoryPart ++ entry
      path = workingDirectory </> candidate
  isDirectory <- doesDirectoryExist path
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

renderPrompt :: Bool -> Int -> PromptState -> IO (Int, Int)
renderPrompt interactive previousRow state =
  renderFrame
    interactive
    (promptKindFor (promptMode state))
    previousRow
    (activePrompt (promptMode state) (promptBuffers state))

renderFrame :: Bool -> PromptKind -> Int -> Prompt -> IO (Int, Int)
renderFrame interactive kind previousRow prompt
  | not interactive = pure (0, 0)
  | otherwise = do
      width <- consoleWidth
      let (output, position) = promptRenderText kind previousRow width prompt
      consoleWrite (syncStart ++ output ++ syncEnd)
      consoleFlush
      pure position

finishPrompt :: Bool -> InputMode -> Int -> Prompt -> IO ()
finishPrompt interactive mode previousRow prompt
  | not interactive = pure ()
  | otherwise = do
      let finished = cursorToEnd (clearSuggestions prompt)
      _ <- renderFrame True (promptKindFor mode) previousRow finished
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
