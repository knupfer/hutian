{-# LANGUAGE ScopedTypeVariables #-}

-- | Raw terminal input handling for the interactive prompt.
module Hutian.Terminal
  ( promptForInput
  , renderToolCommand
  , resetTerminal
  , withoutEcho
  ) where

import Control.Exception (IOException, catch, finally, try)
import Control.Monad (unless, when)
import Data.List (isPrefixOf)
import Hutian.Editor
  ( Prompt
  , PromptBuffers
  , PromptAction (..)
  , PromptMode (..)
  , activePrompt
  , applyCompletionChoice
  , applyKey
  , clearPrompt
  , clearSuggestions
  , commandCompletionTarget
  , completeCommand
  , completionTarget
  , currentLine
  , cursorPosition
  , cursorToEnd
  , promptAction
  , promptLines
  , promptSuggestions
  , promptText
  , storePrompt
  , toggleMode
  )
import Hutian.Completion (CompletionKind (..), chooseCompletion)
import Hutian.History
  ( History
  , HistoryState
  , historyDown
  , historyForMode
  , historyUp
  , initialHistoryState
  )
import Hutian.Key (Key (..), classifyChar, csiKey)
import Hutian.Paths (formatCompletion, splitPathWord)
import Hutian.Render
  ( CursorStyle (..)
  , cursorStyleText
  , moveCursorText
  , prefixFor
  , promptRenderText
  , resetCursorText
  , startPromptText
  , toolPromptText
  )
import System.Console.Terminal.Size (Window (Window), size)
import System.Directory (doesDirectoryExist, listDirectory)
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
  interactive <- hIsTerminalDevice stdout
  let context =
        PromptContext
          { contextInteractive = interactive
          , contextCommands = commands
          , contextTermPrompt = termPrompt
          , contextMode = mode
          , contextHistory = history
          }
  withRawTerminal $ do
    when interactive (startPromptLine mode)
    readPrompt context (ReadState initialHistoryState 0 buffers)

data PromptContext = PromptContext
  { contextInteractive :: Bool
  , contextCommands :: [String]
  , contextTermPrompt :: String
  , contextMode :: PromptMode
  , contextHistory :: History
  }

data ReadState = ReadState
  { readHistoryState :: HistoryState
  , readPreviousRow :: Int
  , readBuffers :: PromptBuffers
  }

resetTerminal :: IO ()
resetTerminal = do
  isTerminal <- hIsTerminalDevice stdout
  when isTerminal (putStr (resetCursorText ++ "\ESC[0m") >> hFlush stdout)

withRawTerminal :: IO a -> IO a
withRawTerminal action = do
  isTerminal <- hIsTerminalDevice stdin
  if not isTerminal
    then action
    else do
      originalAttributes <- getTerminalAttributes stdInput
      setTerminalAttributes stdInput (rawAttributes originalAttributes) Immediately
      action `finally` setTerminalAttributes stdInput originalAttributes Immediately

-- | Keep typed-ahead characters out of streamed output. Characters typed while
-- this action runs stay buffered and are rendered by the next prompt instead.
withoutEcho :: IO a -> IO a
withoutEcho action = do
  isTerminal <- hIsTerminalDevice stdin
  if not isTerminal
    then action
    else do
      originalAttributes <- getTerminalAttributes stdInput
      setTerminalAttributes stdInput (withoutMode originalAttributes EnableEcho) Immediately
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

readPrompt :: PromptContext -> ReadState -> IO (PromptMode, Maybe String, PromptBuffers)
readPrompt context state = do
  position <-
    renderPrompt
      interactive
      termPrompt
      mode
      (readPreviousRow state)
      (activePrompt mode (readBuffers state))
  loop position state
  where
    mode = contextMode context
    interactive = contextInteractive context
    termPrompt = contextTermPrompt context
    commands = contextCommands context
    entries = historyForMode mode (contextHistory context)

    loop position currentState = do
      let current = activePrompt mode (readBuffers currentState)
      key <- readKey
      case promptAction mode current key of
        PromptSubmit -> submit position currentState
        PromptExit -> exitPrompt position currentState
        PromptContinue ->
          redraw
            position
            (nextState initialHistoryState (storePrompt mode (applyKey KeyEnter (cursorToEnd current)) (readBuffers currentState)) currentState)
        PromptClear ->
          redraw
            position
            (nextState initialHistoryState (clearPrompt mode (readBuffers currentState)) currentState)
        PromptEdit key' -> editKey position currentState key'
        PromptHistoryUp -> navigate historyUp position currentState
        PromptHistoryDown -> navigate historyDown position currentState
        PromptSwitchMode ->
          readPrompt
            context { contextMode = toggleMode mode }
            (ReadState initialHistoryState (fst position) (storePrompt mode (clearSuggestions current) (readBuffers currentState)))
        PromptComplete -> do
          completed <- completeInput commands current
          redraw
            position
            (nextState initialHistoryState (storePrompt mode completed (readBuffers currentState)) currentState)

    nextState historyState buffers currentState =
      ReadState historyState (readPreviousRow currentState) buffers

    navigate step position currentState =
      case step entries (readHistoryState currentState) (activePrompt mode (readBuffers currentState)) of
        Nothing -> loop position currentState
        Just (historyState', updated) ->
          redraw
            position
            (nextState historyState' (storePrompt mode updated (readBuffers currentState)) currentState)

    editKey position currentState key = do
      let current = activePrompt mode (readBuffers currentState)
          updated = applyKey key current
          updatedBuffers = storePrompt mode updated (readBuffers currentState)
      if sameBuffer current updated
        then moveCursorOnly position (nextState initialHistoryState updatedBuffers currentState) updated
        else redraw position (nextState initialHistoryState updatedBuffers currentState)

    moveCursorOnly position currentState updated = do
      newPosition <- promptPosition termPrompt mode updated
      moveCursor position newPosition
      loop newPosition currentState

    submit position currentState = do
      let current = activePrompt mode (readBuffers currentState)
      finishPrompt interactive termPrompt mode (fst position) current
      pure
        ( mode
        , Just (promptText current)
        , clearPrompt mode (readBuffers currentState)
        )

    exitPrompt position currentState = do
      let current = activePrompt mode (readBuffers currentState)
      finishPrompt interactive termPrompt mode (fst position) current
      when interactive (putStrLn "" >> hFlush stdout)
      pure (mode, Nothing, readBuffers currentState)

    redraw position currentState = do
      newPosition <-
        renderPrompt
          interactive
          termPrompt
          mode
          (fst position)
          (activePrompt mode (readBuffers currentState))
      loop newPosition currentState

sameBuffer :: Prompt -> Prompt -> Bool
sameBuffer a b =
  promptLines a == promptLines b
    && promptSuggestions a == promptSuggestions b

promptPosition :: String -> PromptMode -> Prompt -> IO (Int, Int)
promptPosition termPrompt mode prompt = do
  width <- terminalWidth
  let prefix = prefixFor termPrompt mode
  pure (cursorPosition width prefix prompt)

moveCursor :: (Int, Int) -> (Int, Int) -> IO ()
moveCursor oldPosition newPosition = do
  putStr (moveCursorText oldPosition newPosition)
  hFlush stdout

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

renderPrompt :: Bool -> String -> PromptMode -> Int -> Prompt -> IO (Int, Int)
renderPrompt interactive termPrompt mode previousRow prompt
  | not interactive = pure (0, 0)
  | otherwise = do
      width <- terminalWidth
      let (output, position) = promptRenderText termPrompt mode previousRow width prompt
      putStr (syncStart ++ output ++ syncEnd)
      hFlush stdout
      pure position

finishPrompt :: Bool -> String -> PromptMode -> Int -> Prompt -> IO ()
finishPrompt interactive termPrompt mode previousRow prompt
  | not interactive = pure ()
  | otherwise = do
      let finished = cursorToEnd (clearSuggestions prompt)
      _ <- renderPrompt True termPrompt mode previousRow finished
      putStr "\ESC[0m"
      unless (null (currentLine finished)) (putStr "\n")
      hFlush stdout

terminalWidth :: IO Int
terminalWidth = do
  window <- size
  pure $
    case window of
      Just (Window _ width) | width > 0 -> width
      _ -> 80

startPromptLine :: PromptMode -> IO ()
startPromptLine mode = do
  putStr (startPromptText mode ++ cursorStyleText UserCursor)
  hFlush stdout

-- | Show a command requested by the model with the tool-call prefix.
renderToolCommand :: String -> String -> IO ()
renderToolCommand toolName command = do
  interactive <- hIsTerminalDevice stdout
  if interactive
    then do
      width <- terminalWidth
      putStr (toolPromptText toolName width command)
    else putStrLn command
  hFlush stdout

escape :: Char
escape = '\ESC'

-- | Begin a synchronized update, so terminals that support it draw the whole
-- prompt at once instead of flashing partially rendered frames.
syncStart :: String
syncStart = "\ESC[?2026h"

-- | End a synchronized update.
syncEnd :: String
syncEnd = "\ESC[?2026l"
