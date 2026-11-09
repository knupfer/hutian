{-# LANGUAGE ScopedTypeVariables #-}

-- | Raw terminal input handling for the interactive prompt.
module Hutian.Terminal
  ( PromptMode (..)
  , PromptBuffers
  , initialPromptBuffers
  , promptForInput
  , resetTerminal
  ) where

import Control.Exception (IOException, catch, finally, try)
import Control.Monad (forM_, unless, when)
import Data.List (isPrefixOf)
import Hutian.Editor
  ( Key (..)
  , Prompt
  , PromptBuffers
  , PromptMode (..)
  , Row (..)
  , SubmitIntent (..)
  , activePrompt
  , applyCompletion
  , applyKey
  , classifyChar
  , clearPrompt
  , clearSuggestions
  , commandCompletionTarget
  , completeCommand
  , completionTarget
  , csiKey
  , currentLine
  , cursorPosition
  , cursorToEnd
  , initialPromptBuffers
  , longestCommonPrefix
  , physicalRows
  , promptLines
  , promptRow
  , promptSuggestions
  , promptText
  , setSuggestions
  , storePrompt
  , submitIntent
  , suggestionRowCount
  , toggleMode
  )
import Hutian.History
  ( HistoryState
  , historyDown
  , historyForMode
  , historyUp
  , initialHistoryState
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
-- 'Nothing' when the user cancels with @Ctrl+C@ and 'Just' the message when
-- the user submits it with @Ctrl+D@. @Ctrl+X@ toggles between 'LlmMode' and
-- 'TermMode'. In term mode, Enter executes the command, Up/Down browse
-- history, and a trailing backslash continues the command on the next line.
-- Ctrl+C clears the current input.
promptForInput :: [String] -> String -> PromptMode -> [String] -> [String] -> PromptBuffers -> IO (PromptMode, Maybe String, PromptBuffers)
promptForInput commands termPrompt mode termHistory llmHistory buffers = do
  interactive <- hIsTerminalDevice stdout
  withRawTerminal $ do
    when interactive (startPromptLine mode)
    readPrompt
      interactive
      commands
      termPrompt
      mode
      termHistory
      llmHistory
      initialHistoryState
      0
      buffers

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

readPrompt :: Bool -> [String] -> String -> PromptMode -> [String] -> [String] -> HistoryState -> Int -> PromptBuffers -> IO (PromptMode, Maybe String, PromptBuffers)
readPrompt interactive commands termPrompt mode termHistory llmHistory historyState previousRow buffers = do
  position <- renderPrompt interactive termPrompt mode previousRow (activePrompt mode buffers)
  loop position historyState buffers
  where
    loop position state currentBuffers = do
      let current = activePrompt mode currentBuffers
      key <- readKey
      case submitIntent mode key current of
        SubmitNow -> submit position currentBuffers
        ContinueLine ->
          redraw
            position
            initialHistoryState
            (storePrompt mode (applyKey KeyEnter (cursorToEnd current)) currentBuffers)
        NotSubmit -> case key of
          KeyCancel ->
            redraw position initialHistoryState (clearPrompt mode currentBuffers)
          KeyUp
            | mode == LlmMode && unRow (promptRow current) > 0 ->
                editKey position currentBuffers KeyUp
            | otherwise -> navigate historyUp position state currentBuffers
          KeyDown
            | mode == LlmMode && unRow (promptRow current) < length (promptLines current) - 1 ->
                editKey position currentBuffers KeyDown
            | otherwise -> navigate historyDown position state currentBuffers
          KeyToggleMode ->
            readPrompt
              interactive
              commands
              termPrompt
              (toggleMode mode)
              termHistory
              llmHistory
              initialHistoryState
              (fst position)
              (storePrompt mode (clearSuggestions current) currentBuffers)
          KeyComplete
            | mode == TermMode -> do
                completed <- completeInput commands current
                redraw position initialHistoryState (storePrompt mode completed currentBuffers)
            | otherwise -> loop position state currentBuffers
          _ -> editKey position currentBuffers key

    entries = historyForMode mode termHistory llmHistory

    navigate step position state currentBuffers =
      case step entries state (activePrompt mode currentBuffers) of
        Nothing -> loop position state currentBuffers
        Just (state', updated) ->
          redraw position state' (storePrompt mode updated currentBuffers)

    editKey position currentBuffers key = do
      let current = activePrompt mode currentBuffers
          updated = applyKey key current
          updatedBuffers = storePrompt mode updated currentBuffers
      if sameBuffer current updated
        then moveCursorOnly position updatedBuffers updated
        else redraw position initialHistoryState updatedBuffers

    moveCursorOnly position currentBuffers updated = do
      newPosition <- promptPosition termPrompt mode updated
      moveCursor position newPosition
      loop newPosition initialHistoryState currentBuffers

    submit position currentBuffers = do
      let current = activePrompt mode currentBuffers
      finishPrompt interactive termPrompt mode (fst position) current
      pure
        ( mode
        , Just (promptText current)
        , clearPrompt mode currentBuffers
        )

    redraw position state currentBuffers = do
      newPosition <-
        renderPrompt
          interactive
          termPrompt
          mode
          (fst position)
          (activePrompt mode currentBuffers)
      loop newPosition state currentBuffers

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
moveCursor (oldRow, _) (newRow, newColumn) = do
  when (newRow < oldRow) $
    putStr ("\ESC[" ++ show (oldRow - newRow) ++ "A")
  when (newRow > oldRow) $
    putStr ("\ESC[" ++ show (newRow - oldRow) ++ "B")
  putStr "\r"
  when (newColumn > 0) $
    putStr ("\ESC[" ++ show newColumn ++ "C")
  hFlush stdout

completeInput :: [String] -> Prompt -> IO Prompt
completeInput commands prompt =
  case commandCompletionTarget prompt of
    Just (_, word)
      | '/' `notElem` word -> pure (completeCommand commands prompt)
    _ -> completePath prompt

completePath :: Prompt -> IO Prompt
completePath prompt =
  case completionTarget prompt of
    Nothing -> pure prompt
    Just (start, word) -> do
      let (directoryPart, base) = splitPathWord word
          directory = if null directoryPart then "." else directoryPart
      entries <- listDirectory directory `catch` \(_ :: IOException) -> pure []
      completions <- mapM (completeEntry directoryPart) (filter (base `isPrefixOf`) entries)
      case completions of
        [] -> pure (clearSuggestions prompt)
        _ ->
          let common = longestCommonPrefix completions
              afterCursor = drop (start + length word) (currentLine prompt)
              replacement
                | [only] <- completions =
                    only ++ if null afterCursor && not (hasTrailingSlash only) then " " else ""
                | length common > length word = common
                | otherwise = word
           in pure $
                if replacement == word
                  then setSuggestions completions prompt
                  else clearSuggestions (applyCompletion start replacement prompt)

completeEntry :: FilePath -> FilePath -> IO FilePath
completeEntry directoryPart entry = do
  let candidate = directoryPart ++ entry
  isDirectory <- doesDirectoryExist candidate
  pure (if isDirectory then candidate ++ "/" else candidate)

splitPathWord :: String -> (String, String)
splitPathWord word =
  case break (== '/') (reverse word) of
    (baseReversed, []) -> ("", reverse baseReversed)
    (baseReversed, _ : directoryReversed) ->
      (reverse directoryReversed ++ "/", reverse baseReversed)

hasTrailingSlash :: String -> Bool
hasTrailingSlash value =
  case reverse value of
    '/' : _ -> True
    _ -> False

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
readSs3 = do
  next <- readChar
  pure $
    case next of
      Right 'A' -> KeyUp
      Right 'B' -> KeyDown
      Right 'C' -> KeyRight
      Right 'D' -> KeyLeft
      Right 'H' -> KeyHome
      Right 'F' -> KeyEnd
      _ -> KeyIgnore

readChar :: IO (Either IOException Char)
readChar = try getChar

renderPrompt :: Bool -> String -> PromptMode -> Int -> Prompt -> IO (Int, Int)
renderPrompt interactive termPrompt mode previousRow prompt
  | not interactive = pure (0, 0)
  | otherwise = do
      width <- terminalWidth
      let prefix = prefixFor termPrompt mode
          rows = physicalRows width prefix prompt
          (cursorRow, cursorColumn) = cursorPosition width prefix prompt
          total = length rows
          suggestionCount = suggestionRowCount width prompt
          inputRowCount = total - suggestionCount
          prefixCounts = prefixCharacterCounts (length prefix) rows
      -- Move relative to the cursor rather than relying on a saved absolute
      -- position. Relative rows stay correct even when the terminal scrolls.
      when (previousRow > 0) $
        putStr ("\ESC[" ++ show previousRow ++ "A")
      putStr "\r"
      putStr "\ESC[0m"
      putStr clearToEnd
      putStr (backgroundFor mode)
      forM_ (zip3 [0 :: Int ..] rows prefixCounts) $ \(index, row, prefixCount) -> do
        when (suggestionCount > 0 && index == inputRowCount) $
          putStr "\ESC[0m"
        let (prefixPart, rest) = splitAt prefixCount row
        when (prefixCount > 0) $ do
          putStr tealForeground
          putStr prefixPart
          putStr defaultForeground
        putStr rest
        -- Paint the rest of the row explicitly. Relying on ESC[K to fill
        -- with the current background is not portable across terminals.
        putStr (replicate (width - length row) ' ')
        when (index < total - 1) (putStr "\n")
      when (cursorRow < total - 1) $
        putStr ("\ESC[" ++ show (total - 1 - cursorRow) ++ "A")
      putStr "\r"
      when (cursorColumn > 0) $
        putStr ("\ESC[" ++ show cursorColumn ++ "C")
      hFlush stdout
      pure (cursorRow, cursorColumn)

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
  putStr (backgroundFor mode)
  putStr clearLine
  hFlush stdout

clearToEnd :: String
clearToEnd = "\ESC[0J"

clearLine :: String
clearLine = "\ESC[K"

prefixCharacterCounts :: Int -> [String] -> [Int]
prefixCharacterCounts = go
  where
    go _ [] = []
    go remaining (row : rest) =
      let used = min remaining (length row)
       in used : go (remaining - used) rest

prefixFor :: String -> PromptMode -> String
prefixFor termPrompt TermMode = termPrompt
prefixFor _ LlmMode = ""

backgroundFor :: PromptMode -> String
backgroundFor mode =
  case mode of
    LlmMode -> "\ESC[48;2;24;0;24m"
    TermMode -> "\ESC[48;2;0;0;40m"

tealForeground :: String
tealForeground = "\ESC[38;2;0;255;200m"

defaultForeground :: String
defaultForeground = "\ESC[39m"

escape :: Char
escape = '\ESC'