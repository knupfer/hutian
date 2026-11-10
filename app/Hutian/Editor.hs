{-@ LIQUID "--no-termination" @-}

-- | Pure editor state, key semantics, and layout used by the interactive prompt.
module Hutian.Editor
  ( PromptMode (..)
  , Row (unRow)
  , Column (unColumn)
  , GoalColumn (unGoalColumn)
  , Prompt
  , promptLines
  , promptRow
  , promptColumn
  , promptGoal
  , promptSuggestions
  , initialPrompt
  , PromptBuffers
  , initialPromptBuffers
  , activePrompt
  , storePrompt
  , clearPrompt
  , Key (..)
  , PromptAction (..)
  , promptAction
  , applyKey
  , toggleMode
  , currentLine
  , promptText
  , cursorToEnd
  , wrapLine
  , cursorPosition
  , physicalRows
  , classifyChar
  , csiKey
  , completionTarget
  , commandCompletionTarget
  , applyCompletion
  , applyCompletionChoice
  , completeCommand
  , setSuggestions
  , clearSuggestions
  , setPromptText
  , suggestionRowCount
  , prefixCharacterCounts
  , needsContinuation
  , atMay
  ) where

import Data.Char (isControl, isSpace)
import Data.List (intercalate, isPrefixOf)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe, listToMaybe)
import Hutian.Completion (Completion (..), chooseCompletion)

{-@ type Pos = {v:Int | v > 0} @-}

-- | Clamp a terminal width to a usable positive value.
{-@ layoutWidth :: width:Int -> Pos @-}
layoutWidth :: Int -> Int
layoutWidth width
  | width <= 0 = 1
  | otherwise = width

-- | The input modes. 'ToolMode' is used only to display commands requested by
-- the model; user input is still edited in 'LlmMode' or 'TermMode'.
data PromptMode
  = LlmMode
  | TermMode
  | ToolMode
  deriving stock (Eq, Show)

newtype Row = Row { unRow :: Int }
  deriving stock (Eq, Ord, Show)

newtype Column = Column { unColumn :: Int }
  deriving stock (Eq, Ord, Show)

newtype GoalColumn = GoalColumn { unGoalColumn :: Int }
  deriving stock (Eq, Ord, Show)

data Prompt = Prompt
  { promptLines :: NonEmpty String
  , promptRow :: Row
  , promptColumn :: Column
  , promptGoal :: Maybe GoalColumn
  , promptSuggestions :: [String]
  }
  deriving stock (Eq, Show)

initialPrompt :: Prompt
initialPrompt =
  Prompt
    { promptLines = "" :| []
    , promptRow = Row 0
    , promptColumn = Column 0
    , promptGoal = Nothing
    , promptSuggestions = []
    }

data PromptBuffers = PromptBuffers
  { llmBuffer :: Prompt
  , termBuffer :: Prompt
  }

initialPromptBuffers :: PromptBuffers
initialPromptBuffers = PromptBuffers initialPrompt initialPrompt

activePrompt :: PromptMode -> PromptBuffers -> Prompt
activePrompt LlmMode = llmBuffer
activePrompt TermMode = termBuffer
activePrompt ToolMode = termBuffer

storePrompt :: PromptMode -> Prompt -> PromptBuffers -> PromptBuffers
storePrompt LlmMode prompt buffers = buffers { llmBuffer = prompt }
storePrompt TermMode prompt buffers = buffers { termBuffer = prompt }
storePrompt ToolMode prompt buffers = buffers { termBuffer = prompt }

clearPrompt :: PromptMode -> PromptBuffers -> PromptBuffers
clearPrompt mode = storePrompt mode initialPrompt

data Key
  = KeyChar Char
  | KeyEnter
  | KeyBackspace
  | KeyDelete
  | KeyLeft
  | KeyRight
  | KeyUp
  | KeyDown
  | KeyHome
  | KeyEnd
  | KeySubmit
  | KeyCancel
  | KeyToggleMode
  | KeyComplete
  | KeyIgnore
  deriving stock (Eq, Show)

-- | What the terminal loop should do in response to a key press.
data PromptAction
  = PromptSubmit
  | PromptExit
  | PromptContinue
  | PromptClear
  | PromptEdit Key
  | PromptHistoryUp
  | PromptHistoryDown
  | PromptSwitchMode
  | PromptComplete
  deriving stock (Eq, Show)

promptAction :: PromptMode -> Prompt -> Key -> PromptAction
promptAction LlmMode prompt KeySubmit
  | isEmptyPrompt prompt = PromptExit
  | otherwise = PromptSubmit
promptAction TermMode prompt KeySubmit
  | isEmptyPrompt prompt = PromptExit
  | otherwise = PromptEdit KeySubmit
promptAction TermMode prompt KeyEnter
  | needsContinuation prompt = PromptContinue
  | otherwise = PromptSubmit
promptAction ToolMode prompt KeyEnter
  | needsContinuation prompt = PromptContinue
  | otherwise = PromptSubmit
promptAction mode prompt key =
  case key of
    KeyCancel -> PromptClear
    KeyUp
      | mode == LlmMode && unRow (promptRow prompt) > 0 -> PromptEdit KeyUp
      | otherwise -> PromptHistoryUp
    KeyDown
      | mode == LlmMode && unRow (promptRow prompt) < length (promptLines prompt) - 1 ->
          PromptEdit KeyDown
      | otherwise -> PromptHistoryDown
    KeyToggleMode -> PromptSwitchMode
    KeyComplete
      | mode == TermMode -> PromptComplete
    _ -> PromptEdit key

applyKey :: Key -> Prompt -> Prompt
applyKey key prompt =
  clearIfNotCompletion $
    case key of
      KeyChar char -> insertChar char prompt
      KeyEnter -> splitLine prompt
      KeyBackspace -> deleteBackward prompt
      KeyDelete -> deleteForward prompt
      KeyLeft -> moveLeft prompt
      KeyRight -> moveRight prompt
      KeyUp -> moveUp prompt
      KeyDown -> moveDown prompt
      KeyHome -> setColumn 0 prompt
      KeyEnd -> setColumn (length (currentLine prompt)) prompt
      KeySubmit -> prompt
      KeyCancel -> prompt
      KeyToggleMode -> prompt
      KeyComplete -> prompt
      KeyIgnore -> prompt
  where
    clearIfNotCompletion updated
      | key == KeyComplete = updated
      | otherwise = clearSuggestions updated

toggleMode :: PromptMode -> PromptMode
toggleMode LlmMode = TermMode
toggleMode TermMode = LlmMode
toggleMode ToolMode = TermMode

insertChar :: Char -> Prompt -> Prompt
insertChar char prompt =
  prompt
    { promptLines = nonEmptyLines (replaceLine row (take column line ++ char : drop column line) allLines)
    , promptColumn = Column (column + 1)
    , promptGoal = Nothing
    }
  where
    row = unRow (promptRow prompt)
    column = unColumn (promptColumn prompt)
    line = currentLine prompt
    allLines = linesOf prompt

splitLine :: Prompt -> Prompt
splitLine prompt =
  prompt
    { promptLines = nonEmptyLines (splice row 1 [before, after] allLines)
    , promptRow = Row (row + 1)
    , promptColumn = Column 0
    , promptGoal = Nothing
    }
  where
    row = unRow (promptRow prompt)
    (before, after) = splitAt (unColumn (promptColumn prompt)) (currentLine prompt)
    allLines = linesOf prompt

deleteBackward :: Prompt -> Prompt
deleteBackward prompt
  | column > 0 =
      prompt
        { promptLines = nonEmptyLines (replaceLine row (take (column - 1) line ++ drop column line) allLines)
        , promptColumn = Column (column - 1)
        , promptGoal = Nothing
        }
  | row > 0 =
      let previous = lineAtRow prompt (row - 1)
       in prompt
            { promptLines = nonEmptyLines (splice (row - 1) 2 [previous ++ line] allLines)
            , promptRow = Row (row - 1)
            , promptColumn = Column (length previous)
            , promptGoal = Nothing
            }
  | otherwise = prompt
  where
    row = unRow (promptRow prompt)
    column = unColumn (promptColumn prompt)
    line = currentLine prompt
    allLines = linesOf prompt

deleteForward :: Prompt -> Prompt
deleteForward prompt
  | column < length line =
      prompt
        { promptLines = nonEmptyLines (replaceLine row (take column line ++ drop (column + 1) line) allLines)
        , promptGoal = Nothing
        }
  | row < length allLines - 1 =
      prompt
        { promptLines =
            nonEmptyLines (splice row 2 [line ++ lineAtRow prompt (row + 1)] allLines)
        , promptGoal = Nothing
        }
  | otherwise = prompt
  where
    row = unRow (promptRow prompt)
    column = unColumn (promptColumn prompt)
    line = currentLine prompt
    allLines = linesOf prompt

moveLeft :: Prompt -> Prompt
moveLeft prompt
  | currentColumn > 0 =
      prompt { promptColumn = Column (currentColumn - 1), promptGoal = Nothing }
  | currentRow > 0 =
      let row = currentRow - 1
          column = length (lineAtRow prompt row)
       in prompt { promptRow = Row row, promptColumn = Column column, promptGoal = Nothing }
  | otherwise = prompt
  where
    currentRow = unRow (promptRow prompt)
    currentColumn = unColumn (promptColumn prompt)

moveRight :: Prompt -> Prompt
moveRight prompt
  | currentColumn < length (currentLine prompt) =
      prompt { promptColumn = Column (currentColumn + 1), promptGoal = Nothing }
  | currentRow < NonEmpty.length (promptLines prompt) - 1 =
      prompt { promptRow = Row (currentRow + 1), promptColumn = Column 0, promptGoal = Nothing }
  | otherwise = prompt
  where
    currentRow = unRow (promptRow prompt)
    currentColumn = unColumn (promptColumn prompt)

moveUp :: Prompt -> Prompt
moveUp prompt
  | unRow (promptRow prompt) <= 0 = prompt
  | otherwise = moveToRow (unRow (promptRow prompt) - 1) prompt

moveDown :: Prompt -> Prompt
moveDown prompt
  | unRow (promptRow prompt) >= NonEmpty.length (promptLines prompt) - 1 = prompt
  | otherwise = moveToRow (unRow (promptRow prompt) + 1) prompt

moveToRow :: Int -> Prompt -> Prompt
moveToRow row prompt =
  let goal = maybe (unColumn (promptColumn prompt)) unGoalColumn (promptGoal prompt)
      column = min goal (length (lineAtRow prompt row))
   in prompt
        { promptRow = Row row
        , promptColumn = Column column
        , promptGoal = Just (GoalColumn goal)
        }

setColumn :: Int -> Prompt -> Prompt
setColumn column prompt =
  prompt
    { promptColumn = Column (min column (length (currentLine prompt)))
    , promptGoal = Nothing
    }

currentLine :: Prompt -> String
currentLine prompt = lineAtRow prompt (unRow (promptRow prompt))

promptText :: Prompt -> String
promptText = unlines . linesOf

isEmptyPrompt :: Prompt -> Bool
isEmptyPrompt = all null . linesOf

needsContinuation :: Prompt -> Bool
needsContinuation prompt =
  go NoQuote False (intercalate "\n" (linesOf prompt))
  where
    go quote escaped [] = quote /= NoQuote || escaped
    go quote escaped (char : rest) =
      case quote of
        NoQuote
          | escaped -> go NoQuote False rest
          | char == '\\' -> go NoQuote True rest
          | char == '\'' -> go SingleQuote False rest
          | char == '"' -> go DoubleQuote False rest
          | otherwise -> go NoQuote False rest
        SingleQuote
          | char == '\'' -> go NoQuote False rest
          | otherwise -> go SingleQuote False rest
        DoubleQuote
          | escaped -> go DoubleQuote False rest
          | char == '\\' -> go DoubleQuote True rest
          | char == '"' -> go NoQuote False rest
          | otherwise -> go DoubleQuote False rest

data Quote
  = NoQuote
  | SingleQuote
  | DoubleQuote
  deriving stock (Eq)

setPromptText :: String -> Prompt -> Prompt
setPromptText text prompt =
  prompt
    { promptLines = updatedLines
    , promptRow = Row row
    , promptColumn = Column (length (lineAtRow updatedPrompt row))
    , promptGoal = Nothing
    , promptSuggestions = []
    }
  where
    updatedLines = nonEmptyLines (lines text)
    updatedPrompt = prompt { promptLines = updatedLines }
    row = NonEmpty.length updatedLines - 1

cursorToEnd :: Prompt -> Prompt
cursorToEnd prompt =
  let row = NonEmpty.length (promptLines prompt) - 1
   in prompt
        { promptRow = Row row
        , promptColumn = Column (length (lineAtRow prompt row))
        , promptGoal = Nothing
        }

linesOf :: Prompt -> [String]
linesOf = NonEmpty.toList . promptLines

nonEmptyLines :: [String] -> NonEmpty String
nonEmptyLines [] = "" :| []
nonEmptyLines (first : rest) = first :| rest

lineAtRow :: Prompt -> Int -> String
lineAtRow prompt row =
  fromMaybe (NonEmpty.head (promptLines prompt)) (atMay (linesOf prompt) row)

atMay :: [a] -> Int -> Maybe a
atMay values index
  | index < 0 = Nothing
  | otherwise = listToMaybe (drop index values)

replaceLine :: Int -> String -> [String] -> [String]
replaceLine index value = splice index 1 [value]

splice :: Int -> Int -> [a] -> [a] -> [a]
splice start count replacement values =
  take start values ++ replacement ++ drop (start + count) values

{-@ physicalRows :: width:Int -> String -> Prompt -> [String] @-}
physicalRows :: Int -> String -> Prompt -> [String]
physicalRows width prefix prompt =
  let width' = layoutWidth width
      inputRows = concatMap (wrapLine width') (renderedLines prefix prompt)
      suggestionRows = concatMap (wrapLine width') (suggestionLines prompt)
      content = inputRows ++ suggestionRows
      (cursorRow, _) = cursorPosition width' prefix prompt
      needed = cursorRow + 1
   in content ++ replicate (max 0 (needed - length content)) ""

suggestionLines :: Prompt -> [String]
suggestionLines prompt =
  case promptSuggestions prompt of
    [] -> []
    matches -> ["  " ++ intercalate "  " matches]

{-@ suggestionRowCount :: width:Int -> Prompt -> Nat @-}
suggestionRowCount :: Int -> Prompt -> Int
suggestionRowCount width prompt =
  length (concatMap (wrapLine (layoutWidth width)) (suggestionLines prompt))

-- | Count how many characters at the start of each rendered row belong to the
-- prompt prefix. Prefix rows separated by newlines occupy their own rows, and
-- the part after the last newline is distributed across the first input row.
prefixCharacterCounts :: Int -> String -> [String] -> [Int]
prefixCharacterCounts width prefix rows =
  take (length rows) (prefixCounts ++ inlineCounts ++ repeat 0)
  where
    width' = layoutWidth width
    (prefixRows, inline) = splitPrefix prefix
    prefixCounts =
      zipWith min (concatMap (map length . wrapLine width') prefixRows) (map length rows)
    inlineCounts = distribute (length inline) (drop (length prefixCounts) rows)

    distribute _ [] = []
    distribute remaining (row : rest) =
      let used = min remaining (length row)
       in used : distribute (remaining - used) rest

{-@ cursorPosition :: width:Int -> String -> Prompt -> (Int, Int) @-}
cursorPosition :: Int -> String -> Prompt -> (Int, Int)
cursorPosition width prefix prompt =
  let width' = layoutWidth width
      (prefixRows, inline) = splitPrefix prefix
      starts = lineStarts width' (renderedLines prefix prompt)
      column = firstLineOffset inline + unColumn (promptColumn prompt)
      start = fromMaybe 0 (atMay starts (unRow (promptRow prompt) + length prefixRows))
   in (start + column `div` width', column `mod` width')
  where
    firstLineOffset inline
      | unRow (promptRow prompt) == 0 = length inline
      | otherwise = 0

-- | Split a prompt prefix into rows that sit above the input and the part that
-- attaches to the first input row.
splitPrefix :: String -> ([String], String)
splitPrefix prefix =
  case break (== '\n') prefix of
    (before, []) -> ([], before)
    (before, _ : rest) ->
      let (rows, inline) = splitPrefix rest
       in (before : rows, inline)

renderedLines :: String -> Prompt -> [String]
renderedLines prefix prompt =
  let first :| rest = promptLines prompt
      (prefixRows, inline) = splitPrefix prefix
   in prefixRows ++ ((inline ++ first) : rest)

{-@ lineStarts :: width:Int -> [String] -> [Nat] @-}
lineStarts :: Int -> [String] -> [Int]
lineStarts width = scanl (+) 0 . map (length . wrapLine (layoutWidth width))

{-@ wrapLine :: width:Int -> String -> [String] @-}
wrapLine :: Int -> String -> [String]
wrapLine width line
  | null line = [""]
  | otherwise = go line
  where
    width' = layoutWidth width
    go [] = []
    go rest =
      let (chunk, remainder) = splitAt width' rest
       in chunk : go remainder

completionTarget :: Prompt -> (Int, String)
completionTarget prompt =
  (unColumn (promptColumn prompt) - length word, word)
  where
    beforeCursor = take (unColumn (promptColumn prompt)) (currentLine prompt)
    word = reverse (takeWhile (not . isSpace) (reverse beforeCursor))

commandCompletionTarget :: Prompt -> Maybe (Int, String)
commandCompletionTarget prompt =
  let target@(start, _) = completionTarget prompt
   in if unRow (promptRow prompt) == 0 && all isSpace (take start (currentLine prompt))
        then Just target
        else Nothing

applyCompletion :: Int -> String -> Prompt -> Prompt
applyCompletion start replacement prompt =
  prompt
    { promptLines = nonEmptyLines (replaceLine row (before ++ replacement ++ after) allLines)
    , promptColumn = Column (start + length replacement)
    , promptGoal = Nothing
    }
  where
    row = unRow (promptRow prompt)
    column = unColumn (promptColumn prompt)
    line = currentLine prompt
    before = take start line
    after = drop column line
    allLines = linesOf prompt

completeCommand :: [String] -> Prompt -> Prompt
completeCommand commands prompt =
  case commandCompletionTarget prompt of
    Just (_, word)
      | null word -> setSuggestions (filter (word `isPrefixOf`) commands) prompt
    Just (start, word) ->
      let matches = filter (word `isPrefixOf`) commands
          afterCursor = drop (unColumn (promptColumn prompt)) (currentLine prompt)
          completion = chooseCompletion (const True) word afterCursor matches
       in applyCompletionChoice start word completion prompt
    Nothing -> clearSuggestions prompt

applyCompletionChoice :: Int -> String -> Completion -> Prompt -> Prompt
applyCompletionChoice start word completion prompt
  | completionReplacement completion == word =
      setSuggestions (completionMatches completion) prompt
  | otherwise =
      clearSuggestions (applyCompletion start (completionReplacement completion) prompt)

setSuggestions :: [String] -> Prompt -> Prompt
setSuggestions matches prompt =
  prompt { promptSuggestions = take maxSuggestions matches }

clearSuggestions :: Prompt -> Prompt
clearSuggestions prompt = prompt { promptSuggestions = [] }

maxSuggestions :: Int
maxSuggestions = 20

classifyChar :: Char -> Key
classifyChar char
  | char == ctrlC = KeyCancel
  | char == ctrlD = KeySubmit
  | char == ctrlX = KeyToggleMode
  | char == newline = KeyEnter
  | char == carriageReturn = KeyEnter
  | char == rubout = KeyBackspace
  | char == backspace = KeyBackspace
  | char == tab = KeyComplete
  | isControl char = KeyIgnore
  | otherwise = KeyChar char

csiKey :: String -> Char -> Key
csiKey parameters final =
  case final of
    'A' -> KeyUp
    'B' -> KeyDown
    'C' -> KeyRight
    'D' -> KeyLeft
    'H' -> KeyHome
    'F' -> KeyEnd
    '~' ->
      case takeWhile (/= ';') parameters of
        "1" -> KeyHome
        "3" -> KeyDelete
        "4" -> KeyEnd
        "7" -> KeyHome
        "8" -> KeyEnd
        _ -> KeyIgnore
    _ -> KeyIgnore

ctrlC, ctrlD, ctrlX, newline, carriageReturn, rubout, backspace, tab :: Char
ctrlC = '\x03'
ctrlD = '\x04'
ctrlX = '\x18'
newline = '\n'
carriageReturn = '\r'
rubout = '\DEL'
backspace = '\b'
tab = '\t'