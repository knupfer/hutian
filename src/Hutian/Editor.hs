{-@ LIQUID "--no-termination" @-}

-- | Pure editor state, key semantics, and layout used by the interactive prompt.
module Hutian.Editor
  ( InputMode (..)
  , Row
  , unRow
  , Column
  , unColumn
  , GoalColumn
  , unGoalColumn
  , rowAt
  , columnAt
  , goalColumnAt
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

import Data.Char (isSpace)
import Data.List (intercalate, isPrefixOf)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import Hutian.Completion (Completion (..), CompletionKind (..), chooseCompletion)
import Hutian.Key (Key (..))

{-@ type Pos = {v:Int | v > 0} @-}

-- | Clamp a terminal width to a usable positive value.
{-@ layoutWidth :: width:Int -> {v:Pos | v >= width && (width > 0 => v == width)} @-}
layoutWidth :: Int -> Int
layoutWidth width
  | width <= 0 = 1
  | otherwise = width

-- | The two modes in which the user can edit and submit input. Commands
-- requested by the model are displayed separately and are never edited, so
-- they no longer need a fake input mode.
data InputMode
  = LlmInput
  | TermInput
  deriving stock (Eq, Show)

newtype Row = Row { unRow :: Int }
  deriving stock (Eq, Ord, Show)

newtype Column = Column { unColumn :: Int }
  deriving stock (Eq, Ord, Show)

newtype GoalColumn = GoalColumn { unGoalColumn :: Int }
  deriving stock (Eq, Ord, Show)

-- | Build non-negative row and column values. Editor state never produces a
-- negative position, so external callers get the same guarantee.
rowAt :: Int -> Row
rowAt = Row . max 0

columnAt :: Int -> Column
columnAt = Column . max 0

goalColumnAt :: Int -> GoalColumn
goalColumnAt = GoalColumn . max 0

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

activePrompt :: InputMode -> PromptBuffers -> Prompt
activePrompt LlmInput = llmBuffer
activePrompt TermInput = termBuffer

storePrompt :: InputMode -> Prompt -> PromptBuffers -> PromptBuffers
storePrompt LlmInput prompt buffers = buffers { llmBuffer = prompt }
storePrompt TermInput prompt buffers = buffers { termBuffer = prompt }

clearPrompt :: InputMode -> PromptBuffers -> PromptBuffers
clearPrompt mode = storePrompt mode initialPrompt

-- | What the terminal loop should do in response to a key press.
data PromptAction
  = PromptSubmit
  | PromptExit
  | PromptContinue
  | PromptClear
  | PromptNoop
  | PromptEdit Key
  | PromptHistoryUp
  | PromptHistoryDown
  | PromptSwitchMode
  | PromptComplete
  deriving stock (Eq, Show)

promptAction :: InputMode -> Prompt -> Key -> PromptAction
promptAction LlmInput prompt KeySubmit
  | isEmptyPrompt prompt = PromptExit
  | otherwise = PromptSubmit
promptAction TermInput prompt KeySubmit
  | isEmptyPrompt prompt = PromptExit
  | otherwise = PromptEdit KeySubmit
promptAction TermInput prompt KeyEnter
  | all isSpace (promptText prompt) = PromptNoop
  | needsContinuation prompt = PromptContinue
  | otherwise = PromptSubmit
promptAction mode prompt key =
  case key of
    KeyCancel -> PromptClear
    KeyUp
      | mode == LlmInput && unRow (promptRow prompt) > 0 -> PromptEdit KeyUp
      | otherwise -> PromptHistoryUp
    KeyDown
      | mode == LlmInput && unRow (promptRow prompt) < length (promptLines prompt) - 1 ->
          PromptEdit KeyDown
      | otherwise -> PromptHistoryDown
    KeyToggleMode -> PromptSwitchMode
    KeyComplete
      | mode == TermInput -> PromptComplete
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

toggleMode :: InputMode -> InputMode
toggleMode LlmInput = TermInput
toggleMode TermInput = LlmInput

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
        { promptRow = rowAt row
        , promptColumn = columnAt column
        , promptGoal = Just (goalColumnAt goal)
        }

setColumn :: Int -> Prompt -> Prompt
setColumn column prompt =
  prompt
    { promptColumn = columnAt (min column (length (currentLine prompt)))
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

{-@ atMay :: values:[a] -> index:Int ->
      {v:Maybe a | (0 <= index && index < len values => isJust v)
                && (index < 0 || index >= len values => not (isJust v))} @-}
atMay :: [a] -> Int -> Maybe a
atMay values index
  | index < 0 = Nothing
  | otherwise =
      case drop index values of
        [] -> Nothing
        value : _ -> Just value

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

{-@ lineStarts :: width:Int -> xs:[String] -> {v:[Nat] | len v == len xs + 1} @-}
lineStarts :: Int -> [String] -> [Int]
lineStarts width = scanl (+) 0 . map (length . wrapLine (layoutWidth width))

{-@ wrapLine :: width:Int -> line:String ->
      {v:[String] | len v > 0
                  && len (head v) <= len line
                  && (width > 0 => len (head v) <= width)
                  && (len line > 0 => len (head v) > 0)
                  && (len line == 0 => len (head v) == 0)} @-}
wrapLine :: Int -> String -> [String]
wrapLine width line
  | null line = [""]
  | otherwise = wrapChunks (layoutWidth width) line

-- | The chunks of a non-empty line. Kept separate from 'wrapLine' so the
-- refinement about the first chunk has a non-empty list to talk about.
{-@ wrapChunks :: width:Int -> rest:String ->
      {v:[String] | (len rest == 0 => len v == 0)
                  && (len rest > 0 => len v > 0)
                  && (len rest > 0 => len (head v) <= len rest)
                  && (width > 0 && len rest > 0 => len (head v) <= width)
                  && (width > 0 && len rest > 0 => len (head v) > 0)} @-}
wrapChunks :: Int -> String -> [String]
wrapChunks _ [] = []
wrapChunks width' rest =
  let (chunk, remainder) = splitAt width' rest
   in chunk : wrapChunks width' remainder

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
          completion = chooseCompletion CommandCompletion word afterCursor matches
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
