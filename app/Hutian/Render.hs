-- | Pure ANSI rendering for the interactive prompt.
module Hutian.Render
  ( promptRenderText
  , toolPromptText
  , moveCursorText
  , startPromptText
  , prefixFor
  , outputForeground
  , grayForeground
  , boldVibrantPink
  , boldTeal
  , reset
  ) where

import Hutian.Editor
  ( Prompt
  , PromptMode (..)
  , cursorPosition
  , initialPrompt
  , physicalRows
  , prefixCharacterCounts
  , setPromptText
  , suggestionRowCount
  )
import Hutian.Markdown (renderMarkdownRows)

-- | Render a prompt and return the text to write plus the cursor position.
promptRenderText :: String -> PromptMode -> Int -> Int -> Prompt -> (String, (Int, Int))
promptRenderText termPrompt mode previousRow width prompt =
  ( moveUp
      ++ "\r"
      ++ reset
      ++ concatMap renderRow (zip3 [0 :: Int ..] rows prefixCounts)
      ++ clearToEnd
      ++ moveDown
      ++ "\r"
      ++ moveRight
  , (cursorRow, cursorColumn)
  )
  where
    prefix = prefixFor termPrompt mode
    plainRows = physicalRows width prefix prompt
    (cursorRow, cursorColumn) = cursorPosition width prefix prompt
    total = length plainRows
    suggestionCount = suggestionRowCount width prompt
    inputRowCount = total - suggestionCount
    rows
      | mode == LlmMode =
          renderMarkdownRows (take inputRowCount plainRows)
            ++ drop inputRowCount plainRows
      | otherwise = plainRows
    prefixCounts = prefixCharacterCounts width prefix plainRows
    moveUp
      | previousRow > 0 = "\ESC[" ++ show previousRow ++ "A"
      | otherwise = ""
    moveDown
      | cursorRow < total - 1 = "\ESC[" ++ show (total - 1 - cursorRow) ++ "A"
      | otherwise = ""
    moveRight
      | cursorColumn > 0 = "\ESC[" ++ show cursorColumn ++ "C"
      | otherwise = ""
    renderRow (index, row, prefixCount) =
      let (prefixPart, rest) = splitAt prefixCount row
          prefixOutput
            | prefixCount > 0 = prefixForegroundFor mode ++ prefixPart ++ reset
            | otherwise = ""
          styleReset
            | mode == LlmMode && '\ESC' `elem` rest = reset
            | otherwise = ""
          newline
            | index < total - 1 = "\n"
            | otherwise = ""
       in prefixOutput ++ rest ++ styleReset ++ clearLine ++ newline

-- | Render a command requested by the model as a prompt line, followed by a
-- newline. Tool calls use a bright red prompt so they stand out from
-- user-entered commands.
toolPromptText :: String -> Int -> String -> String
toolPromptText toolName width command =
  fst (promptRenderText (toolPrefix toolName) ToolMode 0 width (setPromptText command initialPrompt))
    ++ reset
    ++ "\n"

-- | The prefix shown before a tool call. Like every prompt it occupies its own
-- line above the command.
toolPrefix :: String -> String
toolPrefix toolName = "tool " ++ toolName ++ ":\n"

-- | Render the sequence that moves from one cursor position to another.
moveCursorText :: (Int, Int) -> (Int, Int) -> String
moveCursorText (oldRow, _) (newRow, newColumn) =
  up ++ down ++ "\r" ++ right
  where
    up
      | newRow < oldRow = "\ESC[" ++ show (oldRow - newRow) ++ "A"
      | otherwise = ""
    down
      | newRow > oldRow = "\ESC[" ++ show (newRow - oldRow) ++ "B"
      | otherwise = ""
    right
      | newColumn > 0 = "\ESC[" ++ show newColumn ++ "C"
      | otherwise = ""

-- | Clear the line before the first prompt frame is drawn.
startPromptText :: PromptMode -> String
startPromptText _ = clearLine

prefixFor :: String -> PromptMode -> String
prefixFor termPrompt TermMode = termPrompt
prefixFor termPrompt ToolMode = termPrompt
prefixFor _ LlmMode = "ask:\n"

prefixForegroundFor :: PromptMode -> String
prefixForegroundFor TermMode = boldVibrantPink
prefixForegroundFor ToolMode = boldTeal
prefixForegroundFor LlmMode = boldVibrantPink

-- | Foreground for the 'output:' label printed before command output. It is a
-- light version of the colour used by the command's prompt.
outputForeground :: PromptMode -> String
outputForeground TermMode = lightPinkForeground
outputForeground ToolMode = lightTealForeground
outputForeground LlmMode = grayForeground

boldVibrantPink,
  boldTeal,
  lightPinkForeground,
  lightTealForeground,
  grayForeground,
  clearToEnd,
  clearLine,
  reset :: String

boldVibrantPink = "\ESC[1;38;2;255;20;147m"
boldTeal = "\ESC[1;38;2;0;255;200m"
lightPinkForeground = "\ESC[38;2;255;150;200m"
lightTealForeground = "\ESC[38;2;150;255;200m"
grayForeground = "\ESC[38;2;130;130;130m"
clearToEnd = "\ESC[0J"
clearLine = "\ESC[K"
reset = "\ESC[0m"
