-- | Pure ANSI rendering for the interactive prompt.
module Hutian.Render
  ( promptRenderText
  , toolPromptText
  , moveCursorText
  , startPromptText
  , prefixFor
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

-- | Render a prompt and return the text to write plus the cursor position.
promptRenderText :: String -> PromptMode -> Int -> Int -> Prompt -> (String, (Int, Int))
promptRenderText termPrompt mode previousRow width prompt =
  ( moveUp
      ++ "\r"
      ++ reset
      ++ clearToEnd
      ++ backgroundFor mode
      ++ concatMap renderRow (zip3 [0 :: Int ..] rows prefixCounts)
      ++ moveDown
      ++ "\r"
      ++ moveRight
  , (cursorRow, cursorColumn)
  )
  where
    prefix = prefixFor termPrompt mode
    rows = physicalRows width prefix prompt
    (cursorRow, cursorColumn) = cursorPosition width prefix prompt
    total = length rows
    suggestionCount = suggestionRowCount width prompt
    inputRowCount = total - suggestionCount
    prefixCounts = prefixCharacterCounts (length prefix) rows
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
          suggestionReset
            | suggestionCount > 0 && index == inputRowCount = reset
            | otherwise = ""
          prefixOutput
            | prefixCount > 0 =
                pathForegroundFor mode ++ prefixPart ++ defaultForeground
            | otherwise = ""
          padding = replicate (max 0 (width - length row)) ' '
          newline
            | index < total - 1 = "\n"
            | otherwise = ""
       in suggestionReset ++ prefixOutput ++ rest ++ padding ++ newline

-- | Render a command requested by the model as a prompt line, followed by a
-- newline. Tool calls use a dark red background and a bright red path so they
-- are visually distinct from user-entered commands.
toolPromptText :: String -> Int -> String -> String
toolPromptText termPrompt width command =
  fst (promptRenderText termPrompt ToolMode 0 width (setPromptText command initialPrompt))
    ++ "\n"
    ++ reset

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

-- | Render the initial prompt line for a mode.
startPromptText :: PromptMode -> String
startPromptText mode = backgroundFor mode ++ clearLine

prefixFor :: String -> PromptMode -> String
prefixFor termPrompt TermMode = termPrompt
prefixFor termPrompt ToolMode = termPrompt
prefixFor _ LlmMode = ""

backgroundFor :: PromptMode -> String
backgroundFor mode =
  case mode of
    LlmMode -> "\ESC[48;2;24;0;24m"
    TermMode -> "\ESC[48;2;0;0;40m"
    ToolMode -> "\ESC[48;2;40;0;0m"

pathForegroundFor :: PromptMode -> String
pathForegroundFor TermMode = tealForeground
pathForegroundFor ToolMode = brightRedForeground
pathForegroundFor LlmMode = tealForeground

tealForeground, brightRedForeground, defaultForeground, clearToEnd, clearLine, reset :: String
tealForeground = "\ESC[38;2;0;255;200m"
brightRedForeground = "\ESC[38;2;255;0;0m"
defaultForeground = "\ESC[39m"
clearToEnd = "\ESC[0J"
clearLine = "\ESC[K"
reset = "\ESC[0m"
