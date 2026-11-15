-- | Pure ANSI rendering for prompts, output labels, and terminal chrome.
--
-- Rendering is expressed as data and strings only. The single IO boundary that
-- writes these strings lives in "Hutian.Console".
module Hutian.Render
  ( PromptKind (..)
  , OutputKind (..)
  , promptRenderText
  , toolPromptText
  , toolLabelText
  , prefixFor
  , outputForeground
  , outputLabelText
  , grayOutputText
  , ToolApproval (..)
  , approvalForKey
  , approvalDecisionText
  , approvePromptText
  , moveCursorText
  , startPromptText
  , grayForeground
  , boldVibrantPink
  , boldTeal
  , dimPinkForeground
  , dimTealForeground
  , CursorStyle (..)
  , cursorStyleText
  , hideCursorText
  , showCursorText
  , resetCursorText
  , reset
  ) where

import Data.Char (toUpper)
import Numeric (showHex)
import qualified Hutian.Ansi as Ansi
import Hutian.Editor
  ( Prompt
  , cursorPosition
  , initialPrompt
  , physicalRows
  , prefixCharacterCounts
  , setPromptText
  , suggestionRowCount
  )
import Hutian.Markdown (renderMarkdownRows)
import Hutian.Highlight (highlightShellLine)
import System.Console.ANSI
  ( clearFromCursorToScreenEndCode
  , cursorDownCode
  , cursorForwardCode
  , cursorUpCode
  , hideCursorCode
  , showCursorCode
  )

-- | What is being rendered above an input or command. User LLM prompts are
-- Markdown; user term input and model commands are shell text. Model commands
-- carry their tool name so the label cannot drift from the call.
data PromptKind
  = LlmPrompt
  | ShellPrompt
  | ToolPrompt String
  deriving stock (Eq, Show)

-- | Which label and palette to use for captured output.
data OutputKind
  = TermOutput
  | ToolOutput
  deriving stock (Eq, Show)

-- | Render a prompt and return the text to write plus the cursor position.
promptRenderText :: PromptKind -> Int -> Int -> Prompt -> (String, (Int, Int))
promptRenderText kind previousRow width prompt =
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
    prefix = prefixFor kind
    plainRows = physicalRows width prefix prompt
    (cursorRow, cursorColumn) = cursorPosition width prefix prompt
    total = length plainRows
    suggestionCount = suggestionRowCount width prompt
    inputRowCount = total - suggestionCount
    markdownRows = kind == LlmPrompt
    rows
      | markdownRows = renderMarkdownRows (take inputRowCount plainRows) ++ drop inputRowCount plainRows
      | otherwise = plainRows
    prefixCounts = prefixCharacterCounts width prefix plainRows
    moveUp = cursorUpCode previousRow
    moveDown
      | cursorRow < total - 1 = cursorUpCode (total - 1 - cursorRow)
      | otherwise = ""
    moveRight = cursorForwardCode cursorColumn
    renderRow (index, row, prefixCount) =
      let (prefixPart, rest) = splitAt prefixCount row
          prefixOutput
            | prefixCount > 0 = prefixForegroundFor kind ++ prefixPart ++ reset
            | otherwise = ""
          restRendered
            | markdownRows = rest
            | index < inputRowCount = highlightShellLine rest
            | otherwise = rest
          styleReset
            | '\ESC' `elem` restRendered = reset
            | otherwise = ""
          newline
            | index < total - 1 = "\n"
            | otherwise = ""
       in prefixOutput ++ restRendered ++ styleReset ++ clearLine ++ newline

-- | Render a command requested by the model as a prompt line, followed by a
-- newline. Tool calls use a bright teal prompt so they stand out from
-- user-entered commands.
toolPromptText :: String -> Int -> String -> String
toolPromptText toolName width command =
  fst (promptRenderText (ToolPrompt toolName) 0 width (setPromptText command initialPrompt))
    ++ reset
    ++ "\n"

-- | A label for a tool call that has no command to show.
toolLabelText :: String -> String
toolLabelText toolName = boldTeal ++ "tool " ++ toolName ++ ":" ++ reset

-- | The prefix shown above a prompt. Every prompt label occupies its own line.
prefixFor :: PromptKind -> String
prefixFor LlmPrompt = "ask:\n"
prefixFor ShellPrompt = "shell:\n"
prefixFor (ToolPrompt toolName) = "tool " ++ toolName ++ ":\n"

prefixForegroundFor :: PromptKind -> String
prefixForegroundFor (ToolPrompt _) = boldTeal
prefixForegroundFor _ = boldVibrantPink

-- | The user's decision about a model-requested command. Only an explicit
-- @y@ or @n@ decides; @Ctrl+C@ aborts the whole LLM turn.
data ToolApproval
  = ToolApproved
  | ToolDenied
  | ToolAborted
  deriving stock (Eq, Show)

-- | Map a raw terminal key to an approval decision. Unrecognised keys are
-- ignored so a stray key cannot approve a command.
approvalForKey :: Char -> Maybe ToolApproval
approvalForKey 'y' = Just ToolApproved
approvalForKey 'Y' = Just ToolApproved
approvalForKey 'n' = Just ToolDenied
approvalForKey 'N' = Just ToolDenied
approvalForKey '\ETX' = Just ToolAborted
approvalForKey _ = Nothing

-- | The approval prompt shown while waiting for a key. The label uses the
-- user's prompt colour; the choices stay in the terminal's default colour.
approvePromptText :: String
approvePromptText =
  boldVibrantPink
    ++ "approve:"
    ++ reset
    ++ "\n[y] Yes\n[n] No\n"

-- | Replace the two option lines with the chosen result. The cursor is assumed
-- to sit on the line after @[n] No@.
approvalDecisionText :: String -> String
approvalDecisionText decision =
  cursorUpCode 2 ++ "\r" ++ clearToEnd ++ decision ++ "\n"

-- | Render the sequence that moves from one cursor position to another.
moveCursorText :: (Int, Int) -> (Int, Int) -> String
moveCursorText (oldRow, _) (newRow, newColumn) =
  up ++ down ++ "\r" ++ right
  where
    up
      | newRow < oldRow = cursorUpCode (oldRow - newRow)
      | otherwise = ""
    down
      | newRow > oldRow = cursorDownCode (newRow - oldRow)
      | otherwise = ""
    right = cursorForwardCode newColumn

-- | Clear the line before the first prompt frame is drawn.
startPromptText :: String
startPromptText = clearLine

-- | Foreground for the label printed before command output. The colour is the
-- command's mode colour, dimmed like a thinking block.
outputForeground :: OutputKind -> String
outputForeground TermOutput = dimPinkForeground
outputForeground ToolOutput = dimTealForeground

-- | The label printed above captured output, coloured when interactive.
outputLabelText :: OutputKind -> Bool -> String
outputLabelText kind interactive
  | interactive = outputForeground kind ++ label ++ reset
  | otherwise = label
  where
    label
      | kind == ToolOutput = "tool output:"
      | otherwise = "output:"

-- | Wrap captured output in gray when it is going to a terminal.
grayOutputText :: Bool -> String -> String
grayOutputText interactive text
  | interactive && not (null text) = grayForeground ++ text ++ reset
  | otherwise = text

-- | How the terminal cursor should look for a given activity.
data CursorStyle
  = UserCursor
  | RespondingCursor
  deriving stock (Eq, Show)

-- | User input gets a pink steady cursor; a responding model gets a teal
-- steady cursor. The cursor is blinked by toggling 'hideCursorText' and
-- 'showCursorText', because streaming output resets the terminal's own blink
-- timer and would otherwise keep the cursor solid.
cursorStyleText :: CursorStyle -> String
cursorStyleText UserCursor = pinkCursorColor ++ steadyBlockCursor
cursorStyleText RespondingCursor = tealCursorColor ++ steadyBlockCursor

-- | Restore the terminal's default cursor colour and shape, and make sure the
-- cursor is visible.
resetCursorText :: String
resetCursorText = showCursorText ++ resetCursorColor ++ defaultCursorShape

boldVibrantPink,
  boldTeal,
  dimPinkForeground,
  dimTealForeground,
  grayForeground,
  pinkCursorColor,
  tealCursorColor,
  resetCursorColor,
  hideCursorText,
  showCursorText,
  steadyBlockCursor,
  defaultCursorShape,
  clearToEnd,
  clearLine,
  reset :: String

pinkColour, tealColour :: Ansi.Colour
pinkColour = Ansi.rgb 255 20 147
tealColour = Ansi.rgb 0 255 200

pinkStyle, tealStyle, dimPinkStyle, dimTealStyle, grayStyle :: Ansi.Style
pinkStyle = Ansi.bold <> Ansi.foreground pinkColour
tealStyle = Ansi.bold <> Ansi.foreground tealColour
dimPinkStyle = Ansi.darkenStyle 0.6 pinkStyle
dimTealStyle = Ansi.darkenStyle 0.6 tealStyle
grayStyle = Ansi.darkenStyle 0.6 $ Ansi.foreground (Ansi.rgb 255 255 255)

boldVibrantPink = Ansi.styleCode pinkStyle
boldTeal = Ansi.styleCode tealStyle
dimPinkForeground = Ansi.styleCode dimPinkStyle
dimTealForeground = Ansi.styleCode dimTealStyle
grayForeground = Ansi.styleCode grayStyle
pinkCursorColor = cursorColourCode pinkColour
tealCursorColor = cursorColourCode tealColour
resetCursorColor = "\ESC]112\a"
hideCursorText = hideCursorCode
showCursorText = showCursorCode
steadyBlockCursor = "\ESC[2 q"
defaultCursorShape = "\ESC[0 q"
clearToEnd = clearFromCursorToScreenEndCode
-- ansi-terminal's 'clearLineCode' clears the whole line; keep the prompt prefix
-- by clearing only from the cursor to the end.
clearLine = "\ESC[K"
reset = Ansi.styleCode Ansi.reset

-- | Encode a 24-bit RGB colour as an OSC 12 cursor-colour sequence.
cursorColourCode :: Ansi.Colour -> String
cursorColourCode colour =
  let (red, green, blue) = Ansi.colourChannels colour
   in "\ESC]12;#" ++ hex red ++ hex green ++ hex blue ++ "\a"
  where
    hex channel = map toUpper (pad (showHex channel ""))
    pad [digit] = '0' : [digit]
    pad digits = digits
