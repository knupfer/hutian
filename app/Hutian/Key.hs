-- | Pure terminal key classification.
module Hutian.Key
  ( Key (..)
  , classifyChar
  , csiKey
  ) where

import Data.Char (isControl)

-- | A key press after terminal input has been decoded.
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

-- | Map a raw character to its key. Control characters that the editor does
-- not use are ignored instead of being inserted into the prompt.
classifyChar :: Char -> Key
classifyChar char
  | char == ctrlC = KeyCancel
  | char == ctrlD = KeySubmit
  | char == ctrlX = KeyToggleMode
  | char == newline || char == carriageReturn = KeyEnter
  | char == rubout || char == backspace = KeyBackspace
  | char == tab = KeyComplete
  | isControl char = KeyIgnore
  | otherwise = KeyChar char

-- | Map a control-sequence final byte and its parameters to a key.
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
