-- | Pure classification of submitted term-mode input.
module Hutian.Command
  ( TerminalInput (..)
  , classifyTerminalInput
  ) where

import Data.Char (isSpace)

-- | The shape of submitted term-mode text.
data TerminalInput
  = NoTerminalInput
  | SingleLineInput String
  | MultiLineInput String
  deriving stock (Eq, Show)

-- | Classify submitted text. Blank input is ignored; a single line is
-- recorded without its trailing newline, while multi-line input is recorded
-- exactly as submitted.
classifyTerminalInput :: String -> TerminalInput
classifyTerminalInput text
  | all isSpace text = NoTerminalInput
  | otherwise =
      case lines text of
        [line] -> SingleLineInput line
        _ -> MultiLineInput text
