-- | Pure parsing of term-mode input.
module Hutian.Command
  ( Command (..)
  , parseCommandLine
  , TerminalInput (..)
  , classifyTerminalInput
  ) where

-- | A parsed line of user input in term mode. @help@ is the only builtin;
-- every other line is passed to the shell unchanged.
data Command
  = CommandEmpty
  | CommandHelp
  | CommandShell String
  deriving stock (Eq, Show)

parseCommandLine :: String -> Command
parseCommandLine line =
  case words line of
    [] -> CommandEmpty
    ["help"] -> CommandHelp
    _ -> CommandShell line

-- | The shape of submitted term-mode text.
data TerminalInput
  = NoTerminalInput
  | SingleLineInput String
  | MultiLineInput String
  deriving stock (Eq, Show)

classifyTerminalInput :: String -> TerminalInput
classifyTerminalInput text =
  case lines text of
    [] -> NoTerminalInput
    [line] -> SingleLineInput line
    _ -> MultiLineInput text
