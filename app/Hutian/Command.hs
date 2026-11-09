-- | Pure parsing of term-mode input.
module Hutian.Command
  ( Command (..)
  , parseCommandLine
  , TerminalInput (..)
  , classifyTerminalInput
  ) where

-- | A parsed line of user input in term mode. Anything that is not a builtin
-- is passed to the shell unchanged.
data Command
  = CommandEmpty
  | CommandExit
  | CommandHelp
  | CommandPrintDirectory
  | CommandChangeDirectory (Maybe String)
  | CommandLlm (Maybe String)
  | CommandShell String
  deriving stock (Eq, Show)

parseCommandLine :: String -> Command
parseCommandLine line =
  case words line of
    [] -> CommandEmpty
    "exit" : _ -> CommandExit
    "quit" : _ -> CommandExit
    "help" : _ -> CommandHelp
    "pwd" : _ -> CommandPrintDirectory
    "cd" : args -> CommandChangeDirectory (joinedArgs args)
    "llm" : args -> CommandLlm (joinedArgs args)
    _ -> CommandShell line

joinedArgs :: [String] -> Maybe String
joinedArgs [] = Nothing
joinedArgs args = Just (unwords args)

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
