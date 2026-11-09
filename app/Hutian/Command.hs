-- | Pure parsing of term-mode builtin commands.
module Hutian.Command
  ( Command (..)
  , parseCommandLine
  ) where

-- | A parsed line of user input in term mode. Anything that is not a builtin
-- is passed to the shell unchanged.
data Command
  = CommandEmpty
  | CommandExit
  | CommandHelp
  | CommandPrintDirectory
  | CommandChangeDirectory [String]
  | CommandLlm [String]
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
    "cd" : args -> CommandChangeDirectory args
    "llm" : args -> CommandLlm args
    _ -> CommandShell line
