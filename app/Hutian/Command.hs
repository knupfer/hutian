-- | Pure parsing of term-mode input.
module Hutian.Command
  ( Command (..)
  , helpText
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

-- | The text shown by the term-mode @help@ command.
helpText :: [String]
helpText =
  [ "The interactive input has two modes:"
  , "  llm mode   send text to llama.cpp (pink ask: line)"
  , "  term mode  run a shell command (shell: line)"
  , "Ctrl+X toggles between the modes; each mode keeps its own input."
  , "The app starts in llm mode."
  , "In llm mode the model may call the shell tool. Tool calls are shown"
  , "with a bold teal 'tool shell:' line."
  , "Shell output is preceded by an 'output:' line; tool output by a"
  , "'tool output:' line. Output is shown in gray."
  , ""
  , "Term-mode commands:"
  , "  help       show this help"
  , "Every other line is executed with /bin/sh in the sandbox working"
  , "directory. Each command runs in a fresh shell, so cd, exported"
  , "variables, and other shell state do not persist between commands."
  , "Tab completes commands from PATH and filesystem paths; ambiguous"
  , "matches are listed below the input."
  , "Enter runs a command in term mode and adds a line in llm mode."
  , "Up/Down recall history; trailing \\ or open quotes continue a command."
  , "Ctrl+D on empty input exits; Enter runs term commands."
  , "Ctrl+C clears the current input."
  ]
