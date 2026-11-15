-- | Pure parsing of term-mode input.
module Hutian.Command
  ( Command (..)
  , jobUsageText
  , helpText
  , parseCommandLine
  , TerminalInput (..)
  , classifyTerminalInput
  ) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Hutian.Jobs
  ( JobId
  , defaultJobOutputLines
  , parseJobId
  )
import Text.Read (readMaybe)

-- | A parsed line of user input in term mode. @help@, @jobs@, and @job@ are
-- builtins; every other line is passed to the shell unchanged.
data Command
  = CommandEmpty
  | CommandHelp
  | CommandJobs
  | CommandJobOutput JobId Int
  | CommandJobStart String
  | CommandJobKill JobId
  | CommandJobUsage
  | CommandShell String
  deriving stock (Eq, Show)

parseCommandLine :: String -> Command
parseCommandLine line =
  case words line of
    [] -> CommandEmpty
    ["help"] -> CommandHelp
    ["jobs"] -> CommandJobs
    ["job"] -> CommandJobUsage
    ("job" : "start" : _) -> jobStart (afterWords 2 line)
    ["job", "kill", identifier] ->
      maybe CommandJobUsage CommandJobKill (parseJobId identifier)
    ["job", identifier] ->
      case parseJobId identifier of
        Just target -> CommandJobOutput target defaultJobOutputLines
        Nothing -> CommandJobUsage
    ["job", identifier, lineCount] ->
      case (parseJobId identifier, parsePositive lineCount) of
        (Just target, Just count) -> CommandJobOutput target count
        _ -> CommandJobUsage
    _ -> CommandShell line
  where
    jobStart command =
      case dropWhileEnd isSpace command of
        [] -> CommandJobUsage
        trimmed -> CommandJobStart trimmed

-- | Drop a fixed number of leading words and the whitespace around them.
afterWords :: Int -> String -> String
afterWords = go
  where
    go 0 rest = dropWhile isSpace rest
    go remaining rest =
      case dropWhile isSpace rest of
        [] -> []
        wordStart -> go (remaining - 1) (dropWhile (not . isSpace) wordStart)

parsePositive :: String -> Maybe Int
parsePositive text = do
  value <- readMaybe text
  if value > 0 then Just value else Nothing

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

-- | The text shown for malformed @job@ commands.
jobUsageText :: [String]
jobUsageText =
  [ "usage:"
  , "  job <id> [lines]  show the end of a job's output"
  , "  job kill <id>     stop a background job"
  , "  job start <cmd>   run a command in the background"
  , "  jobs              list background jobs"
  ]

-- | The text shown by the term-mode @help@ command.
helpText :: [String]
helpText =
  [ "The interactive input has two modes:"
  , "  llm mode   send text to llama.cpp (pink ask: line)"
  , "  term mode  run a shell command (shell: line)"
  , "Ctrl+X toggles between the modes; each mode keeps its own input."
  , "The app starts in llm mode."
  , "In llm mode the model may call the shell tools. Commands are shown with"
  , "a bold teal 'tool shell:' line. When model shell access is set to 'ask',"
  , "each command needs y or n at the 'approve:' prompt. Background commands"
  , "also warn that they keep running until stopped."
  , "Other keys are ignored; Ctrl+C aborts the current LLM turn."
  , "Shell output is preceded by an 'output:' line; tool output by a"
  , "'tool output:' line. Output is shown in gray."
  , "Background jobs are shown in bold teal when they start, finish, or stop."
  , "The 'jobs' command lists them, and 'job <id>' shows a job's log tail."
  , ""
  , "Term-mode commands:"
  , "  help              show this help"
  , "  jobs              list background jobs"
  , "  job <id> [lines]  show the end of a job's output"
  , "  job kill <id>     stop a background job"
  , "  job start <cmd>   run a command in the background"
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
