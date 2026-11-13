-- | Skylighting-based syntax highlighting for shell commands typed or shown in
-- the interactive prompt.
module Hutian.Syntax (highlightShellLine) where

import Hutian.Highlight (codeStyle, highlightLines, sanitize)

-- | Highlight one shell command row. The visible text is preserved; only ANSI
-- styles are added. Control characters are neutralized so pasted escape
-- sequences cannot reach the terminal.
highlightShellLine :: String -> String
highlightShellLine "" = ""
highlightShellLine line =
  case highlightLines codeStyle "bash" [line] of
    Just [rendered] -> rendered
    _ -> sanitize line
