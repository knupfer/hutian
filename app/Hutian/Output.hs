-- | Gray terminal output with a mode-specific background tint.
module Hutian.Output
  ( grayOutputText
  , withGrayOutput
  , termOutputBackground
  , toolOutputBackground
  ) where

import Control.Exception (finally)
import Control.Monad (when)
import Hutian.Render (grayForeground, reset)
import System.IO (hFlush, hIsTerminalDevice, hPutStr, stderr, stdout)

-- | Background for output produced by user commands in term mode. It is darker
-- than the term-mode input background.
termOutputBackground :: String
termOutputBackground = "\ESC[48;2;0;0;20m"

-- | Background for output produced by LLM tool calls. It is darker than the
-- tool-call input background.
toolOutputBackground :: String
toolOutputBackground = "\ESC[48;2;20;0;0m"

-- | Wrap captured output in gray on the given background when it is going to a
-- terminal.
grayOutputText :: String -> Bool -> String -> String
grayOutputText background interactive text
  | interactive && not (null text) = grayForeground ++ background ++ text ++ reset
  | otherwise = text

-- | Print streamed output in gray on the given background, restoring the
-- terminal afterwards.
withGrayOutput :: String -> IO a -> IO a
withGrayOutput background action = do
  stdoutIsTerminal <- hIsTerminalDevice stdout
  stderrIsTerminal <- hIsTerminalDevice stderr
  let styled = grayForeground ++ background
      restore = do
        when stdoutIsTerminal (putStr reset >> hFlush stdout)
        when stderrIsTerminal (hPutStr stderr reset >> hFlush stderr)
  when stdoutIsTerminal (putStr styled >> hFlush stdout)
  when stderrIsTerminal (hPutStr stderr styled >> hFlush stderr)
  action `finally` restore
