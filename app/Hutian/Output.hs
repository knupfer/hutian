-- | Gray terminal output for commands and tool calls.
module Hutian.Output
  ( grayOutputText
  , withGrayOutput
  ) where

import Control.Exception (finally)
import Control.Monad (when)
import Hutian.Render (grayForeground, reset)
import System.IO (hFlush, hIsTerminalDevice, stdout)

-- | Wrap captured output in gray when it is going to a terminal.
grayOutputText :: Bool -> String -> String
grayOutputText interactive text
  | interactive && not (null text) = grayForeground ++ text ++ reset
  | otherwise = text

-- | Print streamed output in gray, restoring the terminal afterwards.
withGrayOutput :: IO a -> IO a
withGrayOutput action = do
  interactive <- hIsTerminalDevice stdout
  when interactive (putStr grayForeground >> hFlush stdout)
  action `finally` when interactive (putStr reset >> hFlush stdout)
