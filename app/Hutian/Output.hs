-- | Gray terminal output.
module Hutian.Output
  ( grayOutputText
  , withGrayOutput
  , outputLabelText
  , printOutputLabel
  ) where

import Control.Exception (finally)
import Control.Monad (when)
import Hutian.Editor (PromptMode (..))
import Hutian.Render (grayForeground, outputForeground, reset)
import System.IO (hFlush, hIsTerminalDevice, hPutStr, stderr, stdout)

-- | Wrap captured output in gray when it is going to a terminal.
grayOutputText :: Bool -> String -> String
grayOutputText interactive text
  | interactive && not (null text) = grayForeground ++ text ++ reset
  | otherwise = text

-- | The label printed above command output, coloured when interactive. The
-- colour is a light version of the role's prompt colour.
outputLabelText :: PromptMode -> Bool -> String
outputLabelText mode interactive
  | interactive = outputForeground mode ++ label ++ reset
  | otherwise = label
  where
    label = outputLabel mode

-- | Tool output is labelled separately from shell output.
outputLabel :: PromptMode -> String
outputLabel ToolMode = "tool output:"
outputLabel _ = "output:"

-- | Print the label shown on its own line before shell or tool output.
printOutputLabel :: PromptMode -> IO ()
printOutputLabel mode = do
  interactive <- hIsTerminalDevice stdout
  putStrLn (outputLabelText mode interactive)
  hFlush stdout

-- | Print streamed output in gray, restoring the terminal afterwards.
withGrayOutput :: IO a -> IO a
withGrayOutput action = do
  stdoutIsTerminal <- hIsTerminalDevice stdout
  stderrIsTerminal <- hIsTerminalDevice stderr
  let restore = do
        when stdoutIsTerminal (putStr reset >> hFlush stdout)
        when stderrIsTerminal (hPutStr stderr reset >> hFlush stderr)
  when stdoutIsTerminal (putStr grayForeground >> hFlush stdout)
  when stderrIsTerminal (hPutStr stderr grayForeground >> hFlush stderr)
  action `finally` restore
