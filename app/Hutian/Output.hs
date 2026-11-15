-- | Pure label and colour rendering for command output.
module Hutian.Output
  ( grayOutputText
  , outputLabelText
  ) where

import Hutian.Editor (PromptMode (..))
import Hutian.Render
  ( grayForeground
  , outputForeground
  , reset
  )

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
