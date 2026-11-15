-- | Pure label and colour rendering for command and job output.
module Hutian.Output
  ( grayOutputText
  , outputLabelText
  , jobEventText
  , jobsLabelText
  , jobOutputLabelText
  ) where

import Hutian.Editor (PromptMode (..))
import Hutian.Jobs (JobId, jobIdText)
import Hutian.Render
  ( dimTealForeground
  , grayForeground
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

-- | Job lifecycle events are routine and stay in the same dimmed teal as the
-- job labels, so they remain visible without competing with model answers.
jobEventText :: Bool -> String -> String
jobEventText interactive text
  | interactive && not (null text) = dimTealForeground ++ text ++ reset
  | otherwise = text

-- | The label above a list of jobs.
jobsLabelText :: Bool -> String
jobsLabelText interactive =
  labelText interactive (dimTealForeground ++ "jobs:" ++ reset) "jobs:"

-- | The label above one job's output.
jobOutputLabelText :: JobId -> Bool -> String
jobOutputLabelText target interactive =
  labelText
    interactive
    (dimTealForeground ++ "job " ++ jobIdText target ++ " output:" ++ reset)
    ("job " ++ jobIdText target ++ " output:")

labelText :: Bool -> String -> String -> String
labelText interactive styled plain = if interactive then styled else plain
