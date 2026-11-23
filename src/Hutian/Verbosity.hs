-- | Pure output-visibility decisions for model activity. The terminal and
-- presentation layers only apply these decisions.
module Hutian.Verbosity
  ( Verbosity (..)
  , verbosityArgument
  , verbosityFromArgument
  , Visibility (..)
  , AnswerStyle (..)
  , RenderMode (..)
  , renderMode
  , outputBodyVisible
  ) where

import Hutian.Context (TurnContext (..), isSubagentContext)

-- | How much model activity is shown.
data Verbosity
  = VerbosityLow
  | VerbosityNormal
  | VerbosityHigh
  deriving stock (Eq, Show)

verbosityArgument :: Verbosity -> String
verbosityArgument VerbosityLow = "low"
verbosityArgument VerbosityNormal = "normal"
verbosityArgument VerbosityHigh = "high"

verbosityFromArgument :: String -> Maybe Verbosity
verbosityFromArgument "low" = Just VerbosityLow
verbosityFromArgument "normal" = Just VerbosityNormal
verbosityFromArgument "high" = Just VerbosityHigh
verbosityFromArgument _ = Nothing

-- | Whether a block shows only its label or its full content.
data Visibility
  = LabelOnly
  | Full
  deriving stock (Eq, Show)

-- | Answer bodies are normal for the main agent and dim for subagents.
data AnswerStyle
  = AnswerNormal
  | AnswerDim
  deriving stock (Eq, Show)

-- | One turn's rendering policy. Tool calls and subagent headers are always
-- shown, so they do not need a field here.
data RenderMode = RenderMode
  { modeReasoning :: Visibility
  , modeAnswer :: Visibility
  , modeAnswerStyle :: AnswerStyle
  , modeToolOutput :: Visibility
  }
  deriving stock (Eq, Show)

renderMode :: Verbosity -> TurnContext -> RenderMode
renderMode verbosity context =
  RenderMode
    { modeReasoning = mainOutputVisibility
    , modeAnswer = answerVisibility
    , modeAnswerStyle =
        if isSubagentContext context then AnswerDim else AnswerNormal
    , modeToolOutput = mainOutputVisibility
    }
  where
    mainOutputVisibility =
      case verbosity of
        VerbosityHigh -> Full
        VerbosityNormal
          | context == MainContext -> Full
          | otherwise -> LabelOnly
        VerbosityLow -> LabelOnly
    answerVisibility =
      case verbosity of
        VerbosityLow
          | isSubagentContext context -> LabelOnly
        _ -> Full

-- | Tool output labels are always printed; only the body follows the mode.
outputBodyVisible :: Visibility -> Bool
outputBodyVisible Full = True
outputBodyVisible LabelOnly = False
