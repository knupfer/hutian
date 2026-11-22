-- | Pure in-memory command-history navigation and input routing.
module Hutian.History
  ( History (..)
  , historyForMode
  , HistoryState
  , initialHistoryState
  , historyUp
  , historyDown
  , HistoryTarget (..)
  , inputDestination
  , recordHistory
  ) where

import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Hutian.Editor (InputMode (..), Prompt, atMay, promptText, setPromptText)

-- | Term and LLM history kept together, so callers cannot mix them up.
{-@ data History = History
      { termHistory :: [String]
      , llmHistory :: [String]
      } @-}
data History = History
  { termHistory :: [String]
  , llmHistory :: [String]
  }
  deriving stock (Eq, Show)

-- | Where submitted input should be remembered. Blank input is not stored.
data HistoryTarget
  = TermHistory
  | LlmHistory
  deriving stock (Eq, Show)

-- | The history list that an input mode reads and writes.
historyTargetFor :: InputMode -> HistoryTarget
historyTargetFor LlmInput = LlmHistory
historyTargetFor TermInput = TermHistory

historyForMode :: InputMode -> History -> [String]
historyForMode mode (History term llm) =
  case historyTargetFor mode of
    TermHistory -> term
    LlmHistory -> llm

data HistoryState = HistoryState
  { historyOffset :: Maybe Int
  , historyDraft :: String
  }
  deriving stock (Eq, Show)

initialHistoryState :: HistoryState
initialHistoryState = HistoryState Nothing ""

inputDestination :: InputMode -> String -> Maybe HistoryTarget
inputDestination mode text
  | all isSpace text = Nothing
  | otherwise = Just (historyTargetFor mode)

{-@ recordHistory :: target:HistoryTarget -> entry:String -> history:History ->
      {v:History | len (termHistory v) + len (llmHistory v) == len (termHistory history) + len (llmHistory history) + 1} @-}
recordHistory :: HistoryTarget -> String -> History -> History
recordHistory TermHistory entry history =
  history { termHistory = termHistory history ++ [entry] }
recordHistory LlmHistory entry history =
  history { llmHistory = llmHistory history ++ [entry] }

historyUp :: [String] -> HistoryState -> Prompt -> Maybe (HistoryState, Prompt)
historyUp entries state prompt
  | null entries = Nothing
  | otherwise =
      Just
        ( HistoryState (Just offset) draft
        , setPromptText (entryAt entries offset) prompt
        )
  where
    offset =
      maybe 0 (\current -> min (current + 1) (length entries - 1)) (historyOffset state)
    draft =
      maybe (promptText prompt) (const (historyDraft state)) (historyOffset state)

historyDown :: [String] -> HistoryState -> Prompt -> Maybe (HistoryState, Prompt)
historyDown entries state prompt =
  case historyOffset state of
    Nothing -> Nothing
    Just offset
      | offset <= 0 ->
          Just
            ( HistoryState Nothing draft
            , setPromptText draft prompt
            )
      | otherwise ->
          let nextOffset = offset - 1
           in Just
                ( HistoryState (Just nextOffset) draft
                , setPromptText (entryAt entries nextOffset) prompt
                )
  where
    draft = historyDraft state

entryAt :: [String] -> Int -> String
entryAt entries offset =
  fromMaybe "" (atMay entries (length entries - 1 - offset))
