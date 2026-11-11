-- | Pure command-history navigation, input routing, and history-file encoding.
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
  , encodeHistory
  , decodeHistory
  , escapeHistory
  , unescapeHistory
  ) where

import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Hutian.Editor (Prompt, PromptMode (..), atMay, promptText, setPromptText)

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

-- | The history list that a prompt mode reads and writes.
historyTargetFor :: PromptMode -> HistoryTarget
historyTargetFor LlmMode = LlmHistory
historyTargetFor TermMode = TermHistory
historyTargetFor ToolMode = TermHistory

historyForMode :: PromptMode -> History -> [String]
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

inputDestination :: PromptMode -> String -> Maybe HistoryTarget
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

-- | Encode history entries into the on-disk line format.
encodeHistory :: [String] -> String
encodeHistory = concatMap (\entry -> escapeHistory entry ++ "\n")

-- | Decode the on-disk line format into history entries.
decodeHistory :: String -> [String]
decodeHistory = map unescapeHistory . lines

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

escapeHistory :: String -> String
escapeHistory = concatMap escapeChar
  where
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar char = [char]

unescapeHistory :: String -> String
unescapeHistory [] = []
unescapeHistory ('\\' : '\\' : rest) = '\\' : unescapeHistory rest
unescapeHistory ('\\' : 'n' : rest) = '\n' : unescapeHistory rest
unescapeHistory (char : rest) = char : unescapeHistory rest
