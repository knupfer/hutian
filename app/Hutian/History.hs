-- | Pure command-history navigation, input routing, and history-file encoding.
module Hutian.History
  ( HistoryState
  , initialHistoryState
  , historyUp
  , historyDown
  , historyForMode
  , InputDestination (..)
  , inputDestination
  , encodeHistory
  , decodeHistory
  , escapeHistory
  , unescapeHistory
  ) where

import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Hutian.Editor (Prompt, PromptMode (..), atMay, promptText, setPromptText)

data HistoryState = HistoryState
  { historyOffset :: Maybe Int
  , historyDraft :: String
  }
  deriving stock (Eq, Show)

initialHistoryState :: HistoryState
initialHistoryState = HistoryState Nothing ""

{-@ historyForMode :: m:PromptMode -> termHistory:[String] -> llmHistory:[String] ->
      {v:[String] | len v == len termHistory || len v == len llmHistory} @-}
historyForMode :: PromptMode -> [String] -> [String] -> [String]
historyForMode LlmMode _ llmHistory = llmHistory
historyForMode TermMode termHistory _ = termHistory
historyForMode ToolMode termHistory _ = termHistory

-- | Where submitted input should be remembered. Blank input is not stored.
data InputDestination
  = DiscardInput
  | TermHistoryEntry
  | LlmHistoryEntry
  deriving stock (Eq, Show)

inputDestination :: PromptMode -> String -> InputDestination
inputDestination _ text
  | all isSpace text = DiscardInput
inputDestination TermMode _ = TermHistoryEntry
inputDestination ToolMode _ = TermHistoryEntry
inputDestination LlmMode _ = LlmHistoryEntry

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
