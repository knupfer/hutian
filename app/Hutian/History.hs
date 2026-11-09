{-# LANGUAGE ScopedTypeVariables #-}

-- | Pure command-history navigation and history-file encoding.
module Hutian.History
  ( HistoryState
  , initialHistoryState
  , historyUp
  , historyDown
  , historyForMode
  , escapeHistory
  , unescapeHistory
  , loadHistoryFile
  , appendHistoryFile
  ) where

import Control.Exception (IOException, catch)
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

historyUp :: [String] -> HistoryState -> Prompt -> Maybe (HistoryState, Prompt)
historyUp entries state prompt
  | null entries = Nothing
  | otherwise =
      let draft =
            case historyOffset state of
              Nothing -> promptText prompt
              Just _ -> historyDraft state
          offset =
            case historyOffset state of
              Nothing -> 0
              Just current -> min (current + 1) (length entries - 1)
          entry = fromMaybe "" (atMay entries (length entries - 1 - offset))
       in Just (HistoryState (Just offset) draft, setPromptText entry prompt)

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
              entry = fromMaybe "" (atMay entries (length entries - 1 - nextOffset))
           in Just
                ( HistoryState (Just nextOffset) draft
                , setPromptText entry prompt
                )
  where
    draft = historyDraft state

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

loadHistoryFile :: FilePath -> IO [String]
loadHistoryFile path = do
  contents <- readFile path `catch` \(_ :: IOException) -> pure ""
  pure (map unescapeHistory (lines contents))

appendHistoryFile :: FilePath -> String -> IO ()
appendHistoryFile path text =
  appendFile path (escapeHistory text ++ "\n")
    `catch` \(_ :: IOException) -> pure ()