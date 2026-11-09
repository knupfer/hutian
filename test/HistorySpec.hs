module HistorySpec (tests) where

import Hutian.Editor
  ( Column (..)
  , Prompt
  , PromptMode (..)
  , Row (..)
  , initialPrompt
  , promptColumn
  , promptRow
  , promptText
  , setPromptText
  )
import Data.Maybe (fromMaybe)
import Hutian.History
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "history"
    [ unitTests
    , propertyTests
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "unit"
    [ testCase "history is empty by default" $ do
        historyUp [] initialHistoryState initialPrompt @?= Nothing
        historyDown [] initialHistoryState initialPrompt @?= Nothing
    , testCase "Up recalls newest and then older entries" $ do
        let history = ["first", "second"]
            start = setPromptText "draft" initialPrompt
            (state1, prompt1) = recallUp history initialHistoryState start
            (state2, prompt2) = recallUp history state1 prompt1
            (_, prompt3) = recallUp history state2 prompt2
        promptText prompt1 @?= "second\n"
        promptText prompt2 @?= "first\n"
        promptText prompt3 @?= "first\n"
    , testCase "Down moves forward and restores the original draft" $ do
        let history = ["first", "second"]
            start = setPromptText "draft" initialPrompt
            (state1, prompt1) = recallUp history initialHistoryState start
            (state2, prompt2) = recallUp history state1 prompt1
            (state3, prompt3) = recallDown history state2 prompt2
            (_, prompt4) = recallDown history state3 prompt3
        promptText prompt1 @?= "second\n"
        promptText prompt2 @?= "first\n"
        promptText prompt3 @?= "second\n"
        promptText prompt4 @?= "draft\n"
    , testCase "history selection is mode specific" $ do
        historyForMode LlmMode ["term"] ["llm"] @?= ["llm"]
        historyForMode TermMode ["term"] ["llm"] @?= ["term"]
    , testCase "multiline history entries are restored" $ do
        let start = initialPrompt
            (_, prompt) = recallUp ["line one\nline two"] initialHistoryState start
        promptText prompt @?= "line one\nline two\n"
        promptRow prompt @?= Row 1
        promptColumn prompt @?= Column 8
    , testCase "history encoding preserves backslashes and newlines" $ do
        escapeHistory "plain" @?= "plain"
        escapeHistory "a\\b" @?= "a\\\\b"
        escapeHistory "a\nb" @?= "a\\nb"
        unescapeHistory (escapeHistory "a\\n\nb") @?= "a\\n\nb"
    , testCase "history files round-trip through disk" $ do
        temporaryDirectory <- getTemporaryDirectory
        (path, handle) <- openTempFile temporaryDirectory "hutian-history"
        hClose handle
        appendHistoryFile path "first command"
        appendHistoryFile path "multi\nline"
        loaded <- loadHistoryFile path
        loaded @?= ["first command", "multi\nline"]
        removeFile path
    ]

propertyTests :: TestTree
propertyTests =
  testGroup
    "properties"
    [ testProperty "history encoding round-trips" prop_escapeRoundTrip
    , testProperty "encoded history has no newlines" prop_escapeNoNewlines
    , testProperty "recalling every entry and returning restores the draft" prop_historyRoundTrip
    ]

recallUp :: [String] -> HistoryState -> Prompt -> (HistoryState, Prompt)
recallUp history state prompt =
  fromMaybe (state, prompt) (historyUp history state prompt)

recallDown :: [String] -> HistoryState -> Prompt -> (HistoryState, Prompt)
recallDown history state prompt =
  fromMaybe (state, prompt) (historyDown history state prompt)

prop_escapeRoundTrip :: String -> Property
prop_escapeRoundTrip text =
  unescapeHistory (escapeHistory text) === text

prop_escapeNoNewlines :: String -> Property
prop_escapeNoNewlines text =
  property ('\n' `notElem` escapeHistory text)

prop_historyRoundTrip :: [String] -> String -> Property
prop_historyRoundTrip history draft =
  counterexample (show (history, draft) ++ " -> " ++ show final) $
    promptText final === promptText start
  where
    start = setPromptText draft initialPrompt
    (upState, upPrompt) =
      navigate
        (length history)
        (historyUp history)
        (initialHistoryState, start)
    (_, final) =
      navigate
        (length history)
        (historyDown history)
        (upState, upPrompt)

navigate
  :: Int
  -> (HistoryState -> Prompt -> Maybe (HistoryState, Prompt))
  -> (HistoryState, Prompt)
  -> (HistoryState, Prompt)
navigate count step = go count
  where
    go 0 result = result
    go remaining (state, prompt) =
      case step state prompt of
        Nothing -> (state, prompt)
        Just result -> go (remaining - 1) result