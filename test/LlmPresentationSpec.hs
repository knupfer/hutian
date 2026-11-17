{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the stream presentation layer: labels, colours, and incremental
-- Markdown flushing. The protocol reducer itself is covered in "LlmStreamSpec".
module LlmPresentationSpec (tests) where

import Data.List (isInfixOf)
import qualified Data.Text as Text
import Hutian.Llm.Presentation
import Hutian.Llm.Stream (StreamEvent (..))
import Hutian.Render (boldTeal, dimTealForeground, grayForeground, reset)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "llm-presentation"
    [ testCase "reasoning is labelled and uses the thinking colour" $ do
        let output = presentAll [reasoningEvent "thinking\n"]
        assertBool "think label" ((dimTealForeground ++ "think:" ++ reset) `isInfixOf` output)
        assertBool "thinking colour" (grayForeground `isInfixOf` output)
    , testCase "an answer after reasoning flushes thinking and adds an answer label" $ do
        let output = presentAll [reasoningEvent "thinking", contentEvent "hello\n"]
            finished = finishAll [reasoningEvent "thinking", contentEvent "hello\n"]
        assertBool "thinking reset" (reset `isInfixOf` output)
        assertBool "answer label" ((boldTeal ++ "answer:" ++ reset) `isInfixOf` output)
        assertBool "answer text" ("hello" `isInfixOf` (output ++ finished))
    , testCase "finishing flushes a partial answer line" $ do
        let output = presentAll [contentEvent "partial"]
            finished = finishAll [contentEvent "partial"]
        assertBool "answer label is emitted before the partial line" ("answer:" `isInfixOf` output)
        assertBool "partial answer is not emitted before a newline" (not ("partial" `isInfixOf` output))
        assertBool "partial answer flushed" ("partial" `isInfixOf` finished)
    , testCase "empty content events emit nothing" $ do
        presentAll [contentEvent ""] @?= ""
    ]

presentAll :: [StreamEvent] -> String
presentAll = snd . foldl presentStep (initialStreamPresenter, "")

finishAll :: [StreamEvent] -> String
finishAll events =
  let (presenter, _) = foldl presentStep (initialStreamPresenter, "") events
   in snd (finishPresentation presenter)

presentStep :: (StreamPresenter, String) -> StreamEvent -> (StreamPresenter, String)
presentStep (presenter, accumulated) event =
  let (presenter', output) = presentStreamEvent event presenter
   in (presenter', accumulated ++ output)

reasoningEvent :: Text.Text -> StreamEvent
reasoningEvent text = StreamEvent (Just text) Nothing Nothing [] False

contentEvent :: Text.Text -> StreamEvent
contentEvent text = StreamEvent Nothing (Just text) Nothing [] False
