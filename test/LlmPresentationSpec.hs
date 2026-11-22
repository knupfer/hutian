{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the stream presentation layer: labels, colours, and incremental
-- Markdown flushing. The protocol reducer itself is covered in "LlmStreamSpec".
module LlmPresentationSpec (tests) where

import Data.List (isInfixOf)
import qualified Data.Text as Text
import Hutian.Llm.Presentation
import Hutian.Llm.Stream (StreamEvent (..))
import Hutian.Render
  ( TurnContext (..)
  , boldTeal
  , dimBlueForeground
  , dimTealForeground
  , grayForeground
  , reset
  )
import Hutian.Verbosity (Verbosity (..))
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
    , testCase "reasoning after a flushed answer gets its own label" $ do
        let output =
              presentAll
                [ reasoningEvent "first"
                , contentEvent "answer\n"
                , reasoningEvent "second"
                ]
            label = dimTealForeground ++ "think:" ++ reset
        Text.count (Text.pack label) (Text.pack output) @?= 2
    , testCase "finishing flushes a partial answer line" $ do
        let output = presentAll [contentEvent "partial"]
            finished = finishAll [contentEvent "partial"]
        assertBool "answer label is emitted before the partial line" ("answer:" `isInfixOf` output)
        assertBool "partial answer is not emitted before a newline" (not ("partial" `isInfixOf` output))
        assertBool "partial answer flushed" ("partial" `isInfixOf` finished)
    , testCase "empty content events emit nothing" $ do
        presentAll [contentEvent ""] @?= ""
    , testCase "a subagent uses dim blue reasoning and answer labels" $ do
        let output =
              presentAllIn
                SubagentContext
                [reasoningEvent "thinking\n", contentEvent "hello\n"]
        assertBool
          "blue think label"
          ((dimBlueForeground ++ "think:" ++ reset) `isInfixOf` output)
        assertBool
          "dim blue answer label"
          ((dimBlueForeground ++ "answer:" ++ reset) `isInfixOf` output)
        assertBool
          "no teal labels"
          (not ((dimTealForeground ++ "think:") `isInfixOf` output))
    , testCase "low shows the reasoning label without its content" $ do
        let output =
              presentAllWith VerbosityLow MainContext [reasoningEvent "hidden reasoning\n"]
        assertBool "think label" ("think:" `isInfixOf` output)
        assertBool "reasoning hidden" (not ("hidden reasoning" `isInfixOf` output))
    , testCase "low keeps main answers but hides subagent answers" $ do
        let mainOutput =
              presentAllWith
                VerbosityLow
                MainContext
                [contentEvent "visible answer\n"]
            subagentOutput =
              presentAllWith
                VerbosityLow
                SubagentContext
                [contentEvent "hidden answer\n"]
        assertBool "main answer shown" ("visible answer" `isInfixOf` mainOutput)
        assertBool
          "subagent answer label"
          ("answer:" `isInfixOf` subagentOutput)
        assertBool
          "subagent answer hidden"
          (not ("hidden answer" `isInfixOf` subagentOutput))
    , testCase "normal shows main reasoning and labels subagent reasoning" $ do
        let mainOutput =
              presentAllWith
                VerbosityNormal
                MainContext
                [reasoningEvent "visible reasoning\n", contentEvent "answer\n"]
            subagentOutput =
              presentAllWith
                VerbosityNormal
                SubagentContext
                [reasoningEvent "hidden reasoning\n"]
            subagentAnswerOutput =
              presentAllWith
                VerbosityNormal
                SubagentContext
                [contentEvent "hidden answer\n"]
        assertBool "main think label" ("think:" `isInfixOf` mainOutput)
        assertBool "main reasoning shown" ("visible reasoning" `isInfixOf` mainOutput)
        assertBool "answer shown" ("answer" `isInfixOf` mainOutput)
        assertBool "subagent think label" ("think:" `isInfixOf` subagentOutput)
        assertBool
          "subagent reasoning hidden"
          (not ("hidden reasoning" `isInfixOf` subagentOutput))
        assertBool
          "subagent answer label"
          ("answer:" `isInfixOf` subagentAnswerOutput)
        assertBool
          "subagent answer streams"
          ("hidden answer" `isInfixOf` subagentAnswerOutput)
        assertBool
          "subagent answer is dim"
          (grayForeground `isInfixOf` subagentAnswerOutput)
        Text.count (Text.pack "answer:") (Text.pack subagentAnswerOutput) @?= 1
    , testCase "high subagent answers use the dim style" $ do
        let output =
              presentAllWith
                VerbosityHigh
                SubagentContext
                [contentEvent "dim answer\n"]
        assertBool
          "dim blue answer label"
          ((dimBlueForeground ++ "answer:" ++ reset) `isInfixOf` output)
        assertBool "dim body" (grayForeground `isInfixOf` output)
        assertBool "answer text" ("dim answer" `isInfixOf` output)
    ]

presentAll :: [StreamEvent] -> String
presentAll = presentAllWith VerbosityHigh MainContext

presentAllIn :: TurnContext -> [StreamEvent] -> String
presentAllIn = presentAllWith VerbosityHigh

presentAllWith :: Verbosity -> TurnContext -> [StreamEvent] -> String
presentAllWith verbosity context =
  snd . foldl presentStep (initialStreamPresenterIn verbosity context, "")

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
