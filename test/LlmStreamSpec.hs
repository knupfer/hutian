{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the pure stream protocol: parsing is covered in "LlmSpec";
-- this module drives the reducer and checks the result it publishes.
module LlmStreamSpec (tests) where

import qualified Data.Text as Text
import Hutian.Conversation (ToolCall (..))
import Hutian.Llm.Stream
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "llm-stream"
    [ testCase "reasoning does not become answer text" $ do
        let state = applyAll [reasoningEvent "thinking\n"]
            result = resultFromState state
        llmResponseText result @?= Just ""
        llmToolCalls result @?= []
    , testCase "content is accumulated as the answer" $ do
        let state = applyAll [reasoningEvent "thinking", contentEvent "hello\n"]
        resultFromState state @?= LlmResult (Just "hello\n") []
    , testCase "tool-call deltas are merged into complete calls" $ do
        let state =
              applyAll
                [ toolEvent (ToolCallDelta 0 (Just "call-1") (Just "shell") (Just "{\"command\":"))
                , toolEvent (ToolCallDelta 0 Nothing Nothing (Just "\"ls\"}"))
                ]
        resultFromState state
          @?= LlmResult Nothing [ToolCall "call-1" "shell" "{\"command\":\"ls\"}"]
    , testCase "sparse tool-call indices do not create placeholder calls" $ do
        let state =
              applyAll
                [ toolEvent (ToolCallDelta 2 (Just "call-3") (Just "shell") (Just "{\"command\":\"ls\"}"))
                , toolEvent (ToolCallDelta 0 (Just "call-1") (Just "jobs") (Just "{}"))
                ]
        resultFromState state
          @?= LlmResult
            Nothing
            [ ToolCall "call-1" "jobs" "{}"
            , ToolCall "call-3" "shell" "{\"command\":\"ls\"}"
            ]
    , testCase "incomplete tool calls are not published" $ do
        let state =
              applyAll
                [toolEvent (ToolCallDelta 0 (Just "call-1") Nothing (Just "{\"command\":"))]
        resultFromState state @?= LlmResult (Just "") []
    , testCase "an interrupted stream never publishes partial tool calls" $ do
        let state =
              applyAll
                [toolEvent (ToolCallDelta 0 (Just "call-1") (Just "shell") (Just "{\"command\":\"ls\"}"))]
        resultFromState state @?= LlmResult Nothing [ToolCall "call-1" "shell" "{\"command\":\"ls\"}"]
        resultUnlessInterrupted True state @?= emptyLlmResult
    , testCase "an error event is still stream data" $ do
        let state = applyAll [StreamEvent Nothing Nothing (Just "boom") [] False]
        assertBool "stream saw data" (streamSawData state)
        assertBool "stream not stopped" (not (streamDone state))
    , testCase "a stop event marks the stream done" $ do
        let state = applyAll [StreamEvent Nothing Nothing Nothing [] True]
        assertBool "stream done" (streamDone state)
    , testCase "empty content events do not add answer text" $ do
        let state = applyAll [contentEvent ""]
        streamAnswerText state @?= ""
        resultFromState state @?= LlmResult (Just "") []
    ]

applyAll :: [StreamEvent] -> StreamState
applyAll = foldl (flip applyStreamEvent) initialStreamState

reasoningEvent :: Text.Text -> StreamEvent
reasoningEvent text = StreamEvent (Just text) Nothing Nothing [] False

contentEvent :: Text.Text -> StreamEvent
contentEvent text = StreamEvent Nothing (Just text) Nothing [] False

toolEvent :: ToolCallDelta -> StreamEvent
toolEvent delta = StreamEvent Nothing Nothing Nothing [delta] False
