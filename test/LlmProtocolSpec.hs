{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the pure LLM stream reducer and request protocol.
module LlmProtocolSpec (tests) where

import Data.List (isInfixOf)
import qualified Data.Text as Text
import Hutian.Conversation (ToolCall (..))
import Hutian.Llm.Protocol
import Hutian.Render (boldTeal, dimTealForeground, grayForeground, reset)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "llm-protocol"
    [ testCase "reasoning is labelled and uses the thinking colour" $ do
        let (state, output) = applyStreamEvent (reasoningEvent "thinking\n") initialStreamState
        assertBool "think label" ((dimTealForeground ++ "think:" ++ reset) `isInfixOf` outputText output)
        assertBool "thinking colour" (grayForeground `isInfixOf` outputText output)
        assertBool "stream saw data" (streamSawData state)
        resultFromState state @?= LlmResult (Just "") []
    , testCase "an answer after reasoning flushes thinking and adds an answer label" $ do
        let (reasoningState, _) = applyStreamEvent (reasoningEvent "thinking") initialStreamState
            (answerState, output) = applyStreamEvent (contentEvent "hello\n") reasoningState
            (finishedState, finishedOutput) = finishStream answerState
            combined = outputText output ++ outputText finishedOutput
        assertBool "thinking reset" (reset `isInfixOf` combined)
        assertBool "answer label" ((boldTeal ++ "answer:" ++ reset) `isInfixOf` outputText output)
        assertBool "answer text" ("hello" `isInfixOf` combined)
        resultFromState finishedState @?= LlmResult (Just "hello\n") []
    , testCase "tool-call deltas are merged into complete calls" $ do
        let first = toolEvent (ToolCallDelta 0 (Just "call-1") (Just "shell") (Just "{\"command\":"))
            second = toolEvent (ToolCallDelta 0 Nothing Nothing (Just "\"ls\"}"))
            (state, _) =
              applyStreamEvent second (fst (applyStreamEvent first initialStreamState))
        resultFromState state
          @?= LlmResult Nothing [ToolCall "call-1" "shell" "{\"command\":\"ls\"}"]
    , testCase "event errors are surfaced without stopping the stream" $ do
        let event = StreamEvent Nothing Nothing (Just "boom") [] False
            (state, output) = applyStreamEvent event initialStreamState
        outputError output @?= Just "boom"
        assertBool "stream still saw data" (streamSawData state)
        assertBool "stream not stopped" (not (streamDone state))
    , testCase "a stop event marks the stream done" $ do
        let (state, _) = applyStreamEvent (StreamEvent Nothing Nothing Nothing [] True) initialStreamState
        assertBool "stream done" (streamDone state)
    , testCase "finishStream flushes a partial answer line" $ do
        let (state, output) = applyStreamEvent (contentEvent "partial") initialStreamState
            (finishedState, finishedOutput) = finishStream state
        assertBool "answer label is emitted before the partial line" $
          ("answer:" :: String) `isInfixOf` outputText output
        assertBool "partial answer is not emitted before a newline" $
          not (("partial" :: String) `isInfixOf` outputText output)
        assertBool "partial answer flushed" ("partial" `isInfixOf` outputText finishedOutput)
        resultFromState finishedState @?= LlmResult (Just "partial") []
    , testCase "empty content events do not start the answer" $ do
        let (state, output) = applyStreamEvent (contentEvent "") initialStreamState
        outputText output @?= ""
        assertBool "no answer label" (not (("answer:" :: String) `isInfixOf` outputText output))
        resultFromState state @?= LlmResult (Just "") []
    ]

reasoningEvent :: Text.Text -> StreamEvent
reasoningEvent text = StreamEvent (Just text) Nothing Nothing [] False

contentEvent :: Text.Text -> StreamEvent
contentEvent text = StreamEvent Nothing (Just text) Nothing [] False

toolEvent :: ToolCallDelta -> StreamEvent
toolEvent delta = StreamEvent Nothing Nothing Nothing [delta] False
