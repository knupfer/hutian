{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the model/tool exchange loop, driven by a stubbed executor and
-- model so no network or terminal is involved.
module LlmLoopSpec (tests) where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Hutian.Conversation
  ( ConversationEntry (..)
  , ToolCall (..)
  , appendToolResult
  , emptyConversation
  )
import Hutian.Interrupt
  ( newInterruptFlag
  , setInterrupt
  )
import Hutian.Llm.Loop (LlmLoop (..), runToolLoop)
import Hutian.Llm.Stream (LlmResult (..))
import Hutian.Tool.Exec (abortedToolMessage)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "llm-loop"
    [ testCase "an answer ends the loop without running tools" $ do
        flag <- newInterruptFlag
        runs <- newIORef (0 :: Int)
        conversation <-
          runToolLoop (idleLoop runs) flag emptyConversation (LlmResult (Just "done") [])
        conversation @?= emptyConversation
        readIORef runs >>= (@?= 0)
    , testCase "tool results are fed into the continuation" $ do
        flag <- newInterruptFlag
        let call = ToolCall "call-1" "shell" "{}"
            base = [UserEntry "hi", AssistantToolCallEntry "" [call]]
            loop =
              LlmLoop
                { loopRunTools = \conversation calls -> do
                    calls @?= [call]
                    pure (False, appendToolResult "call-1" "ok" conversation)
                , loopContinue = \conversation -> do
                    conversation @?= base ++ [ToolResultEntry "call-1" "ok"]
                    pure (LlmResult (Just "done") [])
                }
        conversation <- runToolLoop loop flag base (LlmResult Nothing [call])
        conversation
          @?= base
            ++ [ ToolResultEntry "call-1" "ok"
               , AssistantEntry "done"
               ]
    , testCase "an interrupt records aborted results without running tools" $ do
        flag <- newInterruptFlag
        setInterrupt flag
        runs <- newIORef (0 :: Int)
        let call = ToolCall "call-1" "shell" "{\"command\":"
            base = [AssistantToolCallEntry "" [call]]
        conversation <- runToolLoop (idleLoop runs) flag base (LlmResult Nothing [call])
        conversation @?= base ++ [ToolResultEntry "call-1" abortedToolMessage]
        readIORef runs >>= (@?= 0)
    , testCase "an aborted tool run stops the turn" $ do
        flag <- newInterruptFlag
        let call = ToolCall "call-1" "shell" "{}"
            base = [AssistantToolCallEntry "" [call]]
            loop =
              LlmLoop
                { loopRunTools = \conversation _ ->
                    pure
                      ( True
                      , appendToolResult "call-1" abortedToolMessage conversation
                      )
                , loopContinue = \_ -> assertFailure "continuation must not run"
                }
        conversation <- runToolLoop loop flag base (LlmResult Nothing [call])
        conversation @?= base ++ [ToolResultEntry "call-1" abortedToolMessage]
    , testCase "the loop continues when the continuation requests more tools" $ do
        flag <- newInterruptFlag
        rounds <- newIORef (0 :: Int)
        let first = ToolCall "call-1" "shell" "{}"
            second = ToolCall "call-2" "shell" "{}"
            base = [AssistantToolCallEntry "" [first]]
            loop =
              LlmLoop
                { loopRunTools = \conversation calls ->
                    pure
                      ( False
                      , foldl
                          (\current call -> appendToolResult (toolCallId call) "ok" current)
                          conversation
                          calls
                      )
                , loopContinue = \_ -> do
                    roundNumber <- readIORef rounds
                    modifyIORef' rounds (+ 1)
                    if roundNumber == 0
                      then pure (LlmResult Nothing [second])
                      else pure (LlmResult (Just "done") [])
                }
        conversation <- runToolLoop loop flag base (LlmResult Nothing [first])
        conversation
          @?= base
            ++ [ ToolResultEntry "call-1" "ok"
               , AssistantToolCallEntry "" [second]
               , ToolResultEntry "call-2" "ok"
               , AssistantEntry "done"
               ]
    ]

idleLoop :: IORef Int -> LlmLoop
idleLoop runs =
  LlmLoop
    { loopRunTools = \conversation _ -> do
        modifyIORef' runs (+ 1)
        pure (False, conversation)
    , loopContinue = \_ -> assertFailure "continuation must not run"
    }
