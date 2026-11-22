{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the interactive session loop, driven by stub effects.
module SessionSpec (tests) where

import Control.Monad (when)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Hutian.Conversation
  ( ConversationEntry (..)
  , appendTerminal
  , emptyConversation
  )
import Hutian.Editor
  ( InputMode (..)
  , activePrompt
  , initialPrompt
  , initialPromptBuffers
  , promptText
  , setPromptText
  , storePrompt
  )
import Hutian.History
  ( History (..)
  , HistoryTarget (..)
  , recordHistory
  )
import Hutian.Session
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "session"
    [ testCase "no input ends the loop" $ do
        prompts <- newIORef (0 :: Int)
        runSession
          ( noEffects
              { sessionPrompt = \_ _ _ -> do
                  modifyIORef' prompts (+ 1)
                  pure (LlmInput, Nothing, initialPromptBuffers)
              }
          )
          initialSession
        readIORef prompts >>= (@?= 1)
    , testCase "submitted input is remembered, clears the interrupt, and runs" $ do
        trace <- newIORef ([] :: [String])
        prompts <- newIORef (0 :: Int)
        runSession
          ( noEffects
              { sessionPrompt = \mode _ _ -> do
                  count <- readIORef prompts
                  modifyIORef' prompts (+ 1)
                  modifyIORef' trace (++ ["prompt"])
                  pure
                    ( mode
                    , if count == 0 then Just "ls" else Nothing
                    , initialPromptBuffers
                    )
              , sessionRememberInput = \mode text history -> do
                  modifyIORef' trace (++ ["remember:" ++ text])
                  pure (recordHistory (targetFor mode) text history)
              , sessionClearInterrupt = modifyIORef' trace (++ ["clear"])
              , sessionRunInput = \_ text conversation -> do
                  modifyIORef' trace (++ ["run:" ++ text])
                  pure (appendTerminal text conversation)
              }
          )
          initialSession
        actual <- readIORef trace
        actual @?= ["prompt", "remember:ls", "clear", "run:ls", "prompt"]
    , testCase "the next prompt sees the updated mode, buffers, history, and conversation" $ do
        let nextBuffers =
              storePrompt TermInput (setPromptText "draft" initialPrompt) initialPromptBuffers
        prompts <- newIORef (0 :: Int)
        runSession
          ( noEffects
              { sessionPrompt = \mode history buffers -> do
                  count <- readIORef prompts
                  modifyIORef' prompts (+ 1)
                  if count == 0
                    then pure (TermInput, Just "pwd", nextBuffers)
                    else do
                      mode @?= TermInput
                      promptText (activePrompt TermInput buffers) @?= "draft\n"
                      history @?= History [] ["pwd"]
                      pure (mode, Nothing, buffers)
              , sessionRememberInput = \_ text history ->
                  pure (recordHistory LlmHistory text history)
              , sessionClearInterrupt = pure ()
              , sessionRunInput = \_ _ conversation ->
                  pure (TerminalEntry "pwd" : conversation)
              }
          )
          initialSession
    , testCase "the conversation is threaded between inputs" $ do
        prompts <- newIORef (0 :: Int)
        runSession
          ( noEffects
              { sessionPrompt = \mode _ _ -> do
                  count <- readIORef prompts
                  modifyIORef' prompts (+ 1)
                  pure
                    ( mode
                    , case count of
                        0 -> Just "one"
                        1 -> Just "two"
                        _ -> Nothing
                    , initialPromptBuffers
                    )
              , sessionRememberInput = \_ _ history -> pure history
              , sessionClearInterrupt = pure ()
              , sessionRunInput = \_ text conversation -> do
                  when (text == "two") $
                    conversation @?= [TerminalEntry "one"]
                  pure (appendTerminal text conversation)
              }
          )
          initialSession
        readIORef prompts >>= (@?= 3)
    , testCase "rememberInput records non-blank entries in the matching history" $ do
        let history = rememberInput TermInput "ls" (History [] [])
        history @?= History ["ls"] []
        rememberInput TermInput "   " history @?= history
        rememberInput LlmInput "hello" history @?= History ["ls"] ["hello"]
    ]

initialSession :: Session
initialSession =
  Session
    { sessionMode = LlmInput
    , sessionBuffers = initialPromptBuffers
    , sessionHistory = History [] []
    , sessionConversation = emptyConversation
    }

noEffects :: SessionEffects
noEffects =
  SessionEffects
    { sessionPrompt = \_ _ _ -> assertFailure "prompt must not run"
    , sessionRememberInput = \_ _ _ -> assertFailure "remember must not run"
    , sessionClearInterrupt = assertFailure "interrupt must not be cleared"
    , sessionRunInput = \_ _ _ -> assertFailure "input must not run"
    }

targetFor :: InputMode -> HistoryTarget
targetFor LlmInput = LlmHistory
targetFor TermInput = TermHistory
