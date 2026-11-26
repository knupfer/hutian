{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the interactive session loop, driven by stub effects.
module SessionSpec (tests) where

import Control.Monad (when)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Hutian.Conversation
  ( Conversation
  , ConversationEntry (..)
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
import Hutian.History (History (..))
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
              { sessionPrompt = \mode history _ -> do
                  count <- readIORef prompts
                  modifyIORef' prompts (+ 1)
                  modifyIORef' trace (++ ["prompt"])
                  when (count == 1) $ history @?= History [] ["ls"]
                  pure
                    ( mode
                    , if count == 0 then Just "ls" else Nothing
                    , initialPromptBuffers
                    )
              , sessionClearInterrupt = modifyIORef' trace (++ ["clear"])
              , sessionRunInput = \_ text conversation -> do
                  modifyIORef' trace (++ ["run:" ++ text])
                  pure (appendTerminal text conversation)
              }
          )
          initialSession
        actual <- readIORef trace
        actual @?= ["prompt", "clear", "run:ls", "prompt"]
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
                      history @?= History ["pwd"] []
                      pure (mode, Nothing, buffers)
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
              , sessionClearInterrupt = pure ()
              , sessionRunInput = \_ text conversation -> do
                  when (text == "two") $
                    conversation @?= [TerminalEntry "one"]
                  pure (appendTerminal text conversation)
              }
          )
          initialSession
        readIORef prompts >>= (@?= 3)
    , testCase "the final conversation is snapshotted when input ends" $ do
        snapshots <- newIORef ([] :: [Conversation])
        prompts <- newIORef (0 :: Int)
        runSession
          ( noEffects
              { sessionPrompt = \mode _ _ -> do
                  count <- readIORef prompts
                  modifyIORef' prompts (+ 1)
                  pure
                    ( mode
                    , if count == 0 then Just "ls" else Nothing
                    , initialPromptBuffers
                    )
              , sessionClearInterrupt = pure ()
              , sessionRunInput = \_ text conversation ->
                  pure (appendTerminal text conversation)
              , sessionSnapshot = \conversation ->
                  modifyIORef' snapshots (++ [conversation])
              }
          )
          initialSession
        readIORef snapshots >>= (@?= [[TerminalEntry "ls"]])
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
    , sessionClearInterrupt = assertFailure "interrupt must not be cleared"
    , sessionRunInput = \_ _ _ -> assertFailure "input must not run"
    , sessionSnapshot = \_ -> pure ()
    }
