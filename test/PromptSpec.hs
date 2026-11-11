module PromptSpec (tests) where

import Hutian.Editor
  ( PromptMode (..)
  , activePrompt
  , initialPrompt
  , initialPromptBuffers
  , promptText
  , setPromptText
  , storePrompt
  )
import Hutian.History (History (..))
import Hutian.Key (Key (..))
import Hutian.Prompt
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "prompt"
    [ testCase "submitting a non-empty llm prompt clears its buffer" $
        case stepPrompt history KeySubmit llmState of
          StepFinish PromptSubmitted prompt next -> do
            promptText prompt @?= "hello\n"
            promptText (activePrompt LlmMode (promptBuffers next)) @?= "\n"
          _ -> assertFailure "expected a submitted prompt"
    , testCase "switching modes preserves the llm buffer" $
        case stepPrompt history KeyToggleMode llmState of
          StepRedraw next -> do
            promptMode next @?= TermMode
            promptText (activePrompt LlmMode (promptBuffers next)) @?= "hello\n"
          _ -> assertFailure "expected a redraw"
    , testCase "history up recalls the active mode's newest entry" $
        case stepPrompt history KeyUp llmState of
          StepRedraw next ->
            promptText (activePrompt LlmMode (promptBuffers next)) @?= "llm\n"
          _ -> assertFailure "expected a redraw"
    , testCase "accepting a completion stores it in the active buffer" $ do
        let completed = setPromptText "echo" initialPrompt
            next = acceptCompletion completed llmState
        promptText (activePrompt LlmMode (promptBuffers next)) @?= "echo\n"
    ]

history :: History
history = History ["term"] ["llm"]

llmState :: PromptState
llmState =
  initialPromptState
    LlmMode
    (storePrompt LlmMode (setPromptText "hello" initialPrompt) initialPromptBuffers)
