module PromptSpec (tests) where

import Hutian.Editor
  ( InputMode (..)
  , activePrompt
  , initialPrompt
  , initialPromptBuffers
  , promptColumn
  , promptText
  , setPromptText
  , storePrompt
  , unColumn
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
            promptMode next @?= LlmInput
            bufferText LlmInput next @?= "\n"
          _ -> assertFailure "expected a submitted prompt"
    , testCase "submitting term input clears only the term buffer" $ do
        let state =
              initialPromptState
                TermInput
                ( storePrompt
                    TermInput
                    (setPromptText "ls" initialPrompt)
                    (storePrompt LlmInput (setPromptText "hello" initialPrompt) initialPromptBuffers)
                )
        case stepPrompt history KeyEnter state of
          StepFinish PromptSubmitted prompt next -> do
            promptText prompt @?= "ls\n"
            promptMode next @?= TermInput
            bufferText TermInput next @?= "\n"
            bufferText LlmInput next @?= "hello\n"
          _ -> assertFailure "expected a submitted prompt"
    , testCase "Ctrl+D exits on an empty prompt" $ do
        assertExit LlmInput
        assertExit TermInput
    , testCase "Ctrl+C clears only the active buffer" $ do
        let state =
              initialPromptState
                TermInput
                ( storePrompt
                    TermInput
                    (setPromptText "rm -rf" initialPrompt)
                    (storePrompt LlmInput (setPromptText "hello" initialPrompt) initialPromptBuffers)
                )
        next <- stepRedraw history KeyCancel state
        promptMode next @?= TermInput
        bufferText TermInput next @?= "\n"
        bufferText LlmInput next @?= "hello\n"
    , testCase "typing redraws and stores the updated text" $ do
        next <- stepRedraw history (KeyChar '!') llmState
        promptMode next @?= LlmInput
        bufferText LlmInput next @?= "hello!\n"
    , testCase "cursor movement moves without redrawing" $
        case stepPrompt history KeyLeft llmState of
          StepMoveCursor prompt next -> do
            unColumn (promptColumn prompt) @?= 4
            promptMode next @?= LlmInput
            bufferText LlmInput next @?= "hello\n"
          _ -> assertFailure "expected a cursor move"
    , testCase "multi-line term input continues instead of submitting" $ do
        next <- stepRedraw history KeyEnter (stateWith TermInput "echo 'open")
        promptMode next @?= TermInput
        bufferText TermInput next @?= "echo 'open\n\n"
    , testCase "history up recalls the active mode's newest entry" $ do
        next <- stepRedraw history KeyUp llmState
        bufferText LlmInput next @?= "llm\n"
    , testCase "history down restores the draft" $ do
        up <- stepRedraw history KeyUp llmState
        down <- stepRedraw history KeyDown up
        bufferText LlmInput down @?= "hello\n"
    , testCase "history down moves toward the newest entry" $ do
        newest <- stepRedraw historyWithTwo KeyUp llmState
        older <- stepRedraw historyWithTwo KeyUp newest
        down <- stepRedraw historyWithTwo KeyDown older
        bufferText LlmInput newest @?= "llm-2\n"
        bufferText LlmInput older @?= "llm-1\n"
        bufferText LlmInput down @?= "llm-2\n"
    , testCase "history navigation with no entries continues" $ do
        assertContinue KeyUp
        assertContinue KeyDown
    , testCase "editing resets history navigation" $ do
        newest <- stepRedraw historyWithTwo KeyUp llmState
        older <- stepRedraw historyWithTwo KeyUp newest
        bufferText LlmInput older @?= "llm-1\n"
        edited <- stepRedraw historyWithTwo (KeyChar 'x') older
        bufferText LlmInput edited @?= "llm-1x\n"
        newest' <- stepRedraw historyWithTwo KeyUp edited
        bufferText LlmInput newest' @?= "llm-2\n"
    , testCase "switching modes resets history navigation" $ do
        recalled <- stepRedraw historyWithTwo KeyUp llmState
        switched <- stepRedraw historyWithTwo KeyToggleMode recalled
        promptMode switched @?= TermInput
        bufferText LlmInput switched @?= "llm-2\n"
        newest <- stepRedraw historyWithTwo KeyUp switched
        bufferText TermInput newest @?= "term-2\n"
    , testCase "completion is requested in term mode" $
        case stepPrompt history KeyComplete (stateWith TermInput "ls") of
          StepComplete next -> do
            promptMode next @?= TermInput
            bufferText TermInput next @?= "ls\n"
          _ -> assertFailure "expected a completion request"
    , testCase "accepting a completion stores it and resets history" $ do
        recalled <- stepRedraw historyWithTwo KeyUp llmState
        let completed = setPromptText "echo" initialPrompt
            next = acceptCompletion completed recalled
        bufferText LlmInput next @?= "echo\n"
        newest <- stepRedraw historyWithTwo KeyUp next
        bufferText LlmInput newest @?= "llm-2\n"
    ]

history :: History
history = History ["term"] ["llm"]

historyWithTwo :: History
historyWithTwo = History ["term-1", "term-2"] ["llm-1", "llm-2"]

llmState :: PromptState
llmState = stateWith LlmInput "hello"

stateWith :: InputMode -> String -> PromptState
stateWith mode text =
  initialPromptState
    mode
    (storePrompt mode (setPromptText text initialPrompt) initialPromptBuffers)

bufferText :: InputMode -> PromptState -> String
bufferText mode = promptText . activePrompt mode . promptBuffers

stepRedraw :: History -> Key -> PromptState -> IO PromptState
stepRedraw hist key state =
  case stepPrompt hist key state of
    StepRedraw next -> pure next
    _ -> assertFailure "expected a redraw" >> pure state

assertExit :: InputMode -> Assertion
assertExit mode =
  case stepPrompt history KeySubmit (initialPromptState mode initialPromptBuffers) of
    StepFinish PromptExited prompt next -> do
      promptText prompt @?= "\n"
      promptMode next @?= mode
      bufferText mode next @?= "\n"
    _ -> assertFailure ("expected " ++ show mode ++ " to exit")

assertContinue :: Key -> Assertion
assertContinue key =
  case stepPrompt (History [] []) key llmState of
    StepContinue next -> bufferText LlmInput next @?= "hello\n"
    _ -> assertFailure "expected history navigation to continue"
