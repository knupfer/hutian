module VerbositySpec (tests) where

import Hutian.Context (TurnContext (..))
import Hutian.Verbosity
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "verbosity"
    [ testCase "low labels reasoning, tool output, and subagent answers" $ do
        renderMode VerbosityLow MainContext
          @?= RenderMode
            { modeReasoning = LabelOnly
            , modeAnswer = Full
            , modeAnswerStyle = AnswerNormal
            , modeToolOutput = LabelOnly
            }
        renderMode VerbosityLow SubagentContext
          @?= RenderMode
            { modeReasoning = LabelOnly
            , modeAnswer = LabelOnly
            , modeAnswerStyle = AnswerDim
            , modeToolOutput = LabelOnly
            }
    , testCase "normal shows main reasoning and full main output" $
        renderMode VerbosityNormal MainContext
          @?= RenderMode
            { modeReasoning = Full
            , modeAnswer = Full
            , modeAnswerStyle = AnswerNormal
            , modeToolOutput = Full
            }
    , testCase "normal presents subagents like low and keeps their answer" $
        renderMode VerbosityNormal SubagentContext
          @?= RenderMode
            { modeReasoning = LabelOnly
            , modeAnswer = Full
            , modeAnswerStyle = AnswerDim
            , modeToolOutput = LabelOnly
            }
    , testCase "high shows everything and dims subagent answers" $ do
        renderMode VerbosityHigh MainContext
          @?= RenderMode
            { modeReasoning = Full
            , modeAnswer = Full
            , modeAnswerStyle = AnswerNormal
            , modeToolOutput = Full
            }
        renderMode VerbosityHigh SubagentContext
          @?= RenderMode
            { modeReasoning = Full
            , modeAnswer = Full
            , modeAnswerStyle = AnswerDim
            , modeToolOutput = Full
            }
    , testCase "full visibility includes the body" $
        outputBodyVisible Full @?= True
    , testCase "label-only visibility hides the body" $
        outputBodyVisible LabelOnly @?= False
    , testCase "arguments round-trip" $
        mapM_
          ( \verbosity ->
              verbosityFromArgument (verbosityArgument verbosity)
                @?= Just verbosity
          )
          [VerbosityLow, VerbosityNormal, VerbosityHigh]
    ]
