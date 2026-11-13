module PolicySpec (tests) where

import Hutian.Policy
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "policy"
    [ testCase "model shell policies round-trip through their argument" $ do
        mapM_
          (\policy -> modelShellFromArgument (modelShellArgument policy) @?= Just policy)
          [ModelShellDenied, ModelShellAsk, ModelShellAllowed]
    , testCase "ask and allow advertise the tool while deny does not" $ do
        modelShellAdvertised ModelShellDenied @?= False
        modelShellAdvertised ModelShellAsk @?= True
        modelShellAdvertised ModelShellAllowed @?= True
    , testCase "unknown model shell policies are rejected" $
        modelShellFromArgument "sometimes" @?= Nothing
    ]
