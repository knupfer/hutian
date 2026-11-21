module PolicySpec (tests) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Hutian.Policy
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "policy"
    [ testCase "policies round-trip through their argument spelling" $
        mapM_
          (\policy -> toolPolicyFromArgument (toolPolicyArgument policy) @?= Just policy)
          [ToolDeny, ToolAsk, ToolAllow]
    , testCase "ask and allow advertise the tool while deny does not" $ do
        toolPolicyAdvertised ToolDeny @?= False
        toolPolicyAdvertised ToolAsk @?= True
        toolPolicyAdvertised ToolAllow @?= True
    , testCase "unknown policies are rejected" $
        toolPolicyFromArgument "sometimes" @?= Nothing
    , testCase "JSON uses the same stable spelling" $ do
        LBS8.unpack (Aeson.encode ToolDeny) @?= "\"deny\""
        LBS8.unpack (Aeson.encode ToolAsk) @?= "\"ask\""
        LBS8.unpack (Aeson.encode ToolAllow) @?= "\"allow\""
        Aeson.decode (LBS8.pack "\"allow\"") @?= Just ToolAllow
        Aeson.decode (LBS8.pack "\"sometimes\"") @?= (Nothing :: Maybe ToolPolicy)
    ]
