module PathsSpec (tests) where

import Hutian.Paths
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "paths"
    [ testCase "displayPath abbreviates the home directory" $ do
        displayPath "/home/user" "/home/user" @?= "~"
        displayPath "/home/user" "/home/user/project" @?= "~/project"
        displayPath "/home/user" "/tmp/project" @?= "/tmp/project"
    , testCase "expandHome expands home-relative paths" $ do
        expandHome "/home/user" "~" @?= "/home/user"
        expandHome "/home/user" "~/project" @?= "/home/user/project"
        expandHome "/home/user" "relative" @?= "relative"
    ]