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
    , testCase "displayPath does not abbreviate sibling prefixes" $ do
        displayPath "/home/user" "/home/user2" @?= "/home/user2"
        displayPath "/" "/tmp/project" @?= "/tmp/project"
    , testCase "expandHome expands home-relative paths" $ do
        expandHome "/home/user" "~" @?= "/home/user"
        expandHome "/home/user" "~/project" @?= "/home/user/project"
        expandHome "/home/user" "relative" @?= "relative"
        expandHome "/home/user" "~other" @?= "~other"
    , testCase "splitPathWord separates directories from base names" $ do
        splitPathWord "" @?= ("", "")
        splitPathWord "file" @?= ("", "file")
        splitPathWord "dir/" @?= ("dir/", "")
        splitPathWord "dir/fi" @?= ("dir/", "fi")
        splitPathWord "/a/b" @?= ("/a/", "b")
    , testCase "hasTrailingSlash only matches a final slash" $ do
        hasTrailingSlash "dir/" @?= True
        hasTrailingSlash "/" @?= True
        hasTrailingSlash "dir" @?= False
        hasTrailingSlash "" @?= False
    ]