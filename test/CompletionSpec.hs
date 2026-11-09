module CompletionSpec (tests) where

import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe)
import Hutian.Completion
import Hutian.Paths (hasTrailingSlash)
import Test.QuickCheck (Property, (.&&.), counterexample, property)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "completion"
    [ testCase "a unique command match appends a space" $
        chooseCompletion (const True) "ec" "" ["echo"]
          @?= Completion ["echo"] "echo "
    , testCase "a unique match keeps text after the cursor" $
        chooseCompletion (const True) "ec" "ho" ["echo"]
          @?= Completion ["echo"] "echo"
    , testCase "a unique path keeps a trailing slash without a space" $
        chooseCompletion (not . hasTrailingSlash) "di" "" ["dir/"]
          @?= Completion ["dir/"] "dir/"
    , testCase "a unique path without a slash gets a space" $
        chooseCompletion (not . hasTrailingSlash) "fi" "" ["file"]
          @?= Completion ["file"] "file "
    , testCase "ambiguous matches extend to their common prefix" $
        chooseCompletion (const True) "al" "" ["alpha", "alpine"]
          @?= Completion ["alpha", "alpine"] "alp"
    , testCase "matches without a longer prefix stay unchanged" $
        chooseCompletion (const True) "e" "" ["echo", "exit"]
          @?= Completion ["echo", "exit"] "e"
    , testCase "no matches keep the original word" $
        chooseCompletion (const True) "zz" "" []
          @?= Completion [] "zz"
    , testCase "longestCommonPrefix handles empty and shared prefixes" $ do
        longestCommonPrefix [] @?= ""
        longestCommonPrefix ["alpha"] @?= "alpha"
        longestCommonPrefix ["alpha", "alpine"] @?= "alp"
        longestCommonPrefix ["alpha", "beta"] @?= ""
    , testProperty "longestCommonPrefix is a prefix of every input" prop_longestCommonPrefix
    ]

prop_longestCommonPrefix :: [String] -> Property
prop_longestCommonPrefix strings =
  let common = longestCommonPrefix strings
      shortest = maybe 0 length (listToMaybe strings)
   in counterexample (show (strings, common)) $
        property (all (common `isPrefixOf`) strings)
          .&&. property (length common <= shortest)
