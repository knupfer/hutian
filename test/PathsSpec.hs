module PathsSpec (tests) where

import Data.List (isSuffixOf)
import Hutian.Paths
import Test.QuickCheck (Property, (.&&.), (===), counterexample, property)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "paths"
    [ testCase "splitPathWord separates directories from base names" $ do
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
    , testCase "formatCompletion marks directories with a slash" $ do
        formatCompletion "dir" True @?= "dir/"
        formatCompletion "file" False @?= "file"
    , testProperty "formatCompletion never shortens a path" prop_formatCompletionLength
    , testProperty "splitPathWord reconstructs the original word" prop_splitPathWord
    , testProperty "splitPathWord returns a slash-terminated directory prefix" prop_splitPathWordDirectory
    , testProperty "hasTrailingSlash agrees with the final character" prop_hasTrailingSlash
    ]

prop_formatCompletionLength :: FilePath -> Bool -> Property
prop_formatCompletionLength candidate isDirectory =
  counterexample (show (candidate, isDirectory)) $
    property (length (formatCompletion candidate isDirectory) >= length candidate)

prop_splitPathWord :: String -> Property
prop_splitPathWord word =
  counterexample (show (directory, base)) $
    property (directory ++ base == word)
      .&&. property ('/' `notElem` base)
  where
    (directory, base) = splitPathWord word

prop_splitPathWordDirectory :: String -> Property
prop_splitPathWordDirectory word =
  counterexample (show directory) $
    property (null directory || "/" `isSuffixOf` directory)
  where
    (directory, _) = splitPathWord word

prop_hasTrailingSlash :: FilePath -> Property
prop_hasTrailingSlash value =
  counterexample (show value) $
    hasTrailingSlash value === (not (null value) && "/" `isSuffixOf` value)
