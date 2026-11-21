module WebSpec (tests) where

import Data.Char (toLower)
import Data.List (isInfixOf)
import Hutian.Web (websearchRequestUrl)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "web"
    [ testCase "the query is appended with q= when the URL has no placeholder" $
        websearchRequestUrl "https://search.test/" "hutian tools"
          @?= "https://search.test/?q=hutian%20tools"
    , testCase "an existing query string gets an ampersand" $
        websearchRequestUrl "https://search.test/?lang=en" "hutian tools"
          @?= "https://search.test/?lang=en&q=hutian%20tools"
    , testCase "a trailing separator adds the query parameter without a separator" $ do
        websearchRequestUrl "https://search.test/?" "hutian"
          @?= "https://search.test/?q=hutian"
        websearchRequestUrl "https://search.test/?lang=en&" "hutian"
          @?= "https://search.test/?lang=en&q=hutian"
        websearchRequestUrl "https://search.test/?q=" "hutian"
          @?= "https://search.test/?q=hutian"
    , testCase "{query} is replaced in place" $
        websearchRequestUrl "https://search.test/search?term={query}&page=1" "a b"
          @?= "https://search.test/search?term=a%20b&page=1"
    , testCase "%s is replaced in place" $
        websearchRequestUrl "https://search.test/search/%s?page=1" "a b"
          @?= "https://search.test/search/a%20b?page=1"
    , testCase "special characters are percent-encoded" $ do
        let url = map toLower (websearchRequestUrl "https://search.test/?q=" "a&b=c")
        assertBool "the ampersand must be encoded" ("a%26b%3dc" `isInfixOf` url)
    ]
