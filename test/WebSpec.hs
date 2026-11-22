module WebSpec (tests) where

import Hutian.Web (FetchResult (..), fetchResultText)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "web"
    [ testCase "a fetched response reports its status and content type" $
        fetchResultText (FetchResult 200 "text/html" "hello")
          @?= "HTTP 200 (text/html)\n\nhello\n"
    , testCase "a missing content type leaves only the status" $
        fetchResultText (FetchResult 404 "" "not found")
          @?= "HTTP 404\n\nnot found\n"
    , testCase "an existing trailing newline is preserved once" $
        fetchResultText (FetchResult 200 "text/plain" "hello\n")
          @?= "HTTP 200 (text/plain)\n\nhello\n"
    ]
