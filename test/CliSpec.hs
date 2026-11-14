module CliSpec (tests) where

import Data.List (isInfixOf)
import Hutian.Cli
import Test.QuickCheck (Property, counterexample, property, (.&&.))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "cli"
    [ testCase "options and flags parse in any order" $ do
        let result =
              parseOptions spec
                [ "--flag"
                , "--model"
                , "test-model"
                , "--url"
                , "http://example.test"
                ]
        case result of
          Left message -> assertFailure message
          Right options -> do
            map optionName options
              @?= ["--flag", "--model", "--url"]
            map optionValue options
              @?= [Nothing, Just "test-model", Just "http://example.test"]
            hasOption "--model" options @?= True
            optionValueFor "--url" options @?= Just "http://example.test"
            flagPresent "--flag" options @?= True
            flagPresent "--model" options @?= True
    , testCase "unknown, positional, and value-less options are rejected" $ do
        assertLeftNames "unknown argument" (parseOptions spec ["--other"])
        assertLeftNames "unknown argument" (parseOptions spec ["value"])
        assertLeftNames "missing value" (parseOptions spec ["--model"])
    , testCase "duplicate options are rejected instead of last-one-wins" $ do
        assertLeftNames
          "duplicate argument"
          (parseOptions spec ["--model", "one", "--model", "two"])
        assertLeftNames
          "duplicate argument"
          (parseOptions spec ["--flag", "--flag"])
    , testCase "missingOptions preserves the required order" $ do
        case parseOptions spec ["--model", "m"] of
          Left message -> assertFailure message
          Right options ->
            missingOptions ["--url", "--model", "--flag"] options
              @?= ["--url", "--flag"]
    , testProperty "every parsed value came from the argument list" prop_valuesFromArguments
    ]

spec :: [(String, OptionKind)]
spec =
  [ ("--url", OptionValue)
  , ("--model", OptionValue)
  , ("--flag", OptionFlag)
  ]

assertLeftNames :: String -> Either String a -> IO ()
assertLeftNames expected result =
  case result of
    Left message ->
      assertBool ("expected " ++ show expected ++ " in " ++ show message) (expected `isInfixOf` message)
    Right _ -> assertFailure ("expected failure mentioning " ++ show expected)

prop_valuesFromArguments :: String -> Property
prop_valuesFromArguments value =
  case parseOptions spec ["--model", value] of
    Left message -> counterexample message False
    Right options ->
      property (optionValueFor "--model" options == Just value)
        .&&. property (not (flagPresent "--flag" options))
