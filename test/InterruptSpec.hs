module InterruptSpec (tests) where

import Hutian.Interrupt
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "interrupt"
    [ testCase "a new flag starts clear" $ do
        flag <- newInterruptFlag
        isInterrupted flag >>= (@?= False)
    , testCase "clearInterrupt resets the flag" $ do
        flag <- newInterruptFlag
        clearInterrupt flag
        isInterrupted flag >>= (@?= False)
    , testCase "withInterruptFlag returns the action result" $ do
        flag <- newInterruptFlag
        result <- withInterruptFlag flag (pure (42 :: Int))
        result @?= 42
        isInterrupted flag >>= (@?= False)
    ]