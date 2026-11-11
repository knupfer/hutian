module InterruptSpec (tests) where

import Control.Concurrent (threadDelay)
import Hutian.Interrupt
import System.Posix.Signals (raiseSignal, sigINT)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "interrupt"
    [ testCase "a new flag starts clear" $ do
        flag <- newInterruptFlag
        isInterrupted flag >>= (@?= False)
    , testCase "the installed handler records SIGINT" $ do
        flag <- newInterruptFlag
        installInterruptHandler flag
        raiseSignal sigINT
        waitUntilInterrupted 100 flag >>= (@?= True)
    , testCase "clearInterrupt resets the flag" $ do
        flag <- newInterruptFlag
        installInterruptHandler flag
        raiseSignal sigINT
        waitUntilInterrupted 100 flag >>= (@?= True)
        clearInterrupt flag
        isInterrupted flag >>= (@?= False)
    ]

-- | Signal handlers run in their own thread, so poll briefly for the interrupt
-- to become visible.
waitUntilInterrupted :: Int -> InterruptFlag -> IO Bool
waitUntilInterrupted attempts flag
  | attempts <= 0 = pure False
  | otherwise = do
      interrupted <- isInterrupted flag
      if interrupted
        then pure True
        else threadDelay 10000 >> waitUntilInterrupted (attempts - 1) flag
