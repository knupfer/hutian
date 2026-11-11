{-# LANGUAGE ScopedTypeVariables #-}

-- | Cooperative handling of terminal interrupts.
module Hutian.Interrupt
  ( InterruptFlag
  , newInterruptFlag
  , clearInterrupt
  , isInterrupted
  , withInterruptFlag
  ) where

import Control.Exception (finally)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT)

-- | A flag shared with signal handlers and long-running operations.
newtype InterruptFlag = InterruptFlag (IORef Bool)

newInterruptFlag :: IO InterruptFlag
newInterruptFlag = InterruptFlag <$> newIORef False

clearInterrupt :: InterruptFlag -> IO ()
clearInterrupt (InterruptFlag flag) = writeIORef flag False

isInterrupted :: InterruptFlag -> IO Bool
isInterrupted (InterruptFlag flag) = readIORef flag

-- | Install a @SIGINT@ handler for the duration of an action. The handler
-- records the interrupt instead of terminating the process, so the action can
-- stop cooperatively.
withInterruptFlag :: InterruptFlag -> IO a -> IO a
withInterruptFlag (InterruptFlag flag) action = do
  previousHandler <- installHandler sigINT (Catch (writeIORef flag True)) Nothing
  action `finally` installHandler sigINT previousHandler Nothing
