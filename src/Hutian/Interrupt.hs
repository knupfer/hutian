{-# LANGUAGE ScopedTypeVariables #-}

-- | Cooperative handling of terminal interrupts.
module Hutian.Interrupt
  ( InterruptFlag
  , newInterruptFlag
  , clearInterrupt
  , setInterrupt
  , isInterrupted
  , installInterruptHandler
  ) where

import Control.Monad (void)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT)

-- | A flag shared with signal handlers and long-running operations.
newtype InterruptFlag = InterruptFlag (IORef Bool)

newInterruptFlag :: IO InterruptFlag
newInterruptFlag = InterruptFlag <$> newIORef False

clearInterrupt :: InterruptFlag -> IO ()
clearInterrupt (InterruptFlag flag) = writeIORef flag False

setInterrupt :: InterruptFlag -> IO ()
setInterrupt (InterruptFlag flag) = writeIORef flag True

isInterrupted :: InterruptFlag -> IO Bool
isInterrupted (InterruptFlag flag) = readIORef flag

-- | Install a process-wide @SIGINT@ handler. The handler records the
-- interrupt instead of terminating the process, so running commands can stop
-- while hutian keeps running.
installInterruptHandler :: InterruptFlag -> IO ()
installInterruptHandler (InterruptFlag flag) =
  void (installHandler sigINT (Catch (writeIORef flag True)) Nothing)
