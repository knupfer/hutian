{-# LANGUAGE ScopedTypeVariables #-}

-- | Cooperative handling of terminal interrupts.
module Hutian.Interrupt
  ( InterruptFlag
  , newInterruptFlag
  , clearInterrupt
  , setInterrupt
  , isInterrupted
  , interruptPending
  , installInterruptHandler
  ) where

import Control.Concurrent.STM (STM, TVar, atomically, check, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Monad (void)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT)

-- | A flag shared with signal handlers and long-running operations. A 'TVar'
-- lets a blocked request wake up as soon as the flag is set, without polling.
newtype InterruptFlag = InterruptFlag (TVar Bool)

newInterruptFlag :: IO InterruptFlag
newInterruptFlag = InterruptFlag <$> newTVarIO False

clearInterrupt :: InterruptFlag -> IO ()
clearInterrupt (InterruptFlag flag) = atomically (writeTVar flag False)

setInterrupt :: InterruptFlag -> IO ()
setInterrupt (InterruptFlag flag) = atomically (writeTVar flag True)

isInterrupted :: InterruptFlag -> IO Bool
isInterrupted (InterruptFlag flag) = readTVarIO flag

-- | Block until an interrupt is requested. Used with 'STM.orElse' to race a
-- request against the user's interrupt.
interruptPending :: InterruptFlag -> STM ()
interruptPending (InterruptFlag flag) = readTVar flag >>= check

-- | Install a process-wide @SIGINT@ handler. The handler records the
-- interrupt instead of terminating the process, so running commands can stop
-- while hutian keeps running.
installInterruptHandler :: InterruptFlag -> IO ()
installInterruptHandler (InterruptFlag flag) =
  void (installHandler sigINT (Catch (atomically (writeTVar flag True))) Nothing)
