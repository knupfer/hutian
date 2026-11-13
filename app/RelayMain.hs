{-# LANGUAGE ScopedTypeVariables #-}

-- | Entry point for the host-side LLM relay.
module Main (main) where

import Control.Exception (SomeException, try)
import Hutian.Relay
  ( RelayRuntime
  , loadRelayKey
  , newRelayRuntime
  , parseRelaySettings
  , relayKeySource
  , runRelay
  )
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  arguments <- getArgs
  case parseRelaySettings arguments of
    Left message -> failWith message
    Right settings -> do
      keyResult <- loadRelayKey (relayKeySource settings)
      case keyResult of
        Left message -> failWith message
        Right key -> do
          runtimeResult <-
            try (newRelayRuntime settings key)
              :: IO (Either SomeException RelayRuntime)
          case runtimeResult of
            Left _ -> failWith "hutian-llm-relay: invalid upstream URL"
            Right runtime -> runRelay runtime

failWith :: String -> IO a
failWith message = do
  hPutStrLn stderr message
  exitWith (ExitFailure 2)
