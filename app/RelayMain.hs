{-# LANGUAGE ScopedTypeVariables #-}

-- | Entry point for the host-side LLM relay.
module Main (main) where

import Control.Exception (SomeException, try)
import Hutian.Relay
  ( RelayRuntime
  , loadRelayKey
  , newRelayRuntime
  , parseRelaySettings
  , runRelay
  , stderrLogger
  )
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr, stdin)

main :: IO ()
main = do
  arguments <- getArgs
  case parseRelaySettings arguments of
    Left message -> failWith message
    Right settings -> do
      keyResult <- loadRelayKey stdin
      case keyResult of
        Left message -> failWith message
        Right key -> do
          runtimeResult <-
            try (newRelayRuntime stderrLogger settings key)
              :: IO (Either SomeException RelayRuntime)
          case runtimeResult of
            Left _ -> failWith "hutian-llm-relay: invalid upstream URL"
            Right runtime -> runRelay runtime

failWith :: String -> IO a
failWith message = do
  hPutStrLn stderr message
  exitWith (ExitFailure 2)
