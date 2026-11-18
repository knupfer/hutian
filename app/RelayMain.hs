{-# LANGUAGE ScopedTypeVariables #-}

-- | Entry point for the host-side LLM relay.
module Main (main) where

import Control.Exception (try)
import Hutian.Relay
  ( RelayAuth (..)
  , RelayAuthPolicy (..)
  , RelayRuntime
  , loadRelayAuth
  , newRelayRuntime
  , parseRelaySettings
  , relayAuthPolicy
  , runRelay
  , stderrLogger
  )
import Network.HTTP.Client (HttpException)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr, stdin)
import System.Posix.Signals (Handler (Ignore), installHandler, sigINT)

main :: IO ()
main = do
  -- The relay runs in the launcher's foreground process group while the
  -- sandbox owns the terminal, so Ctrl+C would otherwise kill the relay and
  -- remove its socket. The launcher stops the relay with SIGTERM on exit.
  _ <- installHandler sigINT Ignore Nothing
  arguments <- getArgs
  case parseRelaySettings arguments of
    Left message -> failWith message
    Right settings -> do
      authResult <-
        case relayAuthPolicy settings of
          RelayAuthDisabled -> pure (Right RelayAuthNone)
          RelayAuthRequired -> loadRelayAuth stdin
      case authResult of
        Left message -> failWith message
        Right auth -> do
          runtimeResult <-
            try (newRelayRuntime stderrLogger settings auth)
              :: IO (Either HttpException RelayRuntime)
          case runtimeResult of
            Left _ -> failWith "hutian-llm-relay: invalid upstream URL"
            Right runtime -> runRelay runtime

failWith :: String -> IO a
failWith message = do
  hPutStrLn stderr message
  exitWith (ExitFailure 2)
