-- | Entry point for the hutian sandbox. A terminal on stdin and stdout starts
-- the interactive shell; any other invocation runs one batch turn.
module Main (main) where

import Control.Exception (finally)
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.Foldable (for_)
import Hutian.Agent
  ( RuntimeEnv (..)
  , discoverMcpSettings
  , runBatchInput
  , runInput
  )
import Hutian.Console
  ( configureConsole
  , consoleFlush
  , consoleWrite
  , consoleWriteErrorLine
  , consoleWriteSystemPrompt
  , consoleWriteToolDefinitions
  , resetTerminal
  )
import Hutian.Context (TurnContext (MainContext))
import Hutian.Conversation (emptyConversation)
import Hutian.Editor (InputMode (..), initialPromptBuffers)
import Hutian.History (History (..))
import Hutian.Interrupt
  ( clearInterrupt
  , installInterruptHandler
  , newInterruptFlag
  )
import Hutian.Jobs.Runtime (withJobRuntime)
import Hutian.Json (decodeUtf8Lenient)
import Hutian.Llm
  ( LlmSettings (..)
  , ModelName (..)
  , SystemPrompt (..)
  , loadLlmLaunch
  )
import Hutian.Mcp (newMcpSessionCache)
import Hutian.Output (interactiveOutput, silentOutput)
import Hutian.Render (trailingNewline)
import Hutian.Session
  ( Session (..)
  , SessionEffects (..)
  , runSession
  )
import Hutian.Shell (executableCommands)
import Hutian.Terminal (promptForInput)
import Hutian.Tool (toolDefinitionsFor)
import Hutian.Tool.Summary (toolSummaryLines)
import Hutian.Transcript
  ( SessionInfo (..)
  , TranscriptEvent (..)
  , closeTranscript
  , openTranscript
  , recordTranscript
  , recordTranscriptMaybe
  )
import Hutian.Verbosity (Verbosity (..))
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO (hIsTerminalDevice, stdin, stdout)

main :: IO ()
main = do
  configureConsole
  arguments <- getArgs
  parsed <- loadLlmLaunch arguments
  case parsed of
    Left message -> failLaunch message
    Right (loaded, promptWords) -> do
      settings <- discoverMcpSettings loaded
      -- The launcher starts the shell in /source. Leave it before the session
      -- begins so the long-lived process does not pin that mount in gVisor;
      -- each command enters the working directory for its own lifetime.
      workingDirectory <- getCurrentDirectory
      -- Open the transcript before leaving the session directory so a relative
      -- --transcript path means the same place as the user's other paths.
      transcript <- traverse openTranscript (settingTranscript settings)
      setCurrentDirectory "/"
      terminals <-
        (&&) <$> hIsTerminalDevice stdin <*> hIsTerminalDevice stdout
      -- An explicit prompt always means one batch turn, even on a terminal.
      let interactive = terminals && null promptWords
      let sessionInfo =
            SessionInfo
              { sessionInfoModel = unModelName (settingModel settings)
              }
          startTranscript =
            for_ transcript $ \recorder ->
              recordTranscript
                recorder
                (TranscriptSessionStarted sessionInfo)
          finishTranscript =
            for_ transcript $ \recorder -> do
              recordTranscript recorder (TranscriptSessionEnded "exit")
              closeTranscript recorder
      startTranscript
      interruptFlag <- newInterruptFlag
      installInterruptHandler interruptFlag
      mcpSessions <- newMcpSessionCache
      directory <- jobDirectory
      let withEnvironment jobs = do
            let env =
                  RuntimeEnv
                    { envWorkingDirectory = workingDirectory
                    , envJobs = jobs
                    , envSettings = settings
                    , envInterrupt = interruptFlag
                    , envMcpSessions = mcpSessions
                    , envTranscript = transcript
                    , envOutput =
                        if interactive then interactiveOutput else silentOutput
                    }
            if interactive
              then runInteractive env
              else runBatch env promptWords
      withJobRuntime directory workingDirectory withEnvironment
        `finally` finishTranscript

-- | The normal two-mode shell. Only used when stdin and stdout are terminals
-- and no prompt argument was given.
runInteractive :: RuntimeEnv -> IO ()
runInteractive env = do
  let settings = envSettings env
  commands <- executableCommands
  let session =
        Session
          { sessionMode = LlmInput
          , sessionBuffers = initialPromptBuffers
          , sessionHistory = History [] []
          , sessionConversation = emptyConversation
          }
  when (settingVerbosity settings == VerbosityHigh) $ do
    consoleWriteSystemPrompt
      MainContext
      (unSystemPrompt (settingSystemPrompt settings))
    consoleWriteToolDefinitions
      MainContext
      (toolSummaryLines (toolDefinitionsFor (settingTools settings)))
  runSession
    (sessionEffects env commands)
    session
    `finally` resetTerminal

-- | One model turn with no terminal presentation. The prompt comes from the
-- positional arguments, or from stdin when no arguments were given. Only the
-- main agent's final answer is written to stdout.
runBatch :: RuntimeEnv -> [String] -> IO ()
runBatch env promptWords = do
  clearInterrupt (envInterrupt env)
  message <- batchMessage promptWords
  case message of
    Left problem -> failUsage problem
    Right prompt -> do
      result <- runBatchInput env prompt
      case result of
        Left problem -> do
          consoleWriteErrorLine problem
          exitWith (ExitFailure 1)
        Right answer -> do
          consoleWrite (trailingNewline answer)
          consoleFlush

-- | Positional arguments are the first user message. Without them, batch mode
-- reads the message from stdin; an interactive stdin would have no prompt to
-- show, so it is an error.
batchMessage :: [String] -> IO (Either String String)
batchMessage [] = do
  stdinIsTerminal <- hIsTerminalDevice stdin
  if stdinIsTerminal
    then pure (Left "hutian: pass a prompt argument or pipe one on standard input")
    else Right . decodeUtf8Lenient <$> BS.getContents
batchMessage promptWords = pure (Right (unwords promptWords))

failLaunch :: String -> IO a
failLaunch message = do
  consoleWriteErrorLine message
  exitWith (ExitFailure 2)

failUsage :: String -> IO a
failUsage message = failLaunch (message ++ "\n" ++ batchUsage)

batchUsage :: String
batchUsage =
  "usage: hutian [options] [--] [PROMPT...]\n"
    ++ "       PROMPT... | hutian [options]"

-- | The interactive loop's effects, bound to the real terminal, job runtime,
-- and model client.
sessionEffects :: RuntimeEnv -> [String] -> SessionEffects
sessionEffects env commands =
  SessionEffects
    { sessionPrompt = promptForInput (envWorkingDirectory env) commands
    , sessionClearInterrupt = clearInterrupt (envInterrupt env)
    , sessionRunInput = runInput env
    , sessionSnapshot =
        recordTranscriptMaybe (envTranscript env) . TranscriptConversationSnapshot
    }

-- | Keep job logs under the session's runtime directory when one is
-- configured, matching the relay's socket convention.
jobDirectory :: IO FilePath
jobDirectory = do
  runtimeDirectory <- lookupEnv "XDG_RUNTIME_DIR"
  pure $ case runtimeDirectory of
    Just directory | not (null directory) -> directory </> "hutian-jobs"
    _ -> "/tmp/hutian-jobs"
