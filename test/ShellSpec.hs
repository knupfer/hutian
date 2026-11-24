module ShellSpec (tests) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (isInfixOf)
import Hutian.Highlight (sanitizeOutputBytes)
import Hutian.Shell
  ( CapturedOutput (..)
  , CommandResult (..)
  , capturedOutputText
  , commandResultText
  , executableCommands
  , runShellCommand
  , runShellCommandCaptured
  , runShellCommandOutput
  , runShellCommandStreaming
  , shellExitSuffix
  )
import System.Directory
  ( Permissions (..)
  , getCurrentDirectory
  , getPermissions
  , removeFile
  , setPermissions
  )
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), withFile)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit
import TestSupport
  ( temporaryFilePath
  , withEnv
  , withTemporaryDirectory
  )

tests :: TestTree
tests =
  testGroup
    "shell"
    [ testCase "runShellCommand executes through /bin/sh" $ do
        path <- temporaryFilePath "hutian-shell-output"
        failure <- runShellCommand "/" ("printf hello > " ++ shellQuote path)
        failure @?= Right ()
        contents <- readFile path
        contents @?= "hello"
        removeFile path
    , testCase "runShellCommandOutput captures output and exit status" $ do
        hello <- runShellCommandOutput "/" "printf hello"
        case hello of
          Left message -> assertFailure message
          Right output -> commandResultText output @?= "hello"
        failed <- runShellCommandOutput "/" "printf bad >&2; exit 3"
        case failed of
          Left message -> assertFailure message
          Right output -> do
            assertBool "stderr should be captured" ("bad" `isInfixOf` commandResultText output)
            assertBool "exit status should be reported" ("[exit 3]" `isInfixOf` commandResultText output)
    , testCase "runShellCommandCaptured keeps bytes and decodes them once" $ do
        result <- runShellCommandCaptured "/" "printf out; printf err >&2; exit 3"
        case result of
          Left message -> assertFailure message
          Right captured -> do
            capturedStdout captured @?= BS8.pack "out"
            capturedStderr captured @?= BS8.pack "err"
            capturedExitCode captured @?= ExitFailure 3
            capturedOutputText captured @?= "outerr\n[exit 3]\n"
    , testCase "runShellCommandCaptured decodes invalid UTF-8 leniently" $ do
        result <- runShellCommandCaptured "/" "printf '\\377'"
        case result of
          Left message -> assertFailure message
          Right captured -> capturedOutputText captured @?= "\65533"
    , testCase "commands run in the requested working directory" $
        withTemporaryDirectory "hutian-shell-cwd" $ \directory -> do
          path <- temporaryFilePath "hutian-shell-pwd"
          failure <- runShellCommand directory ("pwd > " ++ shellQuote path)
          failure @?= Right ()
          contents <- readFile path
          contents @?= directory ++ "\n"
          removeFile path
    , testCase "runShellCommandStreaming sanitizes control characters" $ do
        (failure, output, errors) <-
          captureStreamed
            "printf 'a\\033[31mb\\nc\\td'; printf 'err\\033[2J' >&2"
        failure @?= Right ()
        output @?= BS8.pack "a [31mb\nc\td"
        errors @?= BS8.pack "err [2J"
    , testCase "runShellCommandStreaming leaves UTF-8 bytes alone" $ do
        (_, output, _) <- captureStreamed "printf 'a\\303\\251'"
        output @?= BS8.pack "a\195\169"
    , testCase "runShellCommandStreaming does not wait for background children" $ do
        result <- timeout 3000000 (captureStreamed "sleep 5 & printf done")
        case result of
          Nothing -> assertFailure "streaming waited for a background child"
          Just (failure, output, _) -> do
            failure @?= Right ()
            output @?= BS8.pack "done"
    , testCase "commandResultText appends stderr and exit status in order" $ do
        commandResultText (CommandResult ExitSuccess "out" "err") @?= "outerr"
        commandResultText (CommandResult (ExitFailure 3) "out" "err")
          @?= "outerr\n[exit 3]\n"
    , testCase "commands cannot change the parent working directory" $
        withTemporaryDirectory "hutian-shell-cd" $ \directory -> do
          original <- getCurrentDirectory
          _ <- runShellCommand original ("cd " ++ directory)
          output <- runShellCommandOutput original ("cd " ++ directory)
          changed <- getCurrentDirectory
          case output of
            Left message -> assertFailure message
            Right result -> commandResultText result @?= ""
          changed @?= original
    , testCase "executableCommands includes PATH entries without duplicates" $
        withTemporaryDirectory "hutian-shell-path" $ \directory -> do
          let script = directory </> "test-command"
          writeFile script "#!/bin/sh\n"
          permissions <- getPermissions script
          setPermissions script (permissions { executable = True })
          withEnv "PATH" directory $ do
            commands <- executableCommands
            assertBool "custom command missing" ("test-command" `elem` commands)
            commands @?= ["test-command"]
    , testCase "shellExitSuffix reports only failures" $ do
        shellExitSuffix ExitSuccess @?= ""
        shellExitSuffix (ExitFailure 3) @?= "\n[exit 3]\n"
    ]

shellQuote :: String -> String
shellQuote value =
  "'" ++ concatMap quote value ++ "'"
  where
    quote '\'' = "'\\''"
    quote character = [character]

-- | Capture a streamed command's output in files: the test runner owns the
-- process stdout, so stdout and stderr cannot be captured with pipes here.
captureStreamed :: String -> IO (Either String (), BS.ByteString, BS.ByteString)
captureStreamed command =
  withTemporaryDirectory "hutian-shell-stream" $ \directory -> do
    let outputPath = directory </> "stdout"
        errorPath = directory </> "stderr"
    result <-
      withFile outputPath WriteMode $ \outputHandle ->
        withFile errorPath WriteMode $ \errorHandle ->
          runShellCommandStreaming
            sanitizeOutputBytes
            outputHandle
            errorHandle
            "/"
            command
    output <- BS.readFile outputPath
    errors <- BS.readFile errorPath
    pure (result, output, errors)
