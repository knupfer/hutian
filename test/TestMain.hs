module Main (main) where

import qualified CliSpec
import qualified CommandSpec
import qualified CompletionSpec
import qualified ConsoleSpec
import qualified ConversationSpec
import Data.List (isPrefixOf)
import qualified EditorSpec
import qualified HistorySpec
import qualified HighlightSpec
import qualified InterruptSpec
import qualified JobsSpec
import qualified LlmIntegrationSpec
import qualified LlmLoopSpec
import qualified LlmPresentationSpec
import qualified LlmSocketSpec
import qualified LlmSpec
import qualified LlmStreamSpec
import qualified MarkdownSpec
import qualified PathsSpec
import qualified PolicySpec
import qualified PromptSpec
import qualified RelaySpec
import qualified RenderSpec
import qualified SessionSpec
import qualified ShellSpec
import System.Environment (getArgs, withArgs)
import Test.Tasty (testGroup)
import TestReporter (bufferedMain)
import qualified ToolSpec
import qualified WireSpec

-- The report is buffered until the last test finished because some suites
-- redirect the process-wide stdout and stderr. The tests also share process
-- state such as signal handlers, so they run one at a time.
main :: IO ()
main = do
  args <- getArgs
  let singleThreaded
        | any (isPrefixOf "--num-threads") args = args
        | any (isPrefixOf "-j") args = args
        | otherwise = args ++ ["--num-threads=1"]
  withArgs singleThreaded $
    bufferedMain $
      testGroup
        "hutian-shell"
        [ CliSpec.tests
        , CommandSpec.tests
        , CompletionSpec.tests
        , ConsoleSpec.tests
        , ConversationSpec.tests
        , EditorSpec.tests
        , HistorySpec.tests
        , HighlightSpec.tests
        , JobsSpec.tests
        , LlmSpec.tests
        , LlmIntegrationSpec.tests
        , LlmLoopSpec.tests
        , LlmSocketSpec.tests
        , LlmStreamSpec.tests
        , LlmPresentationSpec.tests
        , MarkdownSpec.tests
        , PathsSpec.tests
        , PolicySpec.tests
        , PromptSpec.tests
        , RelaySpec.tests
        , RenderSpec.tests
        , SessionSpec.tests
        , ShellSpec.tests
        , ToolSpec.tests
        , WireSpec.tests
        , InterruptSpec.tests
        ]
