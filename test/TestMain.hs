module Main (main) where

import qualified CliSpec
import qualified CommandSpec
import qualified CompletionSpec
import qualified ConsoleSpec
import qualified ConversationSpec
import qualified EditorSpec
import qualified HistorySpec
import qualified InterruptSpec
import qualified JobsSpec
import qualified LlmIntegrationSpec
import qualified LlmProtocolSpec
import qualified LlmSocketSpec
import qualified LlmSpec
import qualified MarkdownSpec
import qualified PathsSpec
import qualified PolicySpec
import qualified PromptSpec
import qualified RelaySpec
import qualified RenderSpec
import qualified ShellSpec
import qualified SyntaxSpec
import qualified ToolSpec
import qualified WireSpec
import Test.Tasty
import Test.Tasty.Runners (NumThreads (..))

main :: IO ()
main =
  defaultMain $
    localOption (NumThreads 1) $
      testGroup
        "hutian-shell"
        [ CliSpec.tests
        , CommandSpec.tests
        , CompletionSpec.tests
        , ConsoleSpec.tests
        , ConversationSpec.tests
        , EditorSpec.tests
        , HistorySpec.tests
        , JobsSpec.tests
        , LlmSpec.tests
        , LlmIntegrationSpec.tests
        , LlmSocketSpec.tests
        , LlmProtocolSpec.tests
        , MarkdownSpec.tests
        , PathsSpec.tests
        , PolicySpec.tests
        , PromptSpec.tests
        , RelaySpec.tests
        , RenderSpec.tests
        , ShellSpec.tests
        , SyntaxSpec.tests
        , ToolSpec.tests
        , WireSpec.tests
        , InterruptSpec.tests
        ]
