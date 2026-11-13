module Main (main) where

import qualified CommandSpec
import qualified CompletionSpec
import qualified ConsoleSpec
import qualified ConversationSpec
import qualified EditorSpec
import qualified HistorySpec
import qualified InterruptSpec
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
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "hutian-shell"
      [ CommandSpec.tests
      , CompletionSpec.tests
      , ConsoleSpec.tests
      , ConversationSpec.tests
      , EditorSpec.tests
      , HistorySpec.tests
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
      , InterruptSpec.tests
      ]
