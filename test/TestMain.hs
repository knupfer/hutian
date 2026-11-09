module Main (main) where

import qualified CommandSpec
import qualified CompletionSpec
import qualified ConversationSpec
import qualified EditorSpec
import qualified HistorySpec
import qualified InterruptSpec
import qualified LlmIntegrationSpec
import qualified LlmSpec
import qualified MarkdownSpec
import qualified PathsSpec
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
      , ConversationSpec.tests
      , EditorSpec.tests
      , HistorySpec.tests
      , LlmSpec.tests
      , LlmIntegrationSpec.tests
      , MarkdownSpec.tests
      , PathsSpec.tests
      , RenderSpec.tests
      , ShellSpec.tests
      , InterruptSpec.tests
      ]