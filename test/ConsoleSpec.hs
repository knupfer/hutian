module ConsoleSpec (tests) where

import Data.List (isInfixOf, isSuffixOf)
import Hutian.Console
  ( ToolApproval (..)
  , approvalForKey
  )
import Hutian.Render
  ( approvalDecisionText
  , approvePromptText
  , boldVibrantPink
  , reset
  )
import System.Console.ANSI (clearFromCursorToScreenEndCode, cursorUpCode)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "console"
    [ testCase "approval accepts only y and n" $ do
        approvalForKey 'y' @?= Just ToolApproved
        approvalForKey 'Y' @?= Just ToolApproved
        approvalForKey 'n' @?= Just ToolDenied
        approvalForKey 'N' @?= Just ToolDenied
    , testCase "other keys are ignored instead of denying" $
        mapM_
          (\key -> approvalForKey key @?= Nothing)
          ['a', 'q', ' ', '\n', '\ESC']
    , testCase "Ctrl+C aborts instead of denying" $
        approvalForKey '\ETX' @?= Just ToolAborted
    , testCase "approval prompt puts the label and choices on separate lines" $ do
        approvePromptText
          @?= boldVibrantPink ++ "approve:" ++ reset ++ "\n[y] Yes\n[n] No\n"
        assertBool "Yes should be on its own line" ("\n[y] Yes\n" `isInfixOf` approvePromptText)
        assertBool "No should be on its own line" ("\n[n] No\n" `isInfixOf` approvePromptText)
    , testCase "a decision replaces both option lines" $ do
        approvalDecisionText "Yes"
          @?= cursorUpCode 2 ++ "\r" ++ clearFromCursorToScreenEndCode ++ "Yes\n"
        assertBool "a negative decision ends with No" ("No\n" `isSuffixOf` approvalDecisionText "No")
    ]
