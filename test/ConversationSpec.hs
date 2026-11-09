module ConversationSpec (tests) where

import Data.Char (isSpace)
import Data.List (isInfixOf)
import Hutian.Conversation
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "conversation"
    [ unitTests
    , propertyTests
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "unit"
    [ testCase "request messages include system, history, and current prompt" $ do
        let conversation =
              [ UserEntry "first question"
              , AssistantEntry "first answer"
              , TerminalEntry "git status"
              ]
            messages = requestMessages "system rules" conversation "second question"
        map chatRole messages @?= [SystemRole, UserRole, AssistantRole, UserRole, UserRole]
        map chatContent messages @?=
          [ "system rules"
          , "first question"
          , "first answer"
          , "The user ran this terminal command:\ngit status"
          , "second question"
          ]
    , testCase "empty system prompts are omitted" $ do
        map chatRole (requestMessages "" emptyConversation "hello") @?= [UserRole]
    , testCase "appendExchange records successful answers only" $ do
        appendExchange "question" (Just "answer") emptyConversation
          @?= [UserEntry "question", AssistantEntry "answer"]
        appendExchange "question" (Just "   ") emptyConversation @?= []
        appendExchange "question" Nothing emptyConversation @?= []
    , testCase "appendTerminal records terminal commands" $ do
        appendTerminal "ls -la" emptyConversation @?= [TerminalEntry "ls -la"]
    , testCase "completion prompts flatten the entire conversation" $ do
        let prompt =
              completionPrompt
                "system rules"
                [UserEntry "first", AssistantEntry "answer", TerminalEntry "ls"]
                "next"
        assertBool "system missing" ("System: system rules" `isInfixOf` prompt)
        assertBool "user history missing" ("User: first" `isInfixOf` prompt)
        assertBool "assistant history missing" ("Assistant: answer" `isInfixOf` prompt)
        assertBool "terminal history missing" ("git status" `isInfixOf` prompt || "ls" `isInfixOf` prompt)
        assertBool "current prompt missing" ("User: next" `isInfixOf` prompt)
        assertBool "assistant marker missing" ("Assistant:" `isInfixOf` prompt)
    ]

propertyTests :: TestTree
propertyTests =
  testGroup
    "properties"
    [ testProperty "the current prompt is always the final message" prop_promptLast
    , testProperty "message count matches the conversation" prop_messageCount
    , testProperty "all conversation text is retained" prop_historyRetained
    , testProperty "successful exchanges append both messages" prop_exchangeAppended
    ]

newtype Conv = Conv [ConversationEntry]
  deriving stock (Show)

instance Arbitrary Conv where
  arbitrary = Conv <$> listOf arbitraryEntry
    where
      arbitraryEntry =
        oneof
          [ UserEntry <$> arbitrary
          , AssistantEntry <$> arbitrary
          , TerminalEntry <$> arbitrary
          ]

prop_promptLast :: String -> Conv -> String -> Property
prop_promptLast systemPrompt (Conv conversation) prompt =
  case reverse (requestMessages systemPrompt conversation prompt) of
    ChatMessage role content : _ ->
      counterexample (show (role, content)) $
        property (role == UserRole && content == prompt)
    [] -> counterexample "empty message list" False

prop_messageCount :: String -> Conv -> String -> Property
prop_messageCount systemPrompt (Conv conversation) prompt =
  let expected =
        length conversation
          + 1
          + if null systemPrompt then 0 else 1
   in length (requestMessages systemPrompt conversation prompt) === expected

prop_historyRetained :: String -> Conv -> String -> Property
prop_historyRetained systemPrompt (Conv conversation) prompt =
  let rendered = completionPrompt systemPrompt conversation prompt
      contents =
        prompt
          : systemPrompt
          : concatMap entryTexts conversation
   in conjoin
        [ counterexample ("missing: " ++ text) $
            property (null text || text `isInfixOf` rendered)
        | text <- contents
        ]

prop_exchangeAppended :: String -> String -> Property
prop_exchangeAppended prompt answer =
  let conversation = appendExchange prompt (Just answer) emptyConversation
   in if all isSpace answer
        then conversation === []
        else conversation === [UserEntry prompt, AssistantEntry answer]

entryTexts :: ConversationEntry -> [String]
entryTexts entry =
  case entry of
    UserEntry text -> [text]
    AssistantEntry text -> [text]
    TerminalEntry command -> [command]