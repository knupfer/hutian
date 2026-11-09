-- | Pure conversation history used to build LLM requests.
module Hutian.Conversation
  ( Conversation
  , ConversationEntry (..)
  , ChatMessage (..)
  , emptyConversation
  , appendExchange
  , appendTerminal
  , requestMessages
  , completionPrompt
  ) where

import Data.Char (isSpace)
import Data.List (intercalate)

type Conversation = [ConversationEntry]

data ConversationEntry
  = UserEntry String
  | AssistantEntry String
  | TerminalEntry String
  deriving stock (Eq, Show)

data ChatMessage = ChatMessage
  { chatRole :: String
  , chatContent :: String
  }
  deriving stock (Eq, Show)

emptyConversation :: Conversation
emptyConversation = []

appendExchange :: String -> Maybe String -> Conversation -> Conversation
appendExchange prompt (Just answer) conversation
  | not (all isSpace answer) =
      conversation ++ [UserEntry prompt, AssistantEntry answer]
appendExchange _ _ conversation = conversation

appendTerminal :: String -> Conversation -> Conversation
appendTerminal command conversation =
  conversation ++ [TerminalEntry command]

-- | Build the message list for an OpenAI-compatible chat request.
requestMessages :: String -> Conversation -> String -> [ChatMessage]
requestMessages systemPrompt conversation prompt =
  systemMessages ++ map entryMessage conversation ++ [ChatMessage "user" prompt]
  where
    systemMessages
      | null systemPrompt = []
      | otherwise = [ChatMessage "system" systemPrompt]

entryMessage :: ConversationEntry -> ChatMessage
entryMessage entry =
  case entry of
    UserEntry text -> ChatMessage "user" text
    AssistantEntry text -> ChatMessage "assistant" text
    TerminalEntry command -> ChatMessage "user" (terminalMessage command)

terminalMessage :: String -> String
terminalMessage command =
  "The user ran this terminal command:\n" ++ command

-- | Flatten the same conversation into a prompt for the native completion API.
completionPrompt :: String -> Conversation -> String -> String
completionPrompt systemPrompt conversation prompt =
  intercalate "\n\n" (map renderMessage (requestMessages systemPrompt conversation prompt))
    ++ "\n\nAssistant:"
  where
    renderMessage message =
      case chatRole message of
        "system" -> "System: " ++ chatContent message
        "assistant" -> "Assistant: " ++ chatContent message
        _ -> "User: " ++ chatContent message