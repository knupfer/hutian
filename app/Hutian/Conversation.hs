-- | Pure conversation history used to build LLM requests.
module Hutian.Conversation
  ( Conversation
  , ConversationEntry (..)
  , ChatMessage (..)
  , ChatRole (..)
  , roleName
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

-- | The role of a message in an OpenAI-compatible chat request.
data ChatRole
  = SystemRole
  | UserRole
  | AssistantRole
  deriving stock (Eq, Show)

data ChatMessage = ChatMessage
  { chatRole :: ChatRole
  , chatContent :: String
  }
  deriving stock (Eq, Show)

roleName :: ChatRole -> String
roleName SystemRole = "system"
roleName UserRole = "user"
roleName AssistantRole = "assistant"

{-@ emptyConversation :: {v:Conversation | len v == 0} @-}
emptyConversation :: Conversation
emptyConversation = []

{-@ appendTerminal :: command:String -> conversation:Conversation -> {v:Conversation | len v == len conversation + 1} @-}
appendTerminal :: String -> Conversation -> Conversation
appendTerminal command conversation =
  conversation ++ [TerminalEntry command]

{-@ appendExchange :: prompt:String -> answer:Maybe String -> conversation:Conversation ->
      {v:Conversation | len v >= len conversation && len v <= len conversation + 2} @-}
appendExchange :: String -> Maybe String -> Conversation -> Conversation
appendExchange prompt (Just answer) conversation
  | not (all isSpace answer) =
      conversation ++ [UserEntry prompt, AssistantEntry answer]
appendExchange _ _ conversation = conversation

-- | Build the message list for an OpenAI-compatible chat request.
{-@ requestMessages :: systemPrompt:String -> conversation:Conversation -> prompt:String ->
      {v:[ChatMessage] | len v >= len conversation + 1} @-}
requestMessages :: String -> Conversation -> String -> [ChatMessage]
requestMessages systemPrompt conversation prompt =
  systemMessages ++ map entryMessage conversation ++ [ChatMessage UserRole prompt]
  where
    systemMessages
      | null systemPrompt = []
      | otherwise = [ChatMessage SystemRole systemPrompt]

entryMessage :: ConversationEntry -> ChatMessage
entryMessage entry =
  case entry of
    UserEntry text -> ChatMessage UserRole text
    AssistantEntry text -> ChatMessage AssistantRole text
    TerminalEntry command -> ChatMessage UserRole (terminalMessage command)

{-@ terminalMessage :: command:String -> {v:String | len v >= len command} @-}
terminalMessage :: String -> String
terminalMessage command =
  "The user ran this terminal command:\n" ++ command

-- | Flatten the same conversation into a prompt for the native completion API.
{-@ completionPrompt :: systemPrompt:String -> conversation:Conversation -> prompt:String ->
      {v:String | len v >= 12} @-}
completionPrompt :: String -> Conversation -> String -> String
completionPrompt systemPrompt conversation prompt =
  intercalate "\n\n" (map renderMessage (requestMessages systemPrompt conversation prompt))
    ++ "\n\nAssistant:"
  where
    renderMessage message =
      case chatRole message of
        SystemRole -> "System: " ++ chatContent message
        AssistantRole -> "Assistant: " ++ chatContent message
        UserRole -> "User: " ++ chatContent message
