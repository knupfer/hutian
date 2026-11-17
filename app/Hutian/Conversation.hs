-- | Pure conversation history used to build LLM requests.
module Hutian.Conversation
  ( Conversation
  , ConversationEntry (..)
  , ToolCall (..)
  , ChatMessage (..)
  , emptyConversation
  , appendExchange
  , appendAssistant
  , appendUser
  , appendTerminal
  , appendAssistantToolCalls
  , appendToolResult
  , appendResult
  , requestMessages
  , continuationMessages
  , completionPrompt
  , completionContinuationPrompt
  ) where

import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

type Conversation = [ConversationEntry]

data ConversationEntry
  = UserEntry String
  | AssistantEntry String
  | TerminalEntry String
  | AssistantToolCallEntry String [ToolCall]
  | ToolResultEntry String String
  deriving stock (Eq, Show)

-- | A tool call requested by the assistant. The arguments are the raw JSON
-- string sent by the model, so the conversation can replay it unchanged.
data ToolCall = ToolCall
  { toolCallId :: String
  , toolCallName :: String
  , toolCallArguments :: String
  }
  deriving stock (Eq, Show)

-- | A message in an OpenAI-compatible chat request. The constructors keep
-- role-specific fields together, so a plain message cannot accidentally carry
-- tool metadata.
data ChatMessage
  = SystemMessage String
  | UserMessage String
  | AssistantMessage String
  | AssistantToolCallMessage String [ToolCall]
  | ToolResultMessage String String
  deriving stock (Eq, Show)

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

{-@ appendAssistant :: answer:String -> conversation:Conversation ->
      {v:Conversation | len v >= len conversation && len v <= len conversation + 1} @-}
appendAssistant :: String -> Conversation -> Conversation
appendAssistant answer conversation
  | all isSpace answer = conversation
  | otherwise = conversation ++ [AssistantEntry answer]

{-@ appendUser :: prompt:String -> conversation:Conversation ->
      {v:Conversation | len v == len conversation + 1} @-}
appendUser :: String -> Conversation -> Conversation
appendUser prompt conversation = conversation ++ [UserEntry prompt]

{-@ appendAssistantToolCalls :: text:String -> calls:[ToolCall] -> conversation:Conversation ->
      {v:Conversation | len v == len conversation + 1} @-}
appendAssistantToolCalls :: String -> [ToolCall] -> Conversation -> Conversation
appendAssistantToolCalls text calls conversation =
  conversation ++ [AssistantToolCallEntry text calls]

{-@ appendToolResult :: callId:String -> output:String -> conversation:Conversation ->
      {v:Conversation | len v == len conversation + 1} @-}
appendToolResult :: String -> String -> Conversation -> Conversation
appendToolResult callId output conversation =
  conversation ++ [ToolResultEntry callId output]

-- | Record one model result. The prompt is present for the first request and
-- absent for follow-up requests after tool results.
{-@ appendResult :: prompt:Maybe String -> answer:Maybe String -> calls:[ToolCall] -> conversation:Conversation ->
      {v:Conversation | len v >= len conversation && len v <= len conversation + 2} @-}
appendResult :: Maybe String -> Maybe String -> [ToolCall] -> Conversation -> Conversation
appendResult prompt answer calls conversation =
  case calls of
    [] ->
      case prompt of
        Just text -> appendExchange text answer conversation
        Nothing -> appendAssistant (fromMaybe "" answer) conversation
    _ ->
      appendAssistantToolCalls (fromMaybe "" answer) calls withPrompt
  where
    withPrompt = maybe conversation (`appendUser` conversation) prompt

-- | Build the message list for an OpenAI-compatible chat request.
{-@ requestMessages :: systemPrompt:String -> conversation:Conversation -> prompt:String ->
      {v:[ChatMessage] | len v == len conversation + 1 + (if len systemPrompt == 0 then 0 else 1)} @-}
requestMessages :: String -> Conversation -> String -> [ChatMessage]
requestMessages systemPrompt conversation prompt =
  continuationMessages systemPrompt conversation ++ [UserMessage prompt]

-- | Build the message list for a follow-up request after tool results. The
-- conversation already ends with the messages that the model needs to see.
{-@ continuationMessages :: systemPrompt:String -> conversation:Conversation ->
      {v:[ChatMessage] | len v == len conversation + (if len systemPrompt == 0 then 0 else 1)} @-}
continuationMessages :: String -> Conversation -> [ChatMessage]
continuationMessages systemPrompt conversation =
  systemMessages ++ map entryMessage conversation
  where
    systemMessages
      | null systemPrompt = []
      | otherwise = [SystemMessage systemPrompt]

entryMessage :: ConversationEntry -> ChatMessage
entryMessage entry =
  case entry of
    UserEntry text -> UserMessage text
    AssistantEntry text -> AssistantMessage text
    TerminalEntry command -> UserMessage (terminalMessage command)
    AssistantToolCallEntry text calls -> AssistantToolCallMessage text calls
    ToolResultEntry callId output -> ToolResultMessage callId output

{-@ terminalMessage :: command:String -> {v:String | len v == len command + 36} @-}
terminalMessage :: String -> String
terminalMessage command =
  "The user ran this terminal command:\n" ++ command

-- | Flatten the same conversation into a prompt for the native completion API.
{-@ completionPrompt :: systemPrompt:String -> conversation:Conversation -> prompt:String ->
      {v:String | len v >= 12} @-}
completionPrompt :: String -> Conversation -> String -> String
completionPrompt systemPrompt conversation prompt =
  renderMessages (requestMessages systemPrompt conversation prompt)

-- | Flatten a tool follow-up into a prompt for the native completion API.
{-@ completionContinuationPrompt :: systemPrompt:String -> conversation:Conversation ->
      {v:String | len v >= 12} @-}
completionContinuationPrompt :: String -> Conversation -> String
completionContinuationPrompt systemPrompt conversation =
  renderMessages (continuationMessages systemPrompt conversation)

renderMessages :: [ChatMessage] -> String
renderMessages messages =
  intercalate "\n\n" (map renderMessage messages) ++ "\n\nAssistant:"
  where
    renderMessage message =
      case message of
        SystemMessage text -> labelled "System" text
        UserMessage text -> labelled "User" text
        AssistantMessage text -> labelled "Assistant" text
        AssistantToolCallMessage text _ -> labelled "Assistant" text
        ToolResultMessage _ output -> labelled "Tool" output
    labelled role text = role ++ ": " ++ text
