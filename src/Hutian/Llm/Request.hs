{-# LANGUAGE OverloadedStrings #-}

-- | Request bodies for the chat and completion endpoints. Building a body is
-- pure; sending it belongs to "Hutian.Llm".
module Hutian.Llm.Request
  ( Api (..)
  , apiFor
  , requestBodyFor
  , continuationBodyFor
  ) where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.List (isSuffixOf)
import Hutian.Conversation
  ( ChatMessage (..)
  , Conversation
  , ToolCall (..)
  , completionContinuationPrompt
  , completionPrompt
  , continuationMessages
  , requestMessages
  )
import Hutian.Llm.Settings (LlmUrl (..), ModelName (..), SystemPrompt (..), urlPath)
import Hutian.Policy (ModelShell (..), modelShellAdvertised)
import Hutian.Tool (toolDefinitions)

data Api
  = ChatApi
  | CompletionApi
  deriving stock (Eq, Show)

apiFor :: LlmUrl -> Api
apiFor url
  | "/v1/chat/completions" `isSuffixOf` urlPath url = ChatApi
  | otherwise = CompletionApi

requestBodyFor :: ModelShell -> Api -> ModelName -> SystemPrompt -> Conversation -> String -> Aeson.Value
requestBodyFor policy api model systemPrompt conversation prompt =
  case api of
    ChatApi -> chatBody policy model (requestMessages (unSystemPrompt systemPrompt) conversation prompt)
    CompletionApi ->
      object
        [ "model" .= unModelName model
        , "prompt" .= completionPrompt (unSystemPrompt systemPrompt) conversation prompt
        , "stream" .= True
        ]

-- | Build a follow-up request after tool results, without a new user message.
continuationBodyFor :: ModelShell -> Api -> ModelName -> SystemPrompt -> Conversation -> Aeson.Value
continuationBodyFor policy api model systemPrompt conversation =
  case api of
    ChatApi -> chatBody policy model (continuationMessages (unSystemPrompt systemPrompt) conversation)
    CompletionApi ->
      object
        [ "model" .= unModelName model
        , "prompt" .= completionContinuationPrompt (unSystemPrompt systemPrompt) conversation
        , "stream" .= True
        ]

chatBody :: ModelShell -> ModelName -> [ChatMessage] -> Aeson.Value
chatBody policy model messages =
  object $
    [ "model" .= unModelName model
    , "messages" .= map messageValue messages
    , "stream" .= True
    ]
      ++ toolFields
  where
    toolFields
      | modelShellAdvertised policy =
          [ "tools" .= toolDefinitions
          , "tool_choice" .= ("auto" :: String)
          ]
      | otherwise = []

messageValue :: ChatMessage -> Aeson.Value
messageValue message =
  object $
    case message of
      SystemMessage text -> roleMessage "system" text
      UserMessage text -> roleMessage "user" text
      AssistantMessage text -> roleMessage "assistant" text
      AssistantToolCallMessage text calls ->
        roleMessage "assistant" text
          ++ [ "tool_calls" .= map toolCallValue calls
             | not (null calls)
             ]
      ToolResultMessage callId output ->
        roleMessage "tool" output
          ++ ["tool_call_id" .= callId]
  where
    roleMessage role text =
      [ "role" .= (role :: String)
      , "content" .= text
      ]

toolCallValue :: ToolCall -> Aeson.Value
toolCallValue call =
  object
    [ "id" .= toolCallId call
    , "type" .= ("function" :: String)
    , "function" .=
        object
          [ "name" .= toolCallName call
          , "arguments" .= toolCallArguments call
          ]
    ]
