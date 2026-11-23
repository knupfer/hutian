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
import Hutian.Tool (toolDefinitionsFor)
import Hutian.Tool.Catalog (HasToolCatalog, ToolCatalog, toolCatalogFor)

data Api
  = ChatApi
  | CompletionApi
  deriving stock (Eq, Show)

apiFor :: LlmUrl -> Api
apiFor url
  | "/v1/chat/completions" `isSuffixOf` urlPath url = ChatApi
  | otherwise = CompletionApi

requestBodyFor :: HasToolCatalog tools => tools -> Api -> ModelName -> SystemPrompt -> Conversation -> String -> Aeson.Value
requestBodyFor tools api model systemPrompt conversation prompt =
  case api of
    ChatApi -> chatBody (toolCatalogFor tools) model (requestMessages (unSystemPrompt systemPrompt) conversation prompt)
    CompletionApi ->
      completionBody
        model
        (completionPrompt (unSystemPrompt systemPrompt) conversation prompt)

-- | Build a follow-up request after tool results, without a new user message.
continuationBodyFor :: HasToolCatalog tools => tools -> Api -> ModelName -> SystemPrompt -> Conversation -> Aeson.Value
continuationBodyFor tools api model systemPrompt conversation =
  case api of
    ChatApi -> chatBody (toolCatalogFor tools) model (continuationMessages (unSystemPrompt systemPrompt) conversation)
    CompletionApi ->
      completionBody
        model
        (completionContinuationPrompt (unSystemPrompt systemPrompt) conversation)

completionBody :: ModelName -> String -> Aeson.Value
completionBody model prompt =
  object
    [ "model" .= unModelName model
    , "prompt" .= prompt
    , "stream" .= True
    ]

chatBody :: ToolCatalog -> ModelName -> [ChatMessage] -> Aeson.Value
chatBody catalog model messages =
  object $
    [ "model" .= unModelName model
    , "messages" .= map messageValue messages
    , "stream" .= True
    ]
      ++ toolFields
  where
    definitions = toolDefinitionsFor catalog
    toolFields
      | null definitions = []
      | otherwise =
          [ "tools" .= definitions
          , "tool_choice" .= ("auto" :: String)
          ]

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
