{-# LANGUAGE OverloadedStrings #-}

-- | Request bodies for the chat and completion endpoints. Building a body is
-- pure; sending it belongs to "Hutian.Llm". The settings record is the single
-- source of the model, system prompt, endpoint, and tools, so a request cannot
-- mix values from different configurations.
module Hutian.Llm.Request
  ( requestBodyFor
  , continuationBodyFor
  ) where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Hutian.Conversation
  ( ChatMessage (..)
  , Conversation
  , ToolCall (..)
  , completionContinuationPrompt
  , completionPrompt
  , continuationMessages
  , requestMessages
  )
import Hutian.Llm.Settings
  ( Api (..)
  , LlmSettings (..)
  , ModelName (..)
  , SystemPrompt (..)
  , apiFor
  )
import Hutian.Tool (toolDefinitionsFor)
import Hutian.Tool.Catalog (ToolCatalog)

requestBodyFor :: LlmSettings -> Conversation -> String -> Aeson.Value
requestBodyFor settings conversation prompt =
  bodyFor settings messages completionText
  where
    system = unSystemPrompt (settingSystemPrompt settings)
    messages = requestMessages system conversation prompt
    completionText = completionPrompt system conversation prompt

-- | Build a follow-up request after tool results, without a new user message.
continuationBodyFor :: LlmSettings -> Conversation -> Aeson.Value
continuationBodyFor settings conversation =
  bodyFor settings messages completionText
  where
    system = unSystemPrompt (settingSystemPrompt settings)
    messages = continuationMessages system conversation
    completionText = completionContinuationPrompt system conversation

-- | The endpoint decides which message shape is sent; the model, tools, and
-- system prompt always come from the same settings value.
bodyFor :: LlmSettings -> [ChatMessage] -> String -> Aeson.Value
bodyFor settings messages completionText =
  case apiFor (settingUrl settings) of
    ChatApi ->
      chatBody (settingTools settings) (settingModel settings) messages
    CompletionApi ->
      completionBody (settingModel settings) completionText

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
