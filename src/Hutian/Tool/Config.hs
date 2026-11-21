{-# LANGUAGE OverloadedStrings #-}

-- | Configuration for the tools the model may call. Policies are pure values
-- so request building, planning, and execution all use the same data.
--
-- The launcher passes one JSON document for the whole tree, including the
-- nested tools of every configured subagent. Keeping the parser here means a
-- malformed configuration is rejected before the shell accepts input.
module Hutian.Tool.Config
  ( ToolConfig (..)
  , WebsearchConfig (..)
  , SubagentConfig (..)
  , SubagentConfigs (..)
  , defaultToolConfig
  , HasToolConfig (..)
  ) where

import Data.Aeson
  ( FromJSON (..)
  , Object
  , ToJSON (..)
  , Value (..)
  , object
  , withObject
  , (.:)
  , (.:?)
  , (.!=)
  , (.=)
  )
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List (sortOn)
import qualified Data.Text as Text
import Hutian.Policy (ToolPolicy (..))

-- | One configured subagent. The name is the selector the main model uses; the
-- model is the upstream name sent to the shared API. Subagents run serially and
-- only their final answer is returned to the caller.
data SubagentConfig = SubagentConfig
  { subagentName :: String
  , subagentModel :: String
  , subagentDescription :: String
  , subagentSystemPrompt :: String
  , subagentTools :: ToolConfig
  }
  deriving stock (Eq, Show)

-- | The name-keyed subagent document used by the launcher and by
-- @--subagent-file@. Keeping the map in its own type lets the same parser
-- serve both the nested configuration and the standalone file.
newtype SubagentConfigs = SubagentConfigs
  { unSubagentConfigs :: [SubagentConfig]
  }
  deriving stock (Eq, Show)

-- | A search endpoint plus the policy for the model's search tool.
data WebsearchConfig = WebsearchConfig
  { websearchUrl :: String
  , websearchPolicy :: ToolPolicy
  }
  deriving stock (Eq, Show)

-- | Every tool policy and configuration the model can use.
data ToolConfig = ToolConfig
  { toolShell :: ToolPolicy
  , toolWebfetch :: ToolPolicy
  , toolWebsearch :: Maybe WebsearchConfig
  , toolSubagent :: [SubagentConfig]
  }
  deriving stock (Eq, Show)

-- | Safe defaults used by tests, direct callers that only care about the shell
-- policy, and a launcher that does not provide a tools file.
defaultToolConfig :: ToolConfig
defaultToolConfig =
  ToolConfig
    { toolShell = ToolDeny
    , toolWebfetch = ToolDeny
    , toolWebsearch = Nothing
    , toolSubagent = []
    }

-- | Treat a plain policy value as a full configuration with every other tool
-- disabled. This keeps the old shell-only entry points convenient.
class HasToolConfig a where
  toolConfigFor :: a -> ToolConfig

instance HasToolConfig ToolConfig where
  toolConfigFor = id

instance HasToolConfig ToolPolicy where
  toolConfigFor policy = defaultToolConfig {toolShell = policy}

instance ToJSON ToolConfig where
  toJSON config =
    object
      [ "shell" .= toolShell config
      , "webfetch" .= toolWebfetch config
      , "websearch" .= toolWebsearch config
      , "subagent" .= SubagentConfigs (toolSubagent config)
      ]

instance FromJSON ToolConfig where
  parseJSON = withObject "tools" $ \values -> do
    shell <- policyField values "shell" ToolDeny
    webfetch <- policyField values "webfetch" ToolDeny
    websearch <- values .:? "websearch" >>= traverse parseWebsearch
    subagent <- values .:? "subagent" .!= SubagentConfigs []
    pure
      ToolConfig
        { toolShell = shell
        , toolWebfetch = webfetch
        , toolWebsearch = websearch
        , toolSubagent = unSubagentConfigs subagent
        }

instance ToJSON WebsearchConfig where
  toJSON config =
    object
      [ "url" .= websearchUrl config
      , "policy" .= websearchPolicy config
      ]

instance FromJSON WebsearchConfig where
  parseJSON = withObject "websearch" $ \values -> do
    url <- values .: "url" >>= requireNonEmpty "websearch URL"
    policy <- values .:? "policy" .!= ToolAsk
    pure (WebsearchConfig (Text.unpack url) policy)

instance ToJSON SubagentConfig where
  toJSON agent =
    object
      [ "model" .= subagentModel agent
      , "description" .= subagentDescription agent
      , "systemPrompt" .= subagentSystemPrompt agent
      , "tools" .= subagentTools agent
      ]

instance ToJSON SubagentConfigs where
  toJSON = subagentObject . unSubagentConfigs

instance FromJSON SubagentConfigs where
  parseJSON = withObject "subagent" $ \agents ->
    SubagentConfigs . sortOn subagentName
      <$> traverse (uncurry parseSubagent) (KeyMap.toList agents)

subagentObject :: [SubagentConfig] -> Value
subagentObject agents =
  Object
    ( KeyMap.fromList
        [ (AesonKey.fromString (subagentName agent), toJSON agent)
        | agent <- agents
        ]
    )

parseSubagent :: AesonKey.Key -> Value -> Parser SubagentConfig
parseSubagent nameKey = withObject "subagent" $ \values -> do
  name <- requireName (AesonKey.toText nameKey)
  model <- values .: "model" >>= requireField name "model"
  description <- values .: "description" >>= requireField name "description"
  systemPrompt <- values .: "systemPrompt"
  tools <- values .:? "tools" .!= defaultToolConfig
  pure
    SubagentConfig
      { subagentName = name
      , subagentModel = Text.unpack model
      , subagentDescription = Text.unpack description
      , subagentSystemPrompt = Text.unpack systemPrompt
      , subagentTools = tools
      }
  where
    requireName text
      | Text.null text = fail "subagent name must be non-empty"
      | otherwise = pure (Text.unpack text)
    requireField name field text
      | Text.null text =
          fail ("subagent " ++ name ++ " needs a non-empty " ++ field)
      | otherwise = pure text

parseWebsearch :: Value -> Parser WebsearchConfig
parseWebsearch (String url) = do
  checked <- requireNonEmpty "websearch URL" url
  pure (WebsearchConfig (Text.unpack checked) ToolAsk)
parseWebsearch value = parseJSON value

requireNonEmpty :: String -> Text.Text -> Parser Text.Text
requireNonEmpty label text
  | Text.null text = fail (label ++ " must be non-empty")
  | otherwise = pure text

policyField :: Object -> Text.Text -> ToolPolicy -> Parser ToolPolicy
policyField values key fallback =
  values .:? AesonKey.fromText key .!= fallback
