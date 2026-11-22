{-# LANGUAGE OverloadedStrings #-}

-- | Pure configuration for the tools the model may call. Runtime-discovered
-- tools and sessions live in "Hutian.Tool.Catalog".
--
-- The launcher passes one JSON document for the whole tree, including the
-- nested tools of every configured subagent. Keeping the parser here means a
-- malformed configuration is rejected before the shell accepts input.
module Hutian.Tool.Config
  ( ToolConfig (..)
  , SubagentConfig (..)
  , SubagentConfigs (..)
  , McpServerConfig (..)
  , defaultToolConfig
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

-- | One configured MCP server. The JSON configuration is keyed by name; the
-- discovered tools and session live in "Hutian.Tool.Catalog".
data McpServerConfig = McpServerConfig
  { mcpServerName :: String
  , mcpServerUrl :: String
  , mcpServerPolicy :: ToolPolicy
  }
  deriving stock (Eq, Show)

-- | The name-keyed MCP server document used by the launcher.
newtype McpServers = McpServers
  { unMcpServers :: [McpServerConfig]
  }
  deriving stock (Eq, Show)

-- | Every tool policy and configuration the model can use.
data ToolConfig = ToolConfig
  { toolShell :: ToolPolicy
  , toolWebfetch :: ToolPolicy
  , toolSubagent :: [SubagentConfig]
  , toolMcp :: [McpServerConfig]
  }
  deriving stock (Eq, Show)

-- | Safe defaults used by tests, direct callers that only care about the shell
-- policy, and a launcher that does not provide a tools file.
defaultToolConfig :: ToolConfig
defaultToolConfig =
  ToolConfig
    { toolShell = ToolDeny
    , toolWebfetch = ToolDeny
    , toolSubagent = []
    , toolMcp = []
    }

instance ToJSON ToolConfig where
  toJSON config =
    object
      [ "shell" .= toolShell config
      , "webfetch" .= toolWebfetch config
      , "subagent" .= SubagentConfigs (toolSubagent config)
      , "mcp" .= McpServers (toolMcp config)
      ]

instance FromJSON ToolConfig where
  parseJSON = withObject "tools" $ \values -> do
    shell <- policyField values "shell" ToolDeny
    webfetch <- policyField values "webfetch" ToolDeny
    subagent <- values .:? "subagent" .!= SubagentConfigs []
    mcp <- values .:? "mcp" .!= McpServers []
    pure
      ToolConfig
        { toolShell = shell
        , toolWebfetch = webfetch
        , toolSubagent = unSubagentConfigs subagent
        , toolMcp = unMcpServers mcp
        }

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

instance ToJSON McpServerConfig where
  toJSON config =
    object
      [ "url" .= mcpServerUrl config
      , "policy" .= mcpServerPolicy config
      ]

instance ToJSON McpServers where
  toJSON = mcpServersObject . unMcpServers

instance FromJSON McpServers where
  parseJSON = withObject "mcp" $ \servers ->
    McpServers . sortOn mcpServerName
      <$> traverse (uncurry parseMcpServer) (KeyMap.toList servers)

mcpServersObject :: [McpServerConfig] -> Value
mcpServersObject servers =
  Object
    ( KeyMap.fromList
        [ (AesonKey.fromString (mcpServerName server), toJSON server)
        | server <- servers
        ]
    )

parseMcpServer :: AesonKey.Key -> Value -> Parser McpServerConfig
parseMcpServer nameKey = withObject "mcp server" $ \values -> do
  name <- requireName "MCP server" nameKey
  url <- values .: "url" >>= requireNonEmpty ("MCP server " ++ name ++ " URL")
  policy <- values .:? "policy" .!= ToolAsk
  pure
    McpServerConfig
      { mcpServerName = name
      , mcpServerUrl = Text.unpack url
      , mcpServerPolicy = policy
      }

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
  name <- requireName "subagent" nameKey
  model <-
    values .: "model"
      >>= requireNonEmpty ("subagent " ++ name ++ " model")
  description <-
    values .: "description"
      >>= requireNonEmpty ("subagent " ++ name ++ " description")
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

requireNonEmpty :: String -> Text.Text -> Parser Text.Text
requireNonEmpty label text
  | Text.null text = fail (label ++ " must be non-empty")
  | otherwise = pure text

requireName :: String -> AesonKey.Key -> Parser String
requireName label key =
  Text.unpack <$> requireNonEmpty label (AesonKey.toText key)

policyField :: Object -> Text.Text -> ToolPolicy -> Parser ToolPolicy
policyField values key fallback =
  values .:? AesonKey.fromText key .!= fallback
