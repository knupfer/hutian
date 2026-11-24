{-# LANGUAGE OverloadedStrings #-}

-- | Runtime tool catalog. The pure configuration in "Hutian.Tool.Config" is
-- combined with the MCP tools and sessions discovered before a model turn.
--
-- The catalog is the single source of truth after construction: MCP server
-- configuration and discovery state live together in 'McpServer', so the two
-- cannot drift apart.
module Hutian.Tool.Catalog
  ( ToolCatalog
  , catalogShell
  , catalogWebfetch
  , catalogSubagents
  , catalogMcp
  , toolCatalog
  , withMcpServers
  , shellOnlyCatalog
  , McpServer (..)
  , mcpServerNameOf
  , mcpServerPolicyOf
  , mcpServerHandlersOf
  , McpSessionId
  , mcpSessionIdFromString
  , mcpSessionIdText
  , McpTool (..)
  ) where

import Data.Aeson
  ( FromJSON (..)
  , Value
  , object
  , withObject
  , (.:)
  , (.:?)
  , (.!=)
  , (.=)
  )
import qualified Data.Text as Text
import Hutian.Policy (ToolPolicy (..))
import Hutian.Tool.Config
  ( HandlerConfigs
  , McpServerConfig (..)
  , SubagentConfig
  , ToolConfig (..)
  , defaultToolConfig
  )

-- | A server keeps its immutable configuration next to the discovery result.
data McpServer = McpServer
  { mcpServerConfig :: McpServerConfig
  , mcpServerTools :: [McpTool]
  , mcpServerSession :: Maybe McpSessionId
  }
  deriving stock (Eq, Show)

mcpServerNameOf :: McpServer -> String
mcpServerNameOf = mcpServerName . mcpServerConfig

mcpServerPolicyOf :: McpServer -> ToolPolicy
mcpServerPolicyOf = mcpServerPolicy . mcpServerConfig

mcpServerHandlersOf :: McpServer -> HandlerConfigs
mcpServerHandlersOf = mcpHandlers . mcpServerConfig

-- | The opaque session identifier returned by an MCP server.
newtype McpSessionId = McpSessionId
  { unMcpSessionId :: String
  }
  deriving stock (Eq, Show)

mcpSessionIdFromString :: String -> McpSessionId
mcpSessionIdFromString = McpSessionId

mcpSessionIdText :: McpSessionId -> String
mcpSessionIdText = unMcpSessionId

-- | One tool advertised by an MCP server.
data McpTool = McpTool
  { mcpToolName :: String
  , mcpToolDescription :: String
  , mcpToolInputSchema :: Value
  }
  deriving stock (Eq, Show)

instance FromJSON McpTool where
  parseJSON = withObject "MCP tool" $ \values -> do
    name <- values .: "name"
    description <- values .:? "description" .!= ("" :: Text.Text)
    schema <-
      values .:? "inputSchema"
        .!= object ["type" .= ("object" :: String)]
    pure
      McpTool
        { mcpToolName = Text.unpack name
        , mcpToolDescription = Text.unpack description
        , mcpToolInputSchema = schema
        }

-- | The configuration plus the runtime MCP state used to build requests and
-- plans.
data ToolCatalog = ToolCatalog
  { catalogShell :: ToolPolicy
  , catalogWebfetch :: ToolPolicy
  , catalogSubagents :: [SubagentConfig]
  , catalogMcp :: [McpServer]
  }
  deriving stock (Eq, Show)

-- | Build a catalog whose MCP servers are configured but not yet discovered.
toolCatalog :: ToolConfig -> ToolCatalog
toolCatalog config =
  ToolCatalog
    { catalogShell = toolShell config
    , catalogWebfetch = toolWebfetch config
    , catalogSubagents = toolSubagent config
    , catalogMcp = [McpServer server [] Nothing | server <- toolMcp config]
    }

-- | Replace the configured servers with their discovery results.
withMcpServers :: [McpServer] -> ToolCatalog -> ToolCatalog
withMcpServers servers catalog = catalog {catalogMcp = servers}

-- | A catalog with only a shell policy. Used by callers that have no full tool
-- configuration, such as the shell-only executor and request-body tests.
shellOnlyCatalog :: ToolPolicy -> ToolCatalog
shellOnlyCatalog policy = toolCatalog (defaultToolConfig {toolShell = policy})
