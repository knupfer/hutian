-- | Runtime tool catalog. The pure configuration in "Hutian.Tool.Config" is
-- paired with the MCP tools and sessions discovered before a model turn.
module Hutian.Tool.Catalog
  ( ToolCatalog (..)
  , toolCatalog
  , McpServer (..)
  , McpSessionId (..)
  , McpTool (..)
  , HasToolCatalog (toolCatalogFor)
  ) where

import Data.Aeson (Value)
import Hutian.Policy (ToolPolicy (..))
import Hutian.Tool.Config
  ( McpServerConfig
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

-- | The opaque session identifier returned by an MCP server.
newtype McpSessionId = McpSessionId
  { unMcpSessionId :: String
  }
  deriving stock (Eq, Show)

-- | One tool advertised by an MCP server.
data McpTool = McpTool
  { mcpToolName :: String
  , mcpToolDescription :: String
  , mcpToolInputSchema :: Value
  }
  deriving stock (Eq, Show)

-- | The configuration plus the runtime MCP state used to build requests and
-- plans. Keeping this separate from 'ToolConfig' means the JSON configuration
-- never contains sessions or discovered schemas.
data ToolCatalog = ToolCatalog
  { catalogConfig :: ToolConfig
  , catalogMcp :: [McpServer]
  }
  deriving stock (Eq, Show)

-- | A catalog with no discovered MCP tools.
toolCatalog :: ToolConfig -> ToolCatalog
toolCatalog config = ToolCatalog config []

-- | Callers that only need a policy or a configuration still get a catalog.
class HasToolCatalog a where
  toolCatalogFor :: a -> ToolCatalog

instance HasToolCatalog ToolCatalog where
  toolCatalogFor = id

instance HasToolCatalog ToolConfig where
  toolCatalogFor = toolCatalog

instance HasToolCatalog ToolPolicy where
  toolCatalogFor policy = toolCatalog (defaultToolConfig {toolShell = policy})
