{-# LANGUAGE OverloadedStrings #-}

-- | Pure configuration for the tools the model may call. Runtime-discovered
-- tools and sessions live in "Hutian.Tool.Catalog".
--
-- The launcher passes one JSON document for the whole tree, including the
-- nested tools of every configured subagent and result handler. The parser is
-- as strict as the flake schema: every field is required and unknown keys are
-- rejected, so a malformed configuration is reported before the shell accepts
-- input instead of silently disabling a tool.
module Hutian.Tool.Config
  ( ToolConfig (..)
  , AgentConfig (..)
  , SubagentConfig (..)
  , SubagentConfigs (..)
  , HandlerConfig
  , HandlerConfigs (..)
  , handlerConfigFor
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
  , (.=)
  )
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List (intercalate, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Hutian.Policy (ToolPolicy (..))

-- | The settings shared by a subagent and a tool-result handler.
data AgentConfig = AgentConfig
  { agentModel :: String
  , agentDescription :: String
  , agentSystemPrompt :: String
  , agentTools :: ToolConfig
  }
  deriving stock (Eq, Show)

-- | One configured subagent. The name is the selector the main model uses; the
-- model is the upstream name sent to the shared API. Subagents run serially and
-- only their final answer is returned to the caller.
data SubagentConfig = SubagentConfig
  { subagentName :: String
  , subagentAgent :: AgentConfig
  }
  deriving stock (Eq, Show)

-- | The name-keyed subagent document used by the launcher and by
-- @--subagent-file@. Keeping the map in its own type lets the same parser
-- serve both the nested configuration and the standalone file.
newtype SubagentConfigs = SubagentConfigs
  { unSubagentConfigs :: [SubagentConfig]
  }
  deriving stock (Eq, Show)

-- | A tool-result handler has the same settings as a subagent but no selector
-- name: the tool name and the launcher derive its label. Only concrete MCP
-- tools can attach a handler.
type HandlerConfig = AgentConfig

-- | The tool-keyed result-handler document. A handler may have its own handlers
-- nested in its tools; the parent handler is never inherited.
newtype HandlerConfigs = HandlerConfigs
  { unHandlerConfigs :: Map.Map String HandlerConfig
  }
  deriving stock (Eq, Show)

-- | Look up a result handler by its local MCP tool name.
handlerConfigFor :: String -> HandlerConfigs -> Maybe HandlerConfig
handlerConfigFor name (HandlerConfigs handlers) = Map.lookup name handlers

-- | One configured MCP server. The JSON configuration is keyed by name; the
-- discovered tools and session live in "Hutian.Tool.Catalog".
data McpServerConfig = McpServerConfig
  { mcpServerName :: String
  , mcpServerUrl :: String
  , mcpServerPolicy :: ToolPolicy
  , mcpHandlers :: HandlerConfigs
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
-- policy, and a launcher that constructs a configuration from flags.
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
    requireKnownKeys "tools" ["shell", "webfetch", "subagent", "mcp"] values
    shell <- values .: "shell"
    webfetch <- values .: "webfetch"
    subagent <- values .: "subagent"
    mcp <- values .: "mcp"
    pure
      ToolConfig
        { toolShell = shell
        , toolWebfetch = webfetch
        , toolSubagent = unSubagentConfigs subagent
        , toolMcp = unMcpServers mcp
        }

instance ToJSON AgentConfig where
  toJSON config =
    object
      [ "model" .= agentModel config
      , "description" .= agentDescription config
      , "systemPrompt" .= agentSystemPrompt config
      , "tools" .= agentTools config
      ]

instance ToJSON SubagentConfig where
  toJSON agent = toJSON (subagentAgent agent)

instance ToJSON SubagentConfigs where
  toJSON = subagentObject . unSubagentConfigs

instance FromJSON SubagentConfigs where
  parseJSON = withObject "subagent" $ \agents ->
    SubagentConfigs . sortOn subagentName
      <$> traverse (uncurry parseSubagent) (KeyMap.toList agents)

instance ToJSON HandlerConfigs where
  toJSON = handlersObject . Map.toList . unHandlerConfigs

instance FromJSON HandlerConfigs where
  parseJSON = withObject "handlers" $ \handlers ->
    HandlerConfigs . Map.fromList
      <$> traverse parseEntry (KeyMap.toList handlers)
    where
      parseEntry (key, value) = do
        name <- requireName "handler tool" key
        config <-
          withObject
            "result handler"
            (parseAgentFields ("result handler " ++ name))
            value
        pure (name, config)

instance ToJSON McpServerConfig where
  toJSON config =
    object
      [ "url" .= mcpServerUrl config
      , "policy" .= mcpServerPolicy config
      , "handlers" .= mcpHandlers config
      ]

instance ToJSON McpServers where
  toJSON = mcpServersObject . unMcpServers

instance FromJSON McpServers where
  parseJSON = withObject "mcp" $ \servers ->
    McpServers . sortOn mcpServerName
      <$> traverse (uncurry parseMcpServer) (KeyMap.toList servers)

mcpServersObject :: [McpServerConfig] -> Value
mcpServersObject servers =
  keyedObject
    [ (mcpServerName server, toJSON server)
    | server <- servers
    ]

parseMcpServer :: AesonKey.Key -> Value -> Parser McpServerConfig
parseMcpServer nameKey = withObject "mcp server" $ \values -> do
  name <- requireName "MCP server" nameKey
  requireKnownKeys ("MCP server " ++ name) ["url", "policy", "handlers"] values
  url <- values .: "url" >>= requireNonEmpty ("MCP server " ++ name ++ " URL")
  policy <- values .: "policy"
  handlers <- values .: "handlers"
  pure
    McpServerConfig
      { mcpServerName = name
      , mcpServerUrl = Text.unpack url
      , mcpServerPolicy = policy
      , mcpHandlers = handlers
      }

handlersObject :: [(String, HandlerConfig)] -> Value
handlersObject handlers =
  keyedObject
    [ (name, toJSON config)
    | (name, config) <- handlers
    ]

subagentObject :: [SubagentConfig] -> Value
subagentObject agents =
  keyedObject
    [ (subagentName agent, toJSON agent)
    | agent <- agents
    ]

keyedObject :: [(String, Value)] -> Value
keyedObject entries =
  Object
    ( KeyMap.fromList
        [ (AesonKey.fromString name, value)
        | (name, value) <- entries
        ]
    )

parseSubagent :: AesonKey.Key -> Value -> Parser SubagentConfig
parseSubagent nameKey = withObject "subagent" $ \values -> do
  name <- requireName "subagent" nameKey
  SubagentConfig name <$> parseAgentFields ("subagent " ++ name) values

agentKeys :: [Text.Text]
agentKeys = ["model", "description", "systemPrompt", "tools"]

-- | Parse the fields shared by a subagent and a result handler. The label
-- prefixes both required field errors.
parseAgentFields :: String -> Object -> Parser AgentConfig
parseAgentFields label values = do
  requireKnownKeys label agentKeys values
  model <- values .: "model" >>= requireNonEmpty (label ++ " model")
  description <-
    values .: "description" >>= requireNonEmpty (label ++ " description")
  systemPrompt <- values .: "systemPrompt"
  tools <- values .: "tools"
  pure
    AgentConfig
      { agentModel = Text.unpack model
      , agentDescription = Text.unpack description
      , agentSystemPrompt = Text.unpack systemPrompt
      , agentTools = tools
      }

requireNonEmpty :: String -> Text.Text -> Parser Text.Text
requireNonEmpty label text
  | Text.null text = fail (label ++ " must be non-empty")
  | otherwise = pure text

requireName :: String -> AesonKey.Key -> Parser String
requireName label key =
  Text.unpack <$> requireNonEmpty label (AesonKey.toText key)

-- | Reject keys the schema does not define, instead of silently ignoring a
-- misspelling that would disable a tool.
requireKnownKeys :: String -> [Text.Text] -> Object -> Parser ()
requireKnownKeys label allowed values =
  case filter (`notElem` allowed) (map AesonKey.toText (KeyMap.keys values)) of
    [] -> pure ()
    extra ->
      fail
        ( label
            ++ " has unknown keys: "
            ++ intercalate ", " (map Text.unpack extra)
        )
