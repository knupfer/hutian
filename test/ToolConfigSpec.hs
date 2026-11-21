{-# LANGUAGE OverloadedStrings #-}

module ToolConfigSpec (tests) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import Hutian.Policy (ToolPolicy (..))
import Hutian.Tool.Config
  ( McpServerConfig (..)
  , SubagentConfig (..)
  , ToolConfig (..)
  , WebsearchConfig (..)
  , defaultToolConfig
  )
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "tool-config"
    [ testCase "decodes every tool policy and a disabled websearch" $
        decodeConfig
          (toolsValue (String "allow") (String "ask") Null (object []))
          @?= Just
            defaultToolConfig
              { toolShell = ToolAllow
              , toolWebfetch = ToolAsk
              }
    , testCase "a websearch string uses the default ask policy" $
        decodeConfig
          (toolsValue (String "deny") (String "deny") (String "https://search.test/?q=") (object []))
          @?= Just
            defaultToolConfig
              { toolWebsearch =
                  Just
                    WebsearchConfig
                      { websearchUrl = "https://search.test/?q="
                      , websearchPolicy = ToolAsk
                      }
              }
    , testCase "a websearch object keeps its policy" $
        decodeConfig
          ( toolsValue
              (String "deny")
              (String "deny")
              (object ["url" .= ("https://search.test/?q=" :: String), "policy" .= String "deny"])
              (object [])
          )
          @?= Just
            defaultToolConfig
              { toolWebsearch =
                  Just
                    WebsearchConfig
                      { websearchUrl = "https://search.test/?q="
                      , websearchPolicy = ToolDeny
                      }
              }
    , testCase "subagents are keyed by selector and carry a description" $
        decodeConfig
          ( toolsValue
              (String "ask")
              (String "deny")
              Null
              (object ["cheap" .= agentValue "cheap-upstream" "Summarize pages."])
          )
          @?= Just
            defaultToolConfig
              { toolShell = ToolAsk
              , toolSubagent =
                  [ SubagentConfig
                      { subagentName = "cheap"
                      , subagentModel = "cheap-upstream"
                      , subagentDescription = "Summarize pages."
                      , subagentSystemPrompt = "Fetch and summarize."
                      , subagentTools =
                          defaultToolConfig {toolWebfetch = ToolAllow}
                      }
                  ]
              }
    , testCase "subagents are sorted by selector" $
        decodeConfig
          ( toolsValue
              (String "deny")
              (String "deny")
              Null
              ( object
                  [ "zeta" .= agentValue "z" "Z."
                  , "alpha" .= agentValue "a" "A."
                  ]
              )
          )
          @?= Just
            defaultToolConfig
              { toolSubagent =
                  [ SubagentConfig "alpha" "a" "A." "Fetch and summarize." (defaultToolConfig {toolWebfetch = ToolAllow})
                  , SubagentConfig "zeta" "z" "Z." "Fetch and summarize." (defaultToolConfig {toolWebfetch = ToolAllow})
                  ]
              }
    , testCase "subagent names, models, and descriptions must be non-empty" $ do
        let withAgent name agent =
              toolsValue (String "deny") (String "deny") Null (object [name .= agent])
        decodeConfig (withAgent "" (agentValue "m" "D.")) @?= (Nothing :: Maybe ToolConfig)
        decodeConfig (withAgent "cheap" (agentValue "" "D.")) @?= (Nothing :: Maybe ToolConfig)
        decodeConfig (withAgent "cheap" (agentValue "m" "")) @?= (Nothing :: Maybe ToolConfig)
    , testCase "MCP servers are keyed by name" $
        decodeConfig
          ( toolsObject
              (String "deny")
              (String "deny")
              Null
              (object [])
              ( object
                  [ "demo"
                      .= object
                        [ "url" .= ("https://mcp.example.test/mcp" :: String)
                        , "policy" .= String "allow"
                        ]
                  ]
              )
          )
          @?= Just
            defaultToolConfig
              { toolMcp =
                  [ McpServerConfig
                      { mcpServerName = "demo"
                      , mcpServerUrl = "https://mcp.example.test/mcp"
                      , mcpServerPolicy = ToolAllow
                      }
                  ]
              }
    , testCase "unknown policies and malformed shapes are rejected" $ do
        decodeConfig
          (toolsValue (String "sometimes") (String "deny") Null (object []))
          @?= (Nothing :: Maybe ToolConfig)
        decodeConfig
          (toolsValue (String "deny") (String "deny") (object ["policy" .= String "ask"]) (object []))
          @?= (Nothing :: Maybe ToolConfig)
        decodeConfig
          (toolsValue (String "deny") (String "deny") (object ["endpoint" .= ("https://search.test/?q=" :: String)]) (object []))
          @?= (Nothing :: Maybe ToolConfig)
        decodeConfig
          (toolsValue (String "deny") (String "deny") (String "") (object []))
          @?= (Nothing :: Maybe ToolConfig)
        decodeConfig
          (toolsValue (String "deny") (String "deny") (object ["url" .= ("" :: String)]) (object []))
          @?= (Nothing :: Maybe ToolConfig)
        decodeConfig
          (toolsValue (String "deny") (String "deny") Null (Aeson.toJSON ([] :: [()])))
          @?= (Nothing :: Maybe ToolConfig)
        decodeConfig
          (toolsValue (String "deny") (String "deny") Null (object ["cheap" .= object ["model" .= ("m" :: String)]]))
          @?= (Nothing :: Maybe ToolConfig)
        decodeConfig
          ( toolsValue
              (String "deny")
              (String "deny")
              Null
              (object ["cheap" .= object ["modelName" .= ("m" :: String), "description" .= ("D." :: String), "systemPrompt" .= ("" :: String)]])
          )
          @?= (Nothing :: Maybe ToolConfig)
        decodeConfig
          (toolsObject (String "deny") (String "deny") Null (object []) (Aeson.toJSON ([] :: [()])))
          @?= (Nothing :: Maybe ToolConfig)
        decodeConfig
          (toolsObject (String "deny") (String "deny") Null (object []) (object ["demo" .= object []]))
          @?= (Nothing :: Maybe ToolConfig)
        decodeConfig
          (toolsObject (String "deny") (String "deny") Null (object []) (object ["demo" .= object ["url" .= ("" :: String)]]))
          @?= (Nothing :: Maybe ToolConfig)
        decodeConfig
          (toolsObject (String "deny") (String "deny") Null (object []) (object ["" .= object ["url" .= ("https://mcp.example.test/mcp" :: String)]]))
          @?= (Nothing :: Maybe ToolConfig)
    , testCase "encoding round-trips through the parser" $
        let config =
              defaultToolConfig
                { toolShell = ToolAllow
                , toolWebfetch = ToolAsk
                , toolWebsearch =
                    Just
                      WebsearchConfig
                        { websearchUrl = "https://search.test/?q="
                        , websearchPolicy = ToolDeny
                        }
                , toolSubagent =
                    [ SubagentConfig
                        { subagentName = "cheap"
                        , subagentModel = "cheap-upstream"
                        , subagentDescription = "Summarize pages."
                        , subagentSystemPrompt = "Fetch and summarize."
                        , subagentTools = defaultToolConfig
                        }
                    ]
                , toolMcp =
                    [ McpServerConfig
                        { mcpServerName = "demo"
                        , mcpServerUrl = "https://mcp.example.test/mcp"
                        , mcpServerPolicy = ToolAsk
                        }
                    ]
                }
         in Aeson.decode (Aeson.encode config) @?= Just config
    ]

decodeConfig :: Value -> Maybe ToolConfig
decodeConfig value =
  case Aeson.fromJSON value of
    Aeson.Success config -> Just config
    Aeson.Error _ -> Nothing

toolsValue :: Value -> Value -> Value -> Value -> Value
toolsValue shell webfetch websearch subagent =
  toolsObject shell webfetch websearch subagent (object [])

toolsObject :: Value -> Value -> Value -> Value -> Value -> Value
toolsObject shell webfetch websearch subagent mcp =
  object
    [ "shell" .= shell
    , "webfetch" .= webfetch
    , "websearch" .= websearch
    , "subagent" .= subagent
    , "mcp" .= mcp
    ]

agentValue :: String -> String -> Value
agentValue model description =
  object
    [ "model" .= model
    , "description" .= description
    , "systemPrompt" .= ("Fetch and summarize." :: String)
    , "tools" .= toolsValue (String "deny") (String "allow") Null (object [])
    ]
