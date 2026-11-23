{-# LANGUAGE OverloadedStrings #-}

module ToolConfigSpec (tests) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import Hutian.Policy (ToolPolicy (..))
import Hutian.Tool.Config
  ( HandlerConfig (..)
  , HandlerConfigs (..)
  , McpServerConfig (..)
  , SubagentConfig (..)
  , ToolConfig (..)
  , defaultToolConfig
  )
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "tool-config"
    [ testCase "decodes every tool policy" $
        decodeConfig
          (toolsValue (String "allow") (String "ask") (object []))
          @?= Just
            defaultToolConfig
              { toolShell = ToolAllow
              , toolWebfetch = ToolAsk
              }
    , testCase "subagents are keyed by selector and carry a description" $
        decodeConfig
          ( toolsValue
              (String "ask")
              (String "deny")
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
              toolsValue (String "deny") (String "deny") (object [name .= agent])
        decodeConfig (withAgent "" (agentValue "m" "D.")) @?= (Nothing :: Maybe ToolConfig)
        decodeConfig (withAgent "cheap" (agentValue "" "D.")) @?= (Nothing :: Maybe ToolConfig)
        decodeConfig (withAgent "cheap" (agentValue "m" "")) @?= (Nothing :: Maybe ToolConfig)
    , testCase "MCP servers are keyed by name" $
        decodeConfig
          ( toolsObject
              (String "deny")
              (String "deny")
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
                      , mcpHandlers = HandlerConfigs []
                      }
                  ]
              }
    , testCase "MCP result handlers can have their own MCP result handlers" $
        let nestedHandler =
              HandlerConfig
                { handlerModel = "nested"
                , handlerDescription = "Nested summary."
                , handlerSystemPrompt = "Summarize."
                , handlerTools = defaultToolConfig
                }
            innerServer =
              McpServerConfig
                { mcpServerName = "inner"
                , mcpServerUrl = "https://inner.example.test/mcp"
                , mcpServerPolicy = ToolAllow
                , mcpHandlers = HandlerConfigs [("fetch", nestedHandler)]
                }
            outerHandler =
              HandlerConfig
                { handlerModel = "cheap"
                , handlerDescription = "Summarize search results."
                , handlerSystemPrompt = "Summarize."
                , handlerTools = defaultToolConfig {toolMcp = [innerServer]}
                }
            innerJson =
              object
                [ "url" .= ("https://inner.example.test/mcp" :: String)
                , "policy" .= String "allow"
                , "handlers"
                    .= object
                      [ "fetch"
                          .= handlerValue
                            "nested"
                            "Nested summary."
                            (toolsValue (String "deny") (String "deny") (object []))
                      ]
                ]
            outerJson =
              handlerValue
                "cheap"
                "Summarize search results."
                ( toolsObject
                    (String "deny")
                    (String "deny")
                    (object [])
                    (object ["inner" .= innerJson])
                )
            mcpJson =
              object
                [ "demo"
                    .= object
                      [ "url" .= ("https://mcp.example.test/mcp" :: String)
                      , "policy" .= String "allow"
                      , "handlers" .= object ["search" .= outerJson]
                      ]
                ]
         in decodeConfig (toolsObject (String "deny") (String "deny") (object []) mcpJson)
              @?= Just
                defaultToolConfig
                  { toolMcp =
                      [ McpServerConfig
                          { mcpServerName = "demo"
                          , mcpServerUrl = "https://mcp.example.test/mcp"
                          , mcpServerPolicy = ToolAllow
                          , mcpHandlers = HandlerConfigs [("search", outerHandler)]
                          }
                      ]
                  }
    , testCase "MCP server result handlers are decoded" $
        let handlerConfig =
              HandlerConfig
                { handlerModel = "cheap"
                , handlerDescription = "Summarize search results."
                , handlerSystemPrompt = "Summarize."
                , handlerTools = defaultToolConfig
                }
            mcpJson =
              object
                [ "demo"
                    .= object
                      [ "url" .= ("https://mcp.example.test/mcp" :: String)
                      , "policy" .= String "allow"
                      , "handlers"
                          .= object
                            [ "search"
                                .= handlerValue
                                  "cheap"
                                  "Summarize search results."
                                  (toolsValue (String "deny") (String "deny") (object []))
                            ]
                      ]
                ]
         in decodeConfig (toolsObject (String "deny") (String "deny") (object []) mcpJson)
              @?= Just
                defaultToolConfig
                  { toolMcp =
                      [ McpServerConfig
                          { mcpServerName = "demo"
                          , mcpServerUrl = "https://mcp.example.test/mcp"
                          , mcpServerPolicy = ToolAllow
                          , mcpHandlers = HandlerConfigs [("search", handlerConfig)]
                          }
                      ]
                  }
    , testCase "unknown policies and malformed shapes are rejected" $ do
        decodeConfig
          (toolsValue (String "sometimes") (String "deny") (object []))
          @?= (Nothing :: Maybe ToolConfig)
        decodeConfig
          (toolsValue (String "deny") (String "deny") (Aeson.toJSON ([] :: [()])))
          @?= (Nothing :: Maybe ToolConfig)
        decodeConfig
          (toolsValue (String "deny") (String "deny") (object ["cheap" .= object ["model" .= ("m" :: String)]]))
          @?= (Nothing :: Maybe ToolConfig)
        decodeConfig
          ( toolsValue
              (String "deny")
              (String "deny")
              (object ["cheap" .= object ["modelName" .= ("m" :: String), "description" .= ("D." :: String), "systemPrompt" .= ("" :: String)]])
          )
          @?= (Nothing :: Maybe ToolConfig)
        decodeConfig
          (toolsObject (String "deny") (String "deny") (object []) (Aeson.toJSON ([] :: [()])))
          @?= (Nothing :: Maybe ToolConfig)
        decodeConfig
          (toolsObject (String "deny") (String "deny") (object []) (object ["demo" .= object []]))
          @?= (Nothing :: Maybe ToolConfig)
        decodeConfig
          (toolsObject (String "deny") (String "deny") (object []) (object ["demo" .= object ["url" .= ("" :: String)]]))
          @?= (Nothing :: Maybe ToolConfig)
        decodeConfig
          (toolsObject (String "deny") (String "deny") (object []) (object ["" .= object ["url" .= ("https://mcp.example.test/mcp" :: String)]]))
          @?= (Nothing :: Maybe ToolConfig)
    , testCase "encoding round-trips through the parser" $
        let config =
              defaultToolConfig
                { toolShell = ToolAllow
                , toolWebfetch = ToolAsk
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
                        , mcpHandlers =
                            HandlerConfigs
                              [ ( "search"
                                , HandlerConfig
                                    { handlerModel = "cheap"
                                    , handlerDescription = "Summarize search."
                                    , handlerSystemPrompt = "Summarize."
                                    , handlerTools = defaultToolConfig
                                    }
                                )
                              ]
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

toolsValue :: Value -> Value -> Value -> Value
toolsValue shell webfetch subagent =
  toolsObject shell webfetch subagent (object [])

toolsObject :: Value -> Value -> Value -> Value -> Value
toolsObject shell webfetch subagent mcp =
  object
    [ "shell" .= shell
    , "webfetch" .= webfetch
    , "subagent" .= subagent
    , "mcp" .= mcp
    ]

handlerValue :: String -> String -> Value -> Value
handlerValue model description tools =
  object
    [ "model" .= model
    , "description" .= description
    , "systemPrompt" .= ("Summarize." :: String)
    , "tools" .= tools
    ]

agentValue :: String -> String -> Value
agentValue model description =
  object
    [ "model" .= model
    , "description" .= description
    , "systemPrompt" .= ("Fetch and summarize." :: String)
    , "tools" .= toolsValue (String "deny") (String "allow") (object [])
    ]
