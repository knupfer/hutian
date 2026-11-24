{-# LANGUAGE OverloadedStrings #-}

module ToolConfigSpec (tests) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Map.Strict as Map
import Hutian.Policy (ToolPolicy (..))
import Hutian.Tool.Config
  ( AgentConfig (..)
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
              , toolSubagent = [cheapAgent "cheap-upstream" "Summarize pages."]
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
                  [ setSubagentName "alpha" (cheapAgent "a" "A.")
                  , setSubagentName "zeta" (cheapAgent "z" "Z.")
                  ]
              }
    , testCase "subagent names, models, and descriptions must be non-empty" $ do
        let withAgent name agent =
              toolsValue (String "deny") (String "deny") (object [name .= agent])
        decodeConfig (withAgent "" (agentValue "m" "D.")) @?= (Nothing :: Maybe ToolConfig)
        decodeConfig (withAgent "cheap" (agentValue "" "D.")) @?= (Nothing :: Maybe ToolConfig)
        decodeConfig (withAgent "cheap" (agentValue "m" "")) @?= (Nothing :: Maybe ToolConfig)
    , testCase "MCP servers require url, policy, and handlers" $
        decodeConfig
          ( toolsObject
              (String "deny")
              (String "deny")
              (object [])
              (object ["demo" .= mcpValue "https://mcp.example.test/mcp" "allow" (object [])])
          )
          @?= Just
            defaultToolConfig
              { toolMcp =
                  [ McpServerConfig
                      { mcpServerName = "demo"
                      , mcpServerUrl = "https://mcp.example.test/mcp"
                      , mcpServerPolicy = ToolAllow
                      , mcpHandlers = HandlerConfigs Map.empty
                      }
                  ]
              }
    , testCase "every tool group is required" $ do
        decodeConfig (object ["webfetch" .= String "deny", "subagent" .= object [], "mcp" .= object []])
          @?= (Nothing :: Maybe ToolConfig)
        decodeConfig (object ["shell" .= String "deny", "subagent" .= object [], "mcp" .= object []])
          @?= (Nothing :: Maybe ToolConfig)
        decodeConfig (object ["shell" .= String "deny", "webfetch" .= String "deny", "mcp" .= object []])
          @?= (Nothing :: Maybe ToolConfig)
        decodeConfig (object ["shell" .= String "deny", "webfetch" .= String "deny", "subagent" .= object []])
          @?= (Nothing :: Maybe ToolConfig)
    , testCase "unknown keys are rejected instead of being ignored" $ do
        decodeConfig
          ( insertKey "webFetch"
              (String "allow")
              (toolsValue (String "deny") (String "deny") (object []))
          )
          @?= (Nothing :: Maybe ToolConfig)
        decodeConfig
          ( toolsValue
              (String "deny")
              (String "deny")
              (object ["cheap" .= insertKey "tool" (object []) (agentValue "m" "D.")])
          )
          @?= (Nothing :: Maybe ToolConfig)
        decodeConfig
          ( toolsObject
              (String "deny")
              (String "deny")
              (object [])
              (object ["demo" .= insertKey "url2" (String "x") (mcpValue "https://mcp.example.test/mcp" "allow" (object []))])
          )
          @?= (Nothing :: Maybe ToolConfig)
    , testCase "MCP servers must state their policy and handler map" $ do
        decodeConfig
          ( toolsObject
              (String "deny")
              (String "deny")
              (object [])
              (object ["demo" .= object ["url" .= ("https://mcp.example.test/mcp" :: String), "handlers" .= object []]])
          )
          @?= (Nothing :: Maybe ToolConfig)
        decodeConfig
          ( toolsObject
              (String "deny")
              (String "deny")
              (object [])
              (object ["demo" .= object ["url" .= ("https://mcp.example.test/mcp" :: String), "policy" .= String "allow"]])
          )
          @?= (Nothing :: Maybe ToolConfig)
    , testCase "agent tools are required" $
        decodeConfig
          ( toolsValue
              (String "deny")
              (String "deny")
              ( object
                  [ "cheap"
                      .= object
                        [ "model" .= ("m" :: String)
                        , "description" .= ("D." :: String)
                        , "systemPrompt" .= ("" :: String)
                        ]
                  ]
              )
          )
          @?= (Nothing :: Maybe ToolConfig)
    , testCase "MCP result handlers can have their own MCP result handlers" $
        let nestedHandler = handlerConfig "nested" "Nested summary." defaultToolConfig
            innerServer =
              McpServerConfig
                { mcpServerName = "inner"
                , mcpServerUrl = "https://inner.example.test/mcp"
                , mcpServerPolicy = ToolAllow
                , mcpHandlers = HandlerConfigs (Map.fromList [("fetch", nestedHandler)])
                }
            outerHandler =
              handlerConfig
                "cheap"
                "Summarize search results."
                (defaultToolConfig {toolMcp = [innerServer]})
            innerJson =
              mcpValue
                "https://inner.example.test/mcp"
                "allow"
                ( object
                    [ "fetch"
                        .= handlerValue
                          "nested"
                          "Nested summary."
                          (toolsValue (String "deny") (String "deny") (object []))
                    ]
                )
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
                    .= mcpValue
                      "https://mcp.example.test/mcp"
                      "allow"
                      (object ["search" .= outerJson])
                ]
         in decodeConfig (toolsObject (String "deny") (String "deny") (object []) mcpJson)
              @?= Just
                defaultToolConfig
                  { toolMcp =
                      [ McpServerConfig
                          { mcpServerName = "demo"
                          , mcpServerUrl = "https://mcp.example.test/mcp"
                          , mcpServerPolicy = ToolAllow
                          , mcpHandlers = HandlerConfigs (Map.fromList [("search", outerHandler)])
                          }
                      ]
                  }
    , testCase "MCP server result handlers are decoded" $
        let handlerConfigValue =
              handlerConfig "cheap" "Summarize search results." defaultToolConfig
            mcpJson =
              object
                [ "demo"
                    .= mcpValue
                      "https://mcp.example.test/mcp"
                      "allow"
                      ( object
                          [ "search"
                              .= handlerValue
                                "cheap"
                                "Summarize search results."
                                (toolsValue (String "deny") (String "deny") (object []))
                          ]
                      )
                ]
         in decodeConfig (toolsObject (String "deny") (String "deny") (object []) mcpJson)
              @?= Just
                defaultToolConfig
                  { toolMcp =
                      [ McpServerConfig
                          { mcpServerName = "demo"
                          , mcpServerUrl = "https://mcp.example.test/mcp"
                          , mcpServerPolicy = ToolAllow
                          , mcpHandlers = HandlerConfigs (Map.fromList [("search", handlerConfigValue)])
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
                , toolSubagent = [cheapAgent "cheap-upstream" "Summarize pages."]
                , toolMcp =
                    [ McpServerConfig
                        { mcpServerName = "demo"
                        , mcpServerUrl = "https://mcp.example.test/mcp"
                        , mcpServerPolicy = ToolAsk
                        , mcpHandlers =
                            HandlerConfigs
                              ( Map.fromList
                                  [ ( "search"
                                    , handlerConfig "cheap" "Summarize search." defaultToolConfig
                                    )
                                  ]
                              )
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

mcpValue :: String -> String -> Value -> Value
mcpValue url policy handlers =
  object
    [ "url" .= url
    , "policy" .= policy
    , "handlers" .= handlers
    ]

-- Add one key to an object, for unknown-key tests.
insertKey :: String -> Value -> Value -> Value
insertKey key value (Object fields) =
  Object (KeyMap.insert (AesonKey.fromString key) value fields)
insertKey _ _ value = value

handlerValue :: String -> String -> Value -> Value
handlerValue model description tools =
  object
    [ "model" .= model
    , "description" .= description
    , "systemPrompt" .= ("Summarize." :: String)
    , "tools" .= tools
    ]

handlerConfig :: String -> String -> ToolConfig -> AgentConfig
handlerConfig model description tools =
  AgentConfig
    { agentModel = model
    , agentDescription = description
    , agentSystemPrompt = "Summarize."
    , agentTools = tools
    }

agentValue :: String -> String -> Value
agentValue model description =
  object
    [ "model" .= model
    , "description" .= description
    , "systemPrompt" .= ("Fetch and summarize." :: String)
    , "tools" .= toolsValue (String "deny") (String "allow") (object [])
    ]

cheapAgent :: String -> String -> SubagentConfig
cheapAgent model description =
  SubagentConfig
    { subagentName = "cheap"
    , subagentAgent =
        AgentConfig
          { agentModel = model
          , agentDescription = description
          , agentSystemPrompt = "Fetch and summarize."
          , agentTools = defaultToolConfig {toolWebfetch = ToolAllow}
          }
    }

setSubagentName :: String -> SubagentConfig -> SubagentConfig
setSubagentName name agent = agent {subagentName = name}
