{-# LANGUAGE OverloadedStrings #-}

module McpProtocolSpec (tests) where

import Data.Aeson (object, (.=))
import qualified Data.ByteString.Char8 as BS8
import Hutian.Mcp.Protocol
import Hutian.Tool.Catalog (McpTool (..))
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "mcp-protocol"
    [ testCase "tool names are namespaced and sanitized" $ do
        mcpToolFunctionName "demo" "search"
          @?= "mcp__demo__search"
        mcpToolFunctionName "a.b" "x y"
          @?= "mcp__a_b__x_y"
    , testCase "tools/list decodes descriptions and schemas" $
        parseTools
          ( object
              [ "tools"
                  .= [ object
                        [ "name" .= ("search" :: String)
                        , "description" .= ("Search the web." :: String)
                        , "inputSchema"
                            .= object
                              [ "type" .= ("object" :: String)
                              , "properties"
                                  .= object ["query" .= object ["type" .= ("string" :: String)]]
                              ]
                        ]
                     , object ["name" .= ("fetch" :: String)]
                     ]
              ]
          )
          @?= Right
            [ McpTool
                { mcpToolName = "search"
                , mcpToolDescription = "Search the web."
                , mcpToolInputSchema =
                    object
                      [ "type" .= ("object" :: String)
                      , "properties"
                          .= object ["query" .= object ["type" .= ("string" :: String)]]
                      ]
                }
            , McpTool
                { mcpToolName = "fetch"
                , mcpToolDescription = ""
                , mcpToolInputSchema = object ["type" .= ("object" :: String)]
                }
            ]
    , testCase "malformed tools are rejected" $ do
        parseTools (object ["tools" .= [object []]])
          @?= Left (McpProtocolError "MCP tool is missing a name")
        parseTools (object ["tools" .= [object ["name" .= ("" :: String)]]])
          @?= Left (McpProtocolError "MCP tool name is empty")
        parseTools (object [])
          @?= Left (McpProtocolError "MCP tools/list result is missing tools")
    , testCase "a JSON-RPC error becomes a readable message" $
        selectJsonRpcResponse
          7
          [ object
              [ "jsonrpc" .= ("2.0" :: String)
              , "id" .= (7 :: Int)
              , "error" .= object ["code" .= (-32602 :: Int), "message" .= ("bad params" :: String)]
              ]
          ]
          @?= Left (McpProtocolError "MCP error: bad params")
    , testCase "responses are selected by id" $
        selectJsonRpcResponse
          2
          [ object ["jsonrpc" .= ("2.0" :: String), "id" .= (1 :: Int), "result" .= object []]
          , object ["jsonrpc" .= ("2.0" :: String), "id" .= (2 :: Int), "result" .= object ["ok" .= True]]
          ]
          @?= Right (object ["ok" .= True])
    , testCase "string response ids are accepted" $
        selectJsonRpcResponse
          3
          [object ["jsonrpc" .= ("2.0" :: String), "id" .= ("3" :: String), "result" .= object ["ok" .= True]]]
          @?= Right (object ["ok" .= True])
    , testCase "text and error content become tool text" $ do
        mcpToolResultText
          (object ["content" .= [object ["type" .= ("text" :: String), "text" .= ("hello" :: String)]]])
          @?= Right "hello"
        mcpToolResultText
          ( object
              [ "isError" .= True
              , "content" .= [object ["type" .= ("text" :: String), "text" .= ("nope" :: String)]]
              ]
          )
          @?= Right "MCP tool error: nope"
    , testCase "typed errors format consistently" $ do
        mcpErrorText (McpHttpError 502 "bad gateway")
          @?= "HTTP 502: bad gateway"
        mcpErrorText (McpHttpError 500 "")
          @?= "HTTP 500"
        mcpErrorText (McpTransportError "connection closed")
          @?= "connection closed"
    , testCase "SSE data payloads are extracted in order" $
        decodeSsePayloads
          ( BS8.pack
              "event: message\ndata: one\n\nevent: message\ndata: two\ndata: three\n\n"
          )
          @?= [BS8.pack "one", BS8.pack "two\nthree"]
    , testCase "the tool definition carries the server description" $
        mcpToolDefinition
          "demo"
          McpTool
            { mcpToolName = "search"
            , mcpToolDescription = "Search the web."
            , mcpToolInputSchema = object ["type" .= ("object" :: String)]
            }
          @?= object
            [ "type" .= ("function" :: String)
            , "function"
                .= object
                  [ "name" .= ("mcp__demo__search" :: String)
                  , "description" .= ("Search the web.\n\nMCP server: demo." :: String)
                  , "parameters" .= object ["type" .= ("object" :: String)]
                  ]
            ]
    ]
