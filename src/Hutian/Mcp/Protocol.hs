{-# LANGUAGE OverloadedStrings #-}

-- | Pure MCP protocol helpers: JSON-RPC messages, tool discovery decoding,
-- result formatting, and server-sent event payloads. The HTTP client lives in
-- "Hutian.Mcp".
module Hutian.Mcp.Protocol
  ( mcpProtocolVersion
  , mcpInitializeRequest
  , mcpInitializedNotification
  , mcpListToolsRequest
  , mcpCallToolRequest
  , mcpToolFunctionName
   , mcpToolDefinition
  , McpError (..)
  , mcpErrorText
  , parseJsonRpcResult
  , selectJsonRpcResponse
  , parseTools
  , mcpToolResultText
  , decodeSsePayloads
  ) where

import Data.Aeson
  ( Value (..)
  , object
  , (.=)
  )
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isAlphaNum, isAscii, isSpace)
import Data.List (intercalate)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Hutian.Tool.Catalog (McpTool (..))
import Text.Read (readMaybe)

-- | The protocol revision used for Streamable HTTP servers.
mcpProtocolVersion :: String
mcpProtocolVersion = "2024-11-05"

-- | A transport, HTTP, or protocol failure. Keeping it typed lets callers show
-- the same text while tests assert the failure kind.
data McpError
  = McpHttpError Int String
  | McpTransportError String
  | McpProtocolError String
  deriving stock (Eq, Show)

mcpErrorText :: McpError -> String
mcpErrorText (McpHttpError status body) =
  "HTTP " ++ show status ++ (if null body then "" else ": " ++ body)
mcpErrorText (McpTransportError message) = message
mcpErrorText (McpProtocolError message) = message

mcpInitializeRequest :: Int -> Value
mcpInitializeRequest identifier =
  object
    [ "jsonrpc" .= ("2.0" :: String)
    , "id" .= identifier
    , "method" .= ("initialize" :: String)
    , "params"
        .= object
          [ "protocolVersion" .= mcpProtocolVersion
          , "capabilities" .= object []
          , "clientInfo"
              .= object
                [ "name" .= ("hutian" :: String)
                , "version" .= ("0.9" :: String)
                ]
          ]
    ]

mcpInitializedNotification :: Value
mcpInitializedNotification =
  object
    [ "jsonrpc" .= ("2.0" :: String)
    , "method" .= ("notifications/initialized" :: String)
    ]

mcpListToolsRequest :: Int -> Value
mcpListToolsRequest identifier =
  object
    [ "jsonrpc" .= ("2.0" :: String)
    , "id" .= identifier
    , "method" .= ("tools/list" :: String)
    , "params" .= object []
    ]

mcpCallToolRequest :: Int -> String -> Value -> Value
mcpCallToolRequest identifier toolName arguments =
  object
    [ "jsonrpc" .= ("2.0" :: String)
    , "id" .= identifier
    , "method" .= ("tools/call" :: String)
    , "params"
        .= object
          [ "name" .= toolName
          , "arguments" .= arguments
          ]
    ]

-- | The model-facing name of one MCP tool. The server prefix avoids collisions
-- with built-in tools and with other MCP servers.
mcpToolFunctionName :: String -> String -> String
mcpToolFunctionName serverName toolName =
  "mcp__" ++ sanitizeName serverName ++ "__" ++ sanitizeName toolName

sanitizeName :: String -> String
sanitizeName text =
  case map replace text of
    "" -> "unnamed"
    cleaned -> cleaned
  where
    replace character
      | isAscii character && (isAlphaNum character || character == '_' || character == '-') =
          character
      | otherwise = '_'

mcpToolDefinition :: String -> McpTool -> Value
mcpToolDefinition serverName tool =
  object
    [ "type" .= ("function" :: String)
    , "function"
        .= object
          [ "name" .= mcpToolFunctionName serverName (mcpToolName tool)
          , "description" .= description
          , "parameters" .= mcpToolInputSchema tool
          ]
    ]
  where
    serverLine = "MCP server: " ++ serverName ++ "."
    description
      | null (mcpToolDescription tool) = serverLine
      | otherwise = mcpToolDescription tool ++ "\n\n" ++ serverLine

-- | Decode a JSON-RPC response and return its @result@.
parseJsonRpcResult :: Int -> Value -> Either McpError Value
parseJsonRpcResult expected value =
  case value of
    Object fields ->
      case KeyMap.lookup "error" fields of
        Just errorValue ->
          Left (McpProtocolError ("MCP error: " ++ errorText errorValue))
        Nothing ->
          case responseId value of
            Nothing -> protocolError "MCP response is missing an id"
            Just actual
              | actual /= expected ->
                  protocolError "MCP response id did not match"
              | otherwise ->
                  case KeyMap.lookup "result" fields of
                    Just result -> Right result
                    Nothing ->
                      protocolError "MCP response is missing a result"
    _ -> protocolError "MCP response is not an object"

-- | Pick the response matching the request id out of an SSE message list.
selectJsonRpcResponse :: Int -> [Value] -> Either McpError Value
selectJsonRpcResponse expected = go
  where
    go [] = protocolError "MCP response did not arrive"
    go (value : rest)
      | responseId value == Just expected = parseJsonRpcResult expected value
      | otherwise = go rest

responseId :: Value -> Maybe Int
responseId (Object fields) =
  case KeyMap.lookup "id" fields of
    Just (Number number) -> Just (round number)
    Just (String text) -> readMaybe (Text.unpack text)
    _ -> Nothing
responseId _ = Nothing

errorText :: Value -> String
errorText (Object fields) =
  case KeyMap.lookup "message" fields of
    Just (String message) -> Text.unpack message
    _ -> show (Object fields)
errorText value = show value

protocolError :: String -> Either McpError a
protocolError = Left . McpProtocolError

-- | Decode the @tools/list@ result into the tools Hutian advertises.
parseTools :: Value -> Either McpError [McpTool]
parseTools value =
  case value of
    Object fields ->
      case KeyMap.lookup "tools" fields of
        Just (Array tools) -> traverse parseTool (Vector.toList tools)
        _ -> protocolError "MCP tools/list result is missing tools"
    _ -> protocolError "MCP tools/list result is not an object"
  where
    parseTool (Object fields) =
      case KeyMap.lookup "name" fields of
        Just (String name)
          | Text.null name -> protocolError "MCP tool name is empty"
          | otherwise -> do
              let description =
                    case KeyMap.lookup "description" fields of
                      Just (String text) -> Text.unpack text
                      _ -> ""
                  schema =
                    case KeyMap.lookup "inputSchema" fields of
                      Just schemaValue@(Object _) -> schemaValue
                      _ -> object ["type" .= ("object" :: String)]
              pure
                McpTool
                  { mcpToolName = Text.unpack name
                  , mcpToolDescription = description
                  , mcpToolInputSchema = schema
                  }
        _ -> protocolError "MCP tool is missing a name"
    parseTool _ = protocolError "MCP tool is not an object"

-- | Turn a @tools/call@ result into text for the model. MCP servers return one or more
-- text content parts; other content types are shown as JSON so nothing is lost.
mcpToolResultText :: Value -> Either McpError String
mcpToolResultText value =
  case value of
    Object fields -> do
      let content =
            case KeyMap.lookup "content" fields of
              Just (Array items) -> map contentText (Vector.toList items)
              _ -> []
          structured =
            maybe "" show (KeyMap.lookup "structuredContent" fields)
          text
            | null content = structured
            | otherwise = intercalate "\n" content
          isError =
            case KeyMap.lookup "isError" fields of
              Just (Bool flag) -> flag
              _ -> False
      pure (if isError then "MCP tool error: " ++ text else text)
    _ -> protocolError "MCP tool result is not an object"

contentText :: Value -> String
contentText (Object fields) =
  case KeyMap.lookup "type" fields of
    Just (String "text") -> maybe "" Text.unpack (lookupText "text" fields)
    Just (String "resource") ->
      case KeyMap.lookup "resource" fields of
        Just (Object resource) ->
          maybe (show (Object resource)) Text.unpack (lookupText "text" resource)
        _ -> show (Object fields)
    Just (String contentType) ->
      "[MCP " ++ Text.unpack contentType ++ " content]"
    _ -> show (Object fields)
contentText value = show value

lookupText :: String -> KeyMap.KeyMap Value -> Maybe Text.Text
lookupText key fields =
  case KeyMap.lookup (AesonKey.fromString key) fields of
    Just (String text) -> Just text
    _ -> Nothing

-- | Extract every non-empty @data:@ payload from a server-sent event body.
-- MCP responses are one JSON document per event; multi-line data fields are
-- joined. Servers also send comment or keep-alive events with empty data, which
-- are not JSON-RPC responses.
decodeSsePayloads :: BS.ByteString -> [BS.ByteString]
decodeSsePayloads body = go (BS8.lines body) [] []
  where
    go [] current done = reverse (finish current done)
    go (rawLine : rest) current done =
      let line = BS.dropWhileEnd (== 13) rawLine
       in if BS.null line
            then go rest [] (finish current done)
            else
              case BS.stripPrefix "data:" line of
                Just payload ->
                  go rest (BS.dropWhile (== 32) payload : current) done
                Nothing -> go rest current done
    finish [] done = done
    finish current done =
      let payload = BS.intercalate "\n" (reverse current)
       in if BS8.all isSpace payload
            then done
            else payload : done
