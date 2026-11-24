{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | HTTP client for remote MCP servers. Discovery happens before a model turn;
-- tool calls use the session id from that handshake.
module Hutian.Mcp
  ( discoverMcpServers
  , callMcpTool
  ) where

import Control.Applicative ((<|>))
import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (intercalate, isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncodingError
import Hutian.Http.Body (readBodyAtMost)
import Hutian.Http.Error (trySync)
import Hutian.Mcp.Protocol
  ( McpError (..)
  , decodeSsePayloads
  , mcpCallToolRequest
  , mcpErrorText
  , mcpInitializeRequest
  , mcpInitializedNotification
  , mcpListToolsRequest
  , mcpProtocolVersion
  , mcpToolFunctionName
  , mcpToolResultText
  , parseTools
  , protocolError
  , selectJsonRpcResponse
  )
import Hutian.Policy (ToolPolicy (..))
import Hutian.Tool.Catalog
  ( McpServer (..)
  , McpSessionId
  , McpTool (..)
  , mcpSessionIdFromString
  , mcpSessionIdText
  )
import Hutian.Tool.Config (HandlerConfigs (..), McpServerConfig (..))
import Network.HTTP.Client
  ( Manager
  , Request (..)
  , RequestBody (RequestBodyLBS)
  , Response (..)
  , brRead
  , newManager
  , parseRequest
  , responseTimeoutMicro
  , withResponse
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (HeaderName, hContentType)
import Network.HTTP.Types.Status (statusCode)

-- | Discover every configured MCP server. A failure is reported through the
-- logger and leaves that server with no advertised tools.
discoverMcpServers :: (String -> IO ()) -> [McpServerConfig] -> IO [McpServer]
discoverMcpServers report = mapM (discoverServer report)

discoverServer :: (String -> IO ()) -> McpServerConfig -> IO McpServer
discoverServer report config
  | mcpServerPolicy config == ToolDeny = pure (emptyServer config)
  | otherwise = do
      manager <- newManager tlsManagerSettings
      initialized <-
        postMcp manager config Nothing 1 (mcpInitializeRequest 1)
      case initialized of
        Left failure -> reportFailure failure >> pure (emptyServer config)
        Right (session, _) -> do
          _ <-
            postNotification
              manager
              config
              session
              mcpInitializedNotification
          listed <-
            postMcp manager config session 2 (mcpListToolsRequest 2)
          case listed of
            Left failure -> do
              reportFailure failure
              pure (McpServer config [] session)
            Right (session', result) ->
              case parseTools result of
                Left failure -> do
                  reportFailure failure
                  pure (McpServer config [] (session' <|> session))
                Right tools -> do
                  let (uniqueTools, duplicates) = uniqueMcpTools config tools
                  mapM_ (reportDuplicate report config) duplicates
                  reportUnknownHandlers report config uniqueTools
                  pure (McpServer config uniqueTools (session' <|> session))
  where
    reportFailure failure =
      report ("mcp " ++ mcpServerName config ++ ": " ++ mcpErrorText failure)

emptyServer :: McpServerConfig -> McpServer
emptyServer config = McpServer config [] Nothing

-- | Report handler keys that do not match a tool discovered on the server.
-- The handler stays in the configuration but never runs.
reportUnknownHandlers :: (String -> IO ()) -> McpServerConfig -> [McpTool] -> IO ()
reportUnknownHandlers report config tools =
  mapM_ reportMissing (Map.toList (unHandlerConfigs (mcpHandlers config)))
  where
    known = map mcpToolName tools
    reportMissing (name, _)
      | name `elem` known = pure ()
      | otherwise =
          report
            ( "mcp "
                ++ mcpServerName config
                ++ ": handlers."
                ++ name
                ++ " is configured but the server has no tool named "
                ++ show name
                ++ knownTools
                ++ "; the result handler is ignored"
            )
    knownTools =
      case known of
        [] -> "; the server exposes no tools"
        _ -> "; known tools: " ++ intercalate ", " (map show known)

-- | Keep the first tool for every API-facing name. Two remote tools can
-- sanitize to the same function name, and advertising both would make the
-- model's call ambiguous.
uniqueMcpTools :: McpServerConfig -> [McpTool] -> ([McpTool], [McpTool])
uniqueMcpTools config = go Set.empty
  where
    go _ [] = ([], [])
    go seen (tool : rest)
      | Set.member key seen =
          let (unique, duplicates) = go seen rest
           in (unique, tool : duplicates)
      | otherwise =
          let (unique, duplicates) = go (Set.insert key seen) rest
           in (tool : unique, duplicates)
      where
        key = mcpToolFunctionName (mcpServerName config) (mcpToolName tool)

reportDuplicate :: (String -> IO ()) -> McpServerConfig -> McpTool -> IO ()
reportDuplicate report config tool =
  report
    ( "mcp "
        ++ mcpServerName config
        ++ ": tool "
        ++ show (mcpToolName tool)
        ++ " has the same API name as an earlier tool; only the first is advertised"
    )

-- | Call one discovered MCP tool and format its content as tool output.
callMcpTool :: McpServer -> McpTool -> Value -> IO (Either McpError String)
callMcpTool server tool arguments = do
  manager <- newManager tlsManagerSettings
  result <-
    postMcp
      manager
      (mcpServerConfig server)
      (mcpServerSession server)
      1
      (mcpCallToolRequest 1 (mcpToolName tool) arguments)
  pure $ do
    (_, value) <- result
    mcpToolResultText value

-- * HTTP transport

data McpHttpReply = McpHttpReply
  { mcpHttpSession :: Maybe McpSessionId
  , mcpHttpContentType :: String
  , mcpHttpBody :: BS.ByteString
  }

-- | Send one JSON-RPC request and return the session id plus the unwrapped
-- result. Non-2xx responses and malformed bodies become typed errors.
postMcp
  :: Manager
  -> McpServerConfig
  -> Maybe McpSessionId
  -> Int
  -> Value
  -> IO (Either McpError (Maybe McpSessionId, Value))
postMcp manager config session expectedId message = do
  raw <- postMcpRaw manager config session message
  pure $ do
    reply <- raw
    values <- responseValues reply
    result <- selectJsonRpcResponse expectedId values
    pure (mcpHttpSession reply, result)

-- | Send a JSON-RPC notification. The server normally answers with 202 and no
-- body; a new session id is still captured if it appears.
postNotification
  :: Manager
  -> McpServerConfig
  -> Maybe McpSessionId
  -> Value
  -> IO (Either McpError (Maybe McpSessionId))
postNotification manager config session message = do
  raw <- postMcpRaw manager config session message
  pure (mcpHttpSession <$> raw)

postMcpRaw
  :: Manager
  -> McpServerConfig
  -> Maybe McpSessionId
  -> Value
  -> IO (Either McpError McpHttpReply)
postMcpRaw manager config session message = do
  outcome <- trySync (send manager)
  pure $ case outcome of
    Left failure -> Left (McpTransportError failure)
    Right value -> value
  where
    send currentManager = do
      initialRequest <- parseRequest (mcpServerUrl config)
      let request =
            initialRequest
              { method = "POST"
              , requestHeaders =
                  [ ("Content-Type", "application/json")
                  , ("Accept", "application/json, text/event-stream")
                  , ("MCP-Protocol-Version", BS8.pack mcpProtocolVersion)
                  ]
                    ++ sessionHeaders
              , requestBody = RequestBodyLBS (Aeson.encode message)
              , responseTimeout = responseTimeoutMicro mcpTimeoutMicros
              , redirectCount = 0
              , checkResponse = \_ _ -> pure ()
              }
      withResponse request currentManager $ \response -> do
        body <- readBodyAtMost maxMcpBody (brRead (responseBody response))
        if statusCode (responseStatus response) >= 400
          then
            pure
              ( Left
                  ( McpHttpError
                      (statusCode (responseStatus response))
                      (decodeBodyText (BS.take 1024 body))
                  )
              )
          else
            pure
              ( Right
                  McpHttpReply
                    { mcpHttpSession =
                        mcpSessionIdFromString . BS8.unpack
                          <$> lookup hMcpSessionId (responseHeaders response)
                    , mcpHttpContentType =
                        maybe "" BS8.unpack (lookup hContentType (responseHeaders response))
                    , mcpHttpBody = body
                    }
              )

    sessionHeaders =
      case session of
        Nothing -> []
        Just value -> [("Mcp-Session-Id", BS8.pack (mcpSessionIdText value))]

-- | Decode either an SSE stream or a plain JSON response into JSON-RPC values.
responseValues :: McpHttpReply -> Either McpError [Value]
responseValues reply
  | "text/event-stream" `isInfixOf` mcpHttpContentType reply =
      traverse decodeValue (decodeSsePayloads (mcpHttpBody reply))
  | BS.null (mcpHttpBody reply) =
      protocolError "MCP response body is empty"
  | otherwise =
      (: []) <$> decodeValue (mcpHttpBody reply)
  where
    decodeValue bytes =
      case Aeson.decodeStrict' bytes of
        Just value -> Right value
        Nothing -> protocolError "MCP response is not valid JSON"

-- | MCP tool results can be large pages or search bundles. A complete JSON
-- document must fit, so this is larger than the web-tool body limit while still
-- bounding one call.
maxMcpBody :: Int
maxMcpBody = 16 * 1024 * 1024

mcpTimeoutMicros :: Int
mcpTimeoutMicros = 60 * 1000 * 1000

hMcpSessionId :: HeaderName
hMcpSessionId = "Mcp-Session-Id"

-- | Decode an HTTP error body as UTF-8 text. Header values are ASCII tokens,
-- but an error body can contain any text the server chose to send.
decodeBodyText :: BS.ByteString -> String
decodeBodyText =
  Text.unpack . TextEncoding.decodeUtf8With TextEncodingError.lenientDecode
