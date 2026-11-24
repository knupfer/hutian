{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module McpSpec (tests) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, bracket, catch)
import Data.Aeson (object, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (toLower)
import Data.Either (fromRight)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Hutian.Context (TurnContext (..))
import Hutian.Conversation
  ( ConversationEntry (..)
  , ToolCall (..)
  , emptyConversation
  )
import Hutian.Jobs.Runtime (withJobRuntime)
import Hutian.Mcp (callMcpTool, discoverMcpServers)
import Hutian.Policy (ToolPolicy (..))
import Hutian.Tool.Catalog
  ( McpServer (..)
  , mcpSessionIdFromString
  , McpTool (..)
  , toolCatalog
  , withMcpServers
  )
import Hutian.Tool.Config
  ( AgentConfig (..)
  , HandlerConfigs (..)
  , McpServerConfig (..)
  , ToolConfig (..)
  , defaultToolConfig
  )
import Hutian.Tool.Exec (ToolRuntime (..), runToolCallsWith)
import Hutian.Verbosity (Verbosity (..))
import Network.Socket
  ( Family (AF_INET)
  , SockAddr (SockAddrInet)
  , Socket
  , SocketOption (ReuseAddr)
  , SocketType (Stream)
  , accept
  , bind
  , close
  , defaultProtocol
  , listen
  , setSocketOption
  , socket
  , socketPort
  , tupleToHostAddress
  )
import Network.Socket.ByteString (recv, sendAll)
import Test.Tasty
import Test.Tasty.HUnit
import TestSupport (stripAnsi, withPseudoTerminalOutput, withTemporaryDirectory)
import Text.Read (readMaybe)

tests :: TestTree
tests =
  testGroup
    "mcp"
    [ testCase "discovery and tool calls use the Streamable HTTP protocol" $
        withMcpUpstream $ \port requests -> do
          let server =
                McpServerConfig
                  { mcpServerName = "demo"
                  , mcpServerUrl = "http://127.0.0.1:" ++ show port ++ "/mcp"
                  , mcpServerPolicy = ToolAllow
                  , mcpHandlers = HandlerConfigs Map.empty
                  }
              config = defaultToolConfig {toolMcp = [server]}
          logs <- newIORef []
          discovered <-
            discoverMcpServers
              (\message -> modifyIORef' logs (message :))
              (toolMcp config)
          readIORef logs >>= (@?= [])
          case discovered of
            [discoveredServer] -> do
              mcpServerSession discoveredServer
                @?= Just (mcpSessionIdFromString "test-session")
              map mcpToolName (mcpServerTools discoveredServer) @?= ["search"]
              case mcpServerTools discoveredServer of
                searchTool : _ -> do
                  result <-
                    callMcpTool
                      discoveredServer
                      searchTool
                      (object ["query" .= ("haskell" :: String)])
                  result @?= Right "Found results"
                [] -> assertFailure "discovery returned no tools"
            _ -> assertFailure "discovery did not return exactly one server"
          bodies <- reverse <$> readIORef requests
          length bodies @?= 4
          assertBool
            "the initialized notification was sent"
            (any (("notifications/initialized" `isInfixOf`) . BS8.unpack) bodies)
          assertBool
            "tools/list used the session id"
            ( any
                (\body -> "tools/list" `isInfixOf` BS8.unpack body && "Mcp-Session-Id: test-session" `isInfixOf` BS8.unpack body)
                bodies
            )
          assertBool
            "tools/call used the session id"
            ( any
                (\body -> "tools/call" `isInfixOf` BS8.unpack body && "Mcp-Session-Id: test-session" `isInfixOf` BS8.unpack body)
                bodies
            )
    , testCase "result handlers for unknown MCP tools are reported" $
        withMcpUpstream $ \port _ -> do
          let handler =
                AgentConfig
                  { agentModel = "unused"
                  , agentDescription = "unused"
                  , agentSystemPrompt = "unused"
                  , agentTools = defaultToolConfig
                  }
              server =
                McpServerConfig
                  { mcpServerName = "demo"
                  , mcpServerUrl = "http://127.0.0.1:" ++ show port ++ "/mcp"
                  , mcpServerPolicy = ToolAllow
                  , mcpHandlers = HandlerConfigs (Map.fromList [("missing", handler)])
                  }
          logs <- newIORef []
          servers <-
            discoverMcpServers
              (\message -> modifyIORef' logs (message :))
              [server]
          messages <- readIORef logs
          assertBool
            "unknown result tool is reported"
            ( any
                (\message ->
                   "handlers.missing" `isInfixOf` message
                     && "no tool" `isInfixOf` message
                )
                messages
            )
          length servers @?= 1
    , testCase "a result handler replaces the MCP output label" $
        withMcpUpstream $ \port _ ->
          withTemporaryDirectory "hutian-mcp-result" $ \directory ->
            withJobRuntime directory directory $ \jobs -> do
              let handler =
                    AgentConfig
                      { agentModel = "unused"
                      , agentDescription = "unused"
                      , agentSystemPrompt = "unused"
                      , agentTools = defaultToolConfig
                      }
                  server =
                    McpServerConfig
                      { mcpServerName = "demo"
                      , mcpServerUrl = "http://127.0.0.1:" ++ show port ++ "/mcp"
                      , mcpServerPolicy = ToolAllow
                      , mcpHandlers = HandlerConfigs (Map.fromList [("search", handler)])
                      }
              discovered <- discoverMcpServers (\_ -> pure ()) [server]
              received <- newIORef Nothing
              withPseudoTerminalOutput $ \readOutput -> do
                let runtime =
                      ToolRuntime
                        { toolRuntimeJobs = jobs
                        , toolRuntimeWorkingDirectory = directory
                        , toolRuntimeContext = MainContext
                        , toolRuntimeVerbosity = VerbosityNormal
                        , toolRuntimeSubagent = \_ _ -> pure (Left "not configured")
                        , toolRuntimeHandler =
                            \_ _ _ _ raw -> do
                              writeIORef received (Just raw)
                              pure (Right "handled\n")
                        }
                    catalog =
                      withMcpServers discovered
                        (toolCatalog (defaultToolConfig {toolMcp = [server]}))
                (_, conversation) <-
                  runToolCallsWith
                    runtime
                    catalog
                    emptyConversation
                    [ToolCall "call-1" "mcp__demo__search" "{\"query\":\"haskell\"}"]
                output <- readOutput
                let plain = stripAnsi output
                assertBool
                  "the MCP call is shown"
                  ("mcp.demo.search" `isInfixOf` plain)
                assertBool
                  "the raw result has no output label of its own"
                  (not ("mcp.demo.search.output" `isInfixOf` plain))
                assertBool
                  "the raw result is not printed outside the handler"
                  (not ("Found results" `isInfixOf` plain))
                readIORef received >>= (@?= Just "Found results\n")
                conversation @?= [ToolResultEntry "call-1" "handled\n"]
    , testCase "a denied MCP server is not contacted" $ do
        let server =
              McpServerConfig
                { mcpServerName = "denied"
                , mcpServerUrl = "http://127.0.0.1:1/mcp"
                , mcpServerPolicy = ToolDeny
                , mcpHandlers = HandlerConfigs Map.empty
                }
        logs <- newIORef []
        discovered <-
          discoverMcpServers
            (\message -> modifyIORef' logs (message :))
            [server]
        discovered @?= [McpServer server [] Nothing]
        readIORef logs >>= (@?= [])
    ]

-- * Fake MCP server

withMcpUpstream :: (Int -> IORef [ByteString] -> IO a) -> IO a
withMcpUpstream action =
  bracket start stop $ \(port, requests, _) -> action port requests
  where
    start = do
      listener <- socket AF_INET Stream defaultProtocol
      setSocketOption listener ReuseAddr 1
      bind listener (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
      listen listener 8
      port <- fromIntegral <$> socketPort listener
      requests <- newIORef []
      _ <- forkIO $ serveMany listener requests 4
      pure (port, requests, listener)
    stop (_, _, listener) = close listener `catch` \(_ :: SomeException) -> pure ()

serveMany :: Socket -> IORef [ByteString] -> Int -> IO ()
serveMany listener requests = go
  where
    go 0 = pure ()
    go remaining =
      ( do
          (connection, _) <- accept listener
          request <- readHttpRequest connection
          modifyIORef' requests (request :)
          sendAll connection (responseFor request)
          close connection
          go (remaining - 1)
      )
        `catch` \(_ :: SomeException) -> pure ()

responseFor :: ByteString -> ByteString
responseFor request
  | "notifications/initialized" `BS.isInfixOf` request =
      "HTTP/1.1 202 Accepted\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
  | "initialize" `BS.isInfixOf` request =
      sseResponse
        True
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"protocolVersion\":\"2024-11-05\",\"capabilities\":{},\"serverInfo\":{\"name\":\"fake\",\"version\":\"1\"}}}"
  | "tools/list" `BS.isInfixOf` request =
      sseResponse
        False
        "{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{\"tools\":[{\"name\":\"search\",\"description\":\"Search.\",\"inputSchema\":{\"type\":\"object\",\"properties\":{\"query\":{\"type\":\"string\"}},\"required\":[\"query\"]}}]}}"
  | "tools/call" `BS.isInfixOf` request =
      sseResponse
        False
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"Found results\"}]}}"
  | otherwise =
      "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"

sseResponse :: Bool -> ByteString -> ByteString
sseResponse withSession payload =
  BS.concat
    [ "HTTP/1.1 200 OK\r\n"
    , "Content-Type: text/event-stream\r\n"
    , if withSession then "Mcp-Session-Id: test-session\r\n" else ""
    , "Connection: close\r\n\r\n"
    , "data: \nid: 0\nretry: 3000\n\n"
    , "event: message\n"
    , "data: "
    , payload
    , "\n\n"
    ]

readHttpRequest :: Socket -> IO ByteString
readHttpRequest connection = do
  headResult <- recvUntil connection "\r\n\r\n" maxHead
  case headResult of
    Left _ -> pure BS.empty
    Right (rawHead, leftover) ->
      case contentLengthOf rawHead of
        Nothing -> pure rawHead
        Just bodyLength -> do
          bodyResult <- recvExactly connection bodyLength leftover
          pure (rawHead <> fromRight BS.empty bodyResult)

maxHead :: Int
maxHead = 64 * 1024

contentLengthOf :: ByteString -> Maybe Int
contentLengthOf rawHead =
  listToMaybe
    [ parsed
    | line <- BS8.lines rawHead
    , let (name, rest) = BS8.break (== ':') line
    , BS8.map toLower name == "content-length"
    , let value = BS8.dropWhile (== ' ') (BS8.drop 1 rest)
    , Just parsed <- [readMaybe (BS8.unpack value)]
    ]

-- Local minimal wire helpers; they mirror the ones used by the relay tests.
recvUntil :: Socket -> ByteString -> Int -> IO (Either String (ByteString, ByteString))
recvUntil connection marker limit = go BS.empty
  where
    go buffered
      | BS.length buffered > limit = pure (Left "marker limit exceeded")
      | otherwise = do
          chunk <- recvSome connection
          if BS.null chunk
            then pure (Left "connection closed before marker")
            else do
              let accumulated = buffered <> chunk
              case BS.breakSubstring marker accumulated of
                (before, remainder)
                  | BS.null remainder -> go accumulated
                  | otherwise -> pure (Right (before <> marker, BS.drop (BS.length marker) remainder))

recvExactly :: Socket -> Int -> ByteString -> IO (Either String ByteString)
recvExactly connection count buffered
  | BS.length buffered >= count = pure (Right (BS.take count buffered))
  | otherwise = do
      chunk <- recvSome connection
      if BS.null chunk
        then pure (Left "connection closed before body")
        else recvExactly connection count (buffered <> chunk)

recvSome :: Socket -> IO ByteString
recvSome connection = recv connection 32768
