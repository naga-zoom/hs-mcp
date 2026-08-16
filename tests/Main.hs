{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Concurrent.STM
import Control.Exception (SomeException, throwIO, try)
import Data.Aeson
import Data.Text (Text)
import Data.Text qualified as T
import Network.MCP.Server
import Network.MCP.Server.HTTP (runServerWithHTTP)
import Network.MCP.Server.Transport
import Network.MCP.Server.Types
import Network.MCP.Transport.HTTP (httpConfigPort)
import Network.MCP.Transport.Types
import Network.MCP.Types
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Exit
import Test.HUnit
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  counts <- runTestTT allTests
  if errors counts + failures counts > 0
    then exitFailure
    else exitSuccess

allTests :: Test
allTests = TestList
  [ TestLabel "JSON Serialization Tests" jsonTests
  , TestLabel "Protocol Tests" protocolTests
  , TestLabel "handleRequest total-function tests" handleRequestTests
  , TestLabel "Transport selection tests" transportTests
  , TestLabel "Streamable HTTP transport tests" httpTransportTests
  ]

-- JSON Serialization Tests
jsonTests :: Test
jsonTests = TestList
  [ TestLabel "Protocol Version" testProtocolVersion
  , TestLabel "Protocol Version Negotiation" testProtocolVersionNegotiation
  , TestLabel "Server Info" testServerInfo
  , TestLabel "Client Info" testClientInfo
  , TestLabel "Resource" testResource
  , TestLabel "Tool" testTool
  , TestLabel "Prompt" testPrompt
  , TestLabel "Request/Response" testRequests
  , TestLabel "Structured chart result" testStructuredChartResult
  ]

-- Note: this test previously constructed 'ProtocolVersion 0 1' as if it
-- were a two-field data constructor. 'ProtocolVersion' is actually
-- `type ProtocolVersion = Text` (e.g. "2024-11-05") in
-- Network.MCP.Types -- it never had constructor arity 2, so the old
-- assertion could never have compiled against the real type and has been
-- corrected to match the real, intended representation.
testProtocolVersion :: Test
testProtocolVersion = TestCase $ do
  let version = "2024-11-05" :: ProtocolVersion
      encoded = encode version
      decoded = decode encoded :: Maybe ProtocolVersion

  assertEqual "Protocol version should encode and decode correctly" (Just version) decoded

testProtocolVersionNegotiation :: Test
testProtocolVersionNegotiation = TestCase $ do
  assertEqual "latest supported version is echoed exactly" (Right "2025-06-18") (negotiateProtocolVersion "2025-06-18")
  assertEqual "supported versions are echoed exactly" (Right "2024-11-05") (negotiateProtocolVersion "2024-11-05")
  assertEqual "unsupported versions are rejected" (Left "unsupported protocol version: 2026-07-28") (negotiateProtocolVersion "2026-07-28")

testServerInfo :: Test
testServerInfo = TestCase $ do
  let info = Implementation "test-server" "1.0.0"
      encoded = encode info
      decoded = decode encoded :: Maybe Implementation

  assertEqual "Server info should encode and decode correctly" (Just info) decoded

testClientInfo :: Test
testClientInfo = TestCase $ do
  let info = Implementation "test-client" "1.0.0"
      encoded = encode info
      decoded = decode encoded :: Maybe Implementation

  assertEqual "Client info should encode and decode correctly" (Just info) decoded

testResource :: Test
testResource = TestCase $ do
  let resource = Resource
        { resourceUri = "file:///test/resource"
        , resourceName = "Test Resource"
        , resourceDescription = Just "A test resource"
        , resourceMimeType = Just "text/plain"
        , resourceTemplate = Nothing
        }
      encoded = encode resource
      decoded = decode encoded :: Maybe Resource

  assertEqual "Resource should encode and decode correctly" (Just resource) decoded

testTool :: Test
testTool = TestCase $ do
  let tool = Tool
        { toolName = "test-tool"
        , toolDescription = Just "A test tool"
        , toolInputSchema = "{\"type\":\"object\",\"properties\":{\"param\":{\"type\":\"string\"}}}"
        , toolAnnotations = Nothing
        }
      encoded = encode tool
      decoded = decode encoded :: Maybe Tool

  assertEqual "Tool should encode and decode correctly" (Just tool) decoded

testPrompt :: Test
testPrompt = TestCase $ do
  let promptArg = PromptArgument
        { promptArgumentName = "testArg"
        , promptArgumentDescription = Just "A test argument"
        , promptArgumentRequired = True
        }
      prompt = Prompt
        { promptName = "test-prompt"
        , promptDescription = Just "A test prompt"
        , promptArguments = [promptArg]
        }
      encoded = encode prompt
      decoded = decode encoded :: Maybe Prompt

  assertEqual "Prompt should encode and decode correctly" (Just prompt) decoded

testRequests :: Test
testRequests = TestList
  [ TestLabel "Initialize Request" testInitializeRequest
  , TestLabel "List Resources Request/Result" testListResourcesRequestResult
  , TestLabel "Read Resource Request/Result" testReadResourceRequestResult
  , TestLabel "Call Tool Request/Result" testCallToolRequestResult
  ]

testInitializeRequest :: Test
testInitializeRequest = TestCase $ do
  let clientOpts = ClientInitializeOptions
        { clientInitProtocolVersion = "2024-11-05"
        , clientInitInfo = Implementation "test-client" "1.0.0"
        , clientInitCapabilities = ClientCapabilities Nothing Nothing
        }
      encoded = encode clientOpts
      decoded = decode encoded :: Maybe ClientInitializeOptions

  assertEqual "Initialize options should encode and decode correctly" (Just clientOpts) decoded

testListResourcesRequestResult :: Test
testListResourcesRequestResult = TestCase $ do
  let request = ListResourcesRequest
      resource = Resource
        { resourceUri = "file:///test/resource"
        , resourceName = "Test Resource"
        , resourceDescription = Just "A test resource"
        , resourceMimeType = Just "text/plain"
        , resourceTemplate = Nothing
        }
      result = ListResourcesResult [resource]
      encodedRequest = encode request
      encodedResult = encode result
      decodedRequest = decode encodedRequest :: Maybe ListResourcesRequest
      decodedResult = decode encodedResult :: Maybe ListResourcesResult

  assertEqual "List resources request should encode and decode correctly" (Just request) decodedRequest
  assertEqual "List resources result should encode and decode correctly" (Just result) decodedResult

testReadResourceRequestResult :: Test
testReadResourceRequestResult = TestCase $ do
  let request = ReadResourceRequest "file:///test/resource"
      content = ResourceContent
        { resourceContentUri = "file:///test/resource"
        , resourceContentMimeType = Just "text/plain"
        , resourceContentText = Just "Test content"
        , resourceContentBlob = Nothing
        }
      result = ReadResourceResult [content]
      encodedRequest = encode request
      encodedResult = encode result
      decodedRequest = decode encodedRequest :: Maybe ReadResourceRequest
      decodedResult = decode encodedResult :: Maybe ReadResourceResult

  assertEqual "Read resource request should encode and decode correctly" (Just request) decodedRequest
  assertEqual "Read resource result should encode and decode correctly" (Just result) decodedResult

-- Note: this test previously passed a 'Map Text Value' as the second
-- 'CallToolRequest' field. 'callToolArguments' is `:: Value` in
-- Network.MCP.Types (the raw JSON-RPC arguments object) -- not a Map -- so
-- the old code could never have type-checked. Corrected to construct the
-- 'Value' directly, matching the real, intended representation.
testCallToolRequestResult :: Test
testCallToolRequestResult = TestCase $ do
  let request = CallToolRequest "test-tool" (object ["param" .= String "value"])
      content = ToolContent
        { toolContentType = TextualContent
        , toolContentText = Just "Test result"
        , toolContentResource = Nothing
        }
      result = CallToolResult [content] False Nothing
      encodedRequest = encode request
      encodedResult = encode result
      decodedRequest = decode encodedRequest :: Maybe CallToolRequest
      decodedResult = decode encodedResult :: Maybe CallToolResult

  assertEqual "Call tool request should encode and decode correctly" (Just request) decodedRequest
  assertEqual "Call tool result should encode and decode correctly" (Just result) decodedResult

testStructuredChartResult :: Test
testStructuredChartResult = TestCase $ do
  let chart = object ["$schema" .= String "https://vega.github.io/schema/vega-lite/v5.json", "mark" .= String "bar"]
      resource = ResourceContent
        { resourceContentUri = "urn:semantic-compiler:chart"
        , resourceContentMimeType = Just "application/vnd.vegalite.v5+json"
        , resourceContentText = Just (T.pack (show chart))
        , resourceContentBlob = Nothing
        }
      content = ToolContent EmbeddedResource Nothing (Just resource)
      result = CallToolResult [content] False (Just (object ["chart" .= chart]))
      decoded = decode (encode result) :: Maybe CallToolResult
  assertEqual "structured chart result should round-trip" (Just result) decoded

-- Protocol Tests
protocolTests :: Test
protocolTests = TestList
  [ TestLabel "Server Creation" testServerCreation
  , TestLabel "Resource Registration" testResourceRegistration
  , TestLabel "Tool Registration" testToolRegistration
  , TestLabel "Prompt Registration" testPromptRegistration
  ]

testServerCreation :: Test
testServerCreation = TestCase $ do
  let serverImp = Implementation "test-server" "1.0.0"
      serverCap = ServerCapabilities
        { resourcesCapability = Just $ ResourcesCapability True
        , toolsCapability = Just $ ToolsCapability True
        , promptsCapability = Just $ PromptsCapability True
        }

  server <- createServer serverImp serverCap "No instructions."

  assertEqual "Server name should match" "test-server" (serverName $ serverInfo server)
  assertEqual "Server version should match" "1.0.0" (serverVersion $ serverInfo server)

  let caps = serverCapabilities server
  assertEqual "Resources capability should be enabled"
             (Just $ ResourcesCapability True)
             (resourcesCapability caps)
  assertEqual "Tools capability should be enabled"
             (Just $ ToolsCapability True)
             (toolsCapability caps)
  assertEqual "Prompts capability should be enabled"
             (Just $ PromptsCapability True)
             (promptsCapability caps)

testResourceRegistration :: Test
testResourceRegistration = TestCase $ do
  let serverInfo = Implementation "test-server" "1.0.0"
      serverCapabilities = ServerCapabilities
        { resourcesCapability = Just $ ResourcesCapability True
        , toolsCapability = Nothing
        , promptsCapability = Nothing
        }
      resource = Resource
        { resourceUri = "file:///test/resource"
        , resourceName = "Test Resource"
        , resourceDescription = Just "A test resource"
        , resourceMimeType = Just "text/plain"
        , resourceTemplate = Nothing
        }

  server <- createServer serverInfo serverCapabilities "No instructions."
  registerResources server [resource]

  resources <- readTVarIO (serverResources server)
  assertEqual "Resources should be registered" 1 (length resources)
  assertEqual "Resource URI should match" "file:///test/resource" (resourceUri $ head resources)

testToolRegistration :: Test
testToolRegistration = TestCase $ do
  let serverInfo = Implementation "test-server" "1.0.0"
      serverCapabilities = ServerCapabilities
        { resourcesCapability = Nothing
        , toolsCapability = Just $ ToolsCapability True
        , promptsCapability = Nothing
        }
      tool = Tool
        { toolName = "test-tool"
        , toolDescription = Just "A test tool"
        , toolInputSchema = "{\"type\":\"object\",\"properties\":{\"param\":{\"type\":\"string\"}}}"
        , toolAnnotations = Nothing
        }

  server <- createServer serverInfo serverCapabilities "No instructions."
  registerTools server [tool]

  tools <- readTVarIO (serverTools server)
  assertEqual "Tools should be registered" 1 (length tools)
  assertEqual "Tool name should match" "test-tool" (toolName $ head tools)

testPromptRegistration :: Test
testPromptRegistration = TestCase $ do
  let serverInfo = Implementation "test-server" "1.0.0"
      serverCapabilities = ServerCapabilities
        { resourcesCapability = Nothing
        , toolsCapability = Nothing
        , promptsCapability = Just $ PromptsCapability True
        }
      promptArg = PromptArgument
        { promptArgumentName = "testArg"
        , promptArgumentDescription = Just "A test argument"
        , promptArgumentRequired = True
        }
      prompt = Prompt
        { promptName = "test-prompt"
        , promptDescription = Just "A test prompt"
        , promptArguments = [promptArg]
        }

  server <- createServer serverInfo serverCapabilities "No instructions."
  registerPrompts server [prompt]

  prompts <- readTVarIO (serverPrompts server)
  assertEqual "Prompts should be registered" 1 (length prompts)
  assertEqual "Prompt name should match" "test-prompt" (promptName $ head prompts)

-- ---------------------------------------------------------------------------
-- handleRequest total-function tests
--
-- Regression coverage for the partial-function bug found in
-- Network.MCP.Server.handleRequest: a business-level error reported by a
-- registered handler (e.g. "Invalid tool call parameters") used to be
-- matched with `error (show _err)` -- crashing the request path instead of
-- producing a JSON-RPC error response. handleRequest is now total: both
-- thrown exceptions and handler-reported Lefts flow through the same
-- `Left err` result, which callers (Network.MCP.Server.handleMessage) turn
-- into a normal JSON-RPC error response.
-- ---------------------------------------------------------------------------

handleRequestTests :: Test
handleRequestTests = TestList
  [ TestLabel "Invalid tool-call params produce an error result, not a crash" testInvalidToolCallParamsIsError
  ]

testInvalidToolCallParamsIsError :: Test
testInvalidToolCallParamsIsError = TestCase $ do
  let info = Implementation "test-server" "1.0.0"
      caps = ServerCapabilities Nothing (Just $ ToolsCapability True) Nothing
  server <- createServer info caps "No instructions."
  registerToolCallHandler server $ \_mTok _req -> return $ CallToolResult [] False Nothing

  -- "tools/call" with no "arguments"/"name" fields: fromJSON fails inside
  -- registerCallToolHandler, which reports `Left (toException (userError
  -- ...))`. Before the fix this crashed handleRequest via `error`.
  let badRequest =
        Request
          { requestJsonrpc = JSONRPC "2.0"
          , requestId = Number 1
          , requestMethod = "tools/call"
          , requestParams = Just (object [])
          }

  outcome <- try (handleRequest server Nothing badRequest)
    :: IO (Either SomeException (Either SomeException Response))
  case outcome of
    Left crashed -> assertFailure $ "handleRequest crashed instead of returning an error result: " ++ show crashed
    Right (Left _businessErr) -> pure () -- expected: reported, not thrown
    Right (Right resp) -> assertFailure $ "expected an error result, got: " ++ show resp

-- ---------------------------------------------------------------------------
-- Transport selection tests (Network.MCP.Server.Transport)
-- ---------------------------------------------------------------------------

transportTests :: Test
transportTests = TestList
  [ TestLabel "mkHttpConfig accepts a valid port" testMkHttpConfigValid
  , TestLabel "mkHttpConfig rejects an out-of-range port" testMkHttpConfigInvalidPort
  , TestLabel "TransportChoice distinguishes Stdio from Http" testTransportChoiceShape
  ]

testMkHttpConfigValid :: Test
testMkHttpConfigValid = TestCase $
  case mkHttpConfig 8080 Nothing of
    Left err -> assertFailure $ "expected a valid config, got: " ++ show err
    Right cfg -> assertEqual "port should round-trip" 8080 (httpConfigPort cfg)

testMkHttpConfigInvalidPort :: Test
testMkHttpConfigInvalidPort = TestCase $
  case mkHttpConfig 0 Nothing of
    Left _ -> pure ()
    Right cfg -> assertFailure $ "expected port 0 to be rejected, got a config on port " ++ show (httpConfigPort cfg)

testTransportChoiceShape :: Test
testTransportChoiceShape = TestCase $
  case mkHttpConfig 9999 Nothing of
    Left err -> assertFailure (show err)
    Right cfg -> do
      let choices = [Stdio, Http cfg]
      assertEqual "Stdio and Http are distinct constructors" 2 (length choices)
      assertBool "Stdio is not Http" (Stdio /= Http cfg)

-- ---------------------------------------------------------------------------
-- Streamable HTTP transport tests: a live server, driven over a real TCP
-- socket (no mocking of the transport layer).
-- ---------------------------------------------------------------------------

httpTransportTests :: Test
httpTransportTests = TestList
  [ TestLabel "POSTing a JSON-RPC request over HTTP returns a JSON-RPC response" testHttpRoundTrip
  , TestLabel "A configured bearer token rejects unauthenticated requests" testHttpUnauthorized
  , TestLabel "A per-request Authorization header reaches the tool-call handler" testHttpPerRequestTokenReachesToolHandler
  , TestLabel "No Authorization header means the tool-call handler sees Nothing" testHttpNoTokenReachesToolHandlerAsNothing
  ]

-- | Task: per-call OAuth plumbing. The server has NO configured bearer
-- token (so the transport's own auth *gate* is open to everyone, per
-- 'authorized'\'s existing "Nothing -> True" behavior) -- but a caller's
-- own real @Authorization: Bearer ...@ header must still reach the
-- registered 'ToolCallHandler' for that one call, so a tool implementation
-- (e.g. semantic-compiler's execute_query) can build a per-call credential
-- from it. This is real typed plumbing through 'runHTTPServer'/
-- 'handleMessage' -- not a global/IORef side channel.
testHttpPerRequestTokenReachesToolHandler :: Test
testHttpPerRequestTokenReachesToolHandler = TestCase $ do
  case mkHttpConfig 18082 Nothing of
    Left err -> assertFailure (show err)
    Right cfg -> do
      seenTokenVar <- newTVarIO Nothing
      server <- createServer
        (Implementation "http-test-server" "1.0.0")
        (ServerCapabilities Nothing (Just (ToolsCapability False)) Nothing)
        "No instructions."
      registerToolCallHandler server $ \mTok _req -> do
        atomically $ writeTVar seenTokenVar mTok
        pure $ CallToolResult [] False Nothing
      tid <- forkIO $ runServerWithHTTP server cfg
      threadDelay 200000
      _ <- httpPostRawWithAuth 18082 (Just "user-supplied-token") toolCallRequestBody
      killThread tid
      seen <- readTVarIO seenTokenVar
      assertEqual "handler should see the real per-request bearer token" (Just (BearerToken "user-supplied-token")) seen

testHttpNoTokenReachesToolHandlerAsNothing :: Test
testHttpNoTokenReachesToolHandlerAsNothing = TestCase $ do
  case mkHttpConfig 18083 Nothing of
    Left err -> assertFailure (show err)
    Right cfg -> do
      seenTokenVar <- newTVarIO (Just (BearerToken "sentinel-should-be-overwritten"))
      server <- createServer
        (Implementation "http-test-server" "1.0.0")
        (ServerCapabilities Nothing (Just (ToolsCapability False)) Nothing)
        "No instructions."
      registerToolCallHandler server $ \mTok _req -> do
        atomically $ writeTVar seenTokenVar mTok
        pure $ CallToolResult [] False Nothing
      tid <- forkIO $ runServerWithHTTP server cfg
      threadDelay 200000
      _ <- httpPostRawWithAuth 18083 Nothing toolCallRequestBody
      killThread tid
      seen <- readTVarIO seenTokenVar
      assertEqual "no Authorization header means the handler sees Nothing, not a stale/default token" Nothing seen

toolCallRequestBody :: BL.ByteString
toolCallRequestBody =
  encode $ object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id" .= (1 :: Int)
    , "method" .= ("tools/call" :: Text)
    , "params" .= object
        [ "name" .= ("noop" :: Text)
        , "arguments" .= object []
        ]
    ]

testHttpRoundTrip :: Test
testHttpRoundTrip = TestCase $ do
  case mkHttpConfig 18080 Nothing of
    Left err -> assertFailure (show err)
    Right cfg -> do
      server <- createServer
        (Implementation "http-test-server" "1.0.0")
        (ServerCapabilities Nothing Nothing Nothing)
        "No instructions."
      tid <- forkIO $ runServerWithHTTP server cfg
      threadDelay 200000 -- let the listener come up
      raw <- httpPostRaw 18080 initializeRequestBody
      killThread tid
      assertBool ("expected a 200 status line, got: " ++ BS8.unpack raw) (BS8.pack "200" `BS.isInfixOf` raw)
      case decode (responseBody raw) :: Maybe Value of
        Just (Object _) -> pure ()
        other -> assertFailure $ "expected a JSON object response body, got: " ++ show other

testHttpUnauthorized :: Test
testHttpUnauthorized = TestCase $ do
  case mkHttpConfig 18081 (Just (BearerToken "secret-token")) of
    Left err -> assertFailure (show err)
    Right cfg -> do
      server <- createServer
        (Implementation "http-test-server" "1.0.0")
        (ServerCapabilities Nothing Nothing Nothing)
        "No instructions."
      tid <- forkIO $ runServerWithHTTP server cfg
      threadDelay 200000
      raw <- httpPostRaw 18081 initializeRequestBody
      killThread tid
      assertBool ("expected a 401 status line, got: " ++ BS8.unpack raw) (BS8.pack "401" `BS.isInfixOf` raw)

initializeRequestBody :: BL.ByteString
initializeRequestBody =
  encode $ object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id" .= (1 :: Int)
    , "method" .= ("initialize" :: Text)
    , "params" .= object
        [ "protocolVersion" .= ("2024-11-05" :: Text)
        , "clientInfo" .= object ["name" .= ("test-client" :: Text), "version" .= ("1.0.0" :: Text)]
        , "capabilities" .= object []
        ]
    ]

-- | Send a raw HTTP/1.1 POST over a real TCP socket and return the full
-- raw response (status line + headers + body) so tests can assert on
-- status codes as well as body content.
httpPostRaw :: Int -> BL.ByteString -> IO BS.ByteString
httpPostRaw port body = do
  addrs <- getAddrInfo (Just (defaultHints { addrSocketType = Stream })) (Just "127.0.0.1") (Just (show port))
  case addrs of
    [] -> throwIO (userError "no address info for 127.0.0.1")
    (addr : _) -> do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock (addrAddress addr)
      let bodyBytes = BL.toStrict body
          req =
            BS8.pack $
              "POST / HTTP/1.1\r\n"
                ++ "Host: 127.0.0.1\r\n"
                ++ "Content-Type: application/json\r\n"
                ++ "Content-Length: "
                ++ show (BS.length bodyBytes)
                ++ "\r\n\r\n"
      sendAll sock (req <> bodyBytes)
      resp <- readAll sock BS.empty
      close sock
      pure resp
  where
    readAll sock acc = do
      chunk <- recv sock 4096
      if BS.null chunk then pure acc else readAll sock (acc <> chunk)

-- | Same as 'httpPostRaw', but optionally sets a real
-- @Authorization: Bearer ...@ header, for exercising per-request-token
-- plumbing.
httpPostRawWithAuth :: Int -> Maybe Text -> BL.ByteString -> IO BS.ByteString
httpPostRawWithAuth port mToken body = do
  addrs <- getAddrInfo (Just (defaultHints { addrSocketType = Stream })) (Just "127.0.0.1") (Just (show port))
  case addrs of
    [] -> throwIO (userError "no address info for 127.0.0.1")
    (addr : _) -> do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock (addrAddress addr)
      let bodyBytes = BL.toStrict body
          authHeader = case mToken of
            Nothing -> ""
            Just t -> "Authorization: Bearer " ++ T.unpack t ++ "\r\n"
          req =
            BS8.pack $
              "POST / HTTP/1.1\r\n"
                ++ "Host: 127.0.0.1\r\n"
                ++ "Content-Type: application/json\r\n"
                ++ authHeader
                ++ "Content-Length: "
                ++ show (BS.length bodyBytes)
                ++ "\r\n\r\n"
      sendAll sock (req <> bodyBytes)
      resp <- readAll sock BS.empty
      close sock
      pure resp
  where
    readAll sock acc = do
      chunk <- recv sock 4096
      if BS.null chunk then pure acc else readAll sock (acc <> chunk)

-- | Extract just the body from a raw HTTP response (everything after the
-- first blank line).
responseBody :: BS.ByteString -> BL.ByteString
responseBody raw =
  let sep = BS8.pack "\r\n\r\n"
      (_, rest) = BS.breakSubstring sep raw
   in if BS.null rest
        then BL.fromStrict raw
        else BL.fromStrict (BS.drop (BS.length sep) rest)
