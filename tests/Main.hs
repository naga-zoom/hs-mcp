{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (forkIO, killThread, newEmptyMVar, putMVar, takeMVar, threadDelay, tryPutMVar)
import Control.Concurrent.STM
import Control.Exception (SomeException, throwIO, try)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseMaybe)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Network.MCP.Client.Types (McpClientError (..))
import Network.MCP.Server
import Network.MCP.Server.HTTP (runServerWithHTTP)
import Network.MCP.Server.Transport
import Network.MCP.Server.Types
import Network.MCP.Transport.HTTP
  ( BearerToken (..)
  , HttpConfig (httpConfigHost, httpConfigPort)
  , connectHttpClient
  , mkHttpClient
  , sendHttpClientRequest
  )
import Network.MCP.Transport.Types
import Network.MCP.Types
import Network.HTTP.Types.Header (hAuthorization, hContentLength, hContentType)
import Network.HTTP.Types.Status (mkStatus)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import System.Exit
import System.Timeout (timeout)
import Test.HUnit

main :: IO ()
main = do
  counts <- runTestTT allTests
  if errors counts + failures counts > 0
    then exitFailure
    else exitSuccess

allTests :: Test
allTests =
  TestList
    [ TestLabel "JSON Serialization Tests" jsonTests
    , TestLabel "Protocol Tests" protocolTests
    , TestLabel "handleRequest total-function tests" handleRequestTests
    , TestLabel "Transport selection tests" transportTests
    , TestLabel "Streamable HTTP transport tests" httpTransportTests
    , TestLabel "Streamable HTTP client tests" httpClientTests
    ]

-- JSON Serialization Tests
jsonTests :: Test
jsonTests =
  TestList
    [ TestLabel "Protocol Version" testProtocolVersion
    , TestLabel "Protocol Version Negotiation" testProtocolVersionNegotiation
    , TestLabel "Server Info" testServerInfo
    , TestLabel "Client Info" testClientInfo
    , TestLabel "Resource" testResource
    , TestLabel "Tool" testTool
    , TestLabel "tools/list exposes MCP hint fields to clients" testToolAnnotationsInteroperability
    , TestLabel "canonical incoming tool hints decode" testCanonicalIncomingToolAnnotations
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
  assertEqual
    "latest supported version is echoed exactly"
    (Right "2025-06-18")
    (negotiateProtocolVersion "2025-06-18")
  assertEqual
    "supported versions are echoed exactly"
    (Right "2024-11-05")
    (negotiateProtocolVersion "2024-11-05")
  assertEqual
    "unsupported versions are rejected"
    (Left "unsupported protocol version: 2026-07-28")
    (negotiateProtocolVersion "2026-07-28")

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
  let resource =
        Resource
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
  let tool =
        Tool
          { toolName = "test-tool"
          , toolDescription = Just "A test tool"
          , toolInputSchema = "{\"type\":\"object\",\"properties\":{\"param\":{\"type\":\"string\"}}}"
          , toolAnnotations = Nothing
          }
      encoded = encode tool
      decoded = decode encoded :: Maybe Tool

  assertEqual "Tool should encode and decode correctly" (Just tool) decoded

-- A client reading tools/list under MCP 2025-06-18 uses the four *Hint
-- fields to classify the advertised tool. This goes through the real server
-- handlers so a changed wire codec cannot pass by round-tripping itself.
testToolAnnotationsInteroperability :: Test
testToolAnnotationsInteroperability = TestCase $ do
  let info = Implementation "test-server" "1.0.0"
      caps = ServerCapabilities Nothing (Just $ ToolsCapability True) Nothing
      annotations = ToolAnnotations Nothing (Just False) (Just False) (Just True) (Just False)
      tool = Tool "append-entry" (Just "Adds an entry") (object ["type" .= String "object"]) (Just annotations)
      initialize =
        Request (JSONRPC "2.0") (Number 1) "initialize"
          (Just $ toJSON $ ClientInitializeOptions "2025-06-18" (Implementation "test-client" "1.0.0") (ClientCapabilities Nothing Nothing))
      list = Request (JSONRPC "2.0") (Number 2) "tools/list" Nothing

  server <- createServer info caps "No instructions."
  initialized <- handleRequest server Nothing initialize
  case initialized of
    Left err -> assertFailure $ "initialize failed: " ++ show err
    Right response ->
      case response.responseResult >>= parseMaybe parseJSON of
        Just (options :: ServerInitializeOptions) ->
          assertEqual "protocol negotiation" "2025-06-18" options.serverInitProtocolVersion
        Nothing -> assertFailure "initialize did not return server options"

  registerTools server [tool]
  listed <- handleRequest server Nothing list
  case listed of
    Left err -> assertFailure $ "tools/list failed: " ++ show err
    Right response -> do
      assertEqual "tools/list emits canonical optional fields"
        (Just $ object ["tools" .= [object
          [ "name" .= String "append-entry"
          , "description" .= String "Adds an entry"
          , "inputSchema" .= object ["type" .= String "object"]
          , "annotations" .= object
              [ "readOnlyHint" .= False
              , "destructiveHint" .= False
              , "idempotentHint" .= True
              , "openWorldHint" .= False
              ]
          ]]])
        response.responseResult
      let clientClassifiesRepeatableLocalAppend = response.responseResult >>= parseMaybe
            (withObject "tools/list" $ \result -> do
              tools <- result .: "tools" :: Parser [Value]
              case tools of
                [oneTool] -> withObject "tool" (\listedTool -> do
                  hints <- listedTool .: "annotations"
                  withObject "tool annotations" (\a -> do
                    readOnly <- a .: "readOnlyHint"
                    destructive <- a .: "destructiveHint"
                    idempotent <- a .: "idempotentHint"
                    openWorld <- a .: "openWorldHint"
                    pure (not readOnly && not destructive && idempotent && not openWorld)) hints) oneTool
                _ -> fail "expected one tool")
      assertEqual "a spec client recognizes an additive, repeatable, closed-world tool" (Just True) clientClassifiesRepeatableLocalAppend

testCanonicalIncomingToolAnnotations :: Test
testCanonicalIncomingToolAnnotations = TestCase $ do
  let canonicalTool =
        object
          [ "name" .= String "append-entry"
          , "inputSchema" .= object ["type" .= String "object"]
          , "annotations" .= object
              [ "readOnlyHint" .= False
              , "destructiveHint" .= False
              , "idempotentHint" .= True
              , "openWorldHint" .= False
              ]
          ]
  case fromJSON canonicalTool of
    Error err -> assertFailure $ "canonical tool did not decode: " ++ err
    Success (received :: Tool) ->
      assertEqual "canonical incoming hints retain client semantics"
        (Just (ToolAnnotations Nothing (Just False) (Just False) (Just True) (Just False)))
        received.toolAnnotations

testPrompt :: Test
testPrompt = TestCase $ do
  let promptArg =
        PromptArgument
          { promptArgumentName = "testArg"
          , promptArgumentDescription = Just "A test argument"
          , promptArgumentRequired = True
          }
      prompt =
        Prompt
          { promptName = "test-prompt"
          , promptDescription = Just "A test prompt"
          , promptArguments = [promptArg]
          }
      encoded = encode prompt
      decoded = decode encoded :: Maybe Prompt

  assertEqual "Prompt should encode and decode correctly" (Just prompt) decoded

testRequests :: Test
testRequests =
  TestList
    [ TestLabel "Initialize Request" testInitializeRequest
    , TestLabel "List Resources Request/Result" testListResourcesRequestResult
    , TestLabel "Read Resource Request/Result" testReadResourceRequestResult
    , TestLabel "Call Tool Request/Result" testCallToolRequestResult
    ]

testInitializeRequest :: Test
testInitializeRequest = TestCase $ do
  let clientOpts =
        ClientInitializeOptions
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
      resource =
        Resource
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

  assertEqual
    "List resources request should encode and decode correctly"
    (Just request)
    decodedRequest
  assertEqual "List resources result should encode and decode correctly" (Just result) decodedResult

testReadResourceRequestResult :: Test
testReadResourceRequestResult = TestCase $ do
  let request = ReadResourceRequest "file:///test/resource"
      content =
        ResourceContent
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
      content =
        ToolContent
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
  let chart =
        object
          ["$schema" .= String "https://vega.github.io/schema/vega-lite/v5.json", "mark" .= String "bar"]
      resource =
        ResourceContent
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
protocolTests =
  TestList
    [ TestLabel "Server Creation" testServerCreation
    , TestLabel "Resource Registration" testResourceRegistration
    , TestLabel "Tool Registration" testToolRegistration
    , TestLabel "Prompt Registration" testPromptRegistration
    ]

testServerCreation :: Test
testServerCreation = TestCase $ do
  let serverImp = Implementation "test-server" "1.0.0"
      serverCap =
        ServerCapabilities
          { resourcesCapability = Just $ ResourcesCapability True
          , toolsCapability = Just $ ToolsCapability True
          , promptsCapability = Just $ PromptsCapability True
          }

  server <- createServer serverImp serverCap "No instructions."

  assertEqual "Server name should match" "test-server" server.serverInfo.serverName
  assertEqual "Server version should match" "1.0.0" server.serverInfo.serverVersion

  let caps = server.serverCapabilities
  assertEqual
    "Resources capability should be enabled"
    (Just $ ResourcesCapability True)
    caps.resourcesCapability
  assertEqual
    "Tools capability should be enabled"
    (Just $ ToolsCapability True)
    caps.toolsCapability
  assertEqual
    "Prompts capability should be enabled"
    (Just $ PromptsCapability True)
    caps.promptsCapability

testResourceRegistration :: Test
testResourceRegistration = TestCase $ do
  let serverInfo = Implementation "test-server" "1.0.0"
      serverCapabilities =
        ServerCapabilities
          { resourcesCapability = Just $ ResourcesCapability True
          , toolsCapability = Nothing
          , promptsCapability = Nothing
          }
      resource =
        Resource
          { resourceUri = "file:///test/resource"
          , resourceName = "Test Resource"
          , resourceDescription = Just "A test resource"
          , resourceMimeType = Just "text/plain"
          , resourceTemplate = Nothing
          }

  server <- createServer serverInfo serverCapabilities "No instructions."
  registerResources server [resource]

  resources <- readTVarIO server.serverResources
  assertEqual "Resources should be registered" 1 (length resources)
  assertEqual "Resource URI should match" "file:///test/resource" (head resources).resourceUri

testToolRegistration :: Test
testToolRegistration = TestCase $ do
  let serverInfo = Implementation "test-server" "1.0.0"
      serverCapabilities =
        ServerCapabilities
          { resourcesCapability = Nothing
          , toolsCapability = Just $ ToolsCapability True
          , promptsCapability = Nothing
          }
      tool =
        Tool
          { toolName = "test-tool"
          , toolDescription = Just "A test tool"
          , toolInputSchema = "{\"type\":\"object\",\"properties\":{\"param\":{\"type\":\"string\"}}}"
          , toolAnnotations = Nothing
          }

  server <- createServer serverInfo serverCapabilities "No instructions."
  registerTools server [tool]

  tools <- readTVarIO server.serverTools
  assertEqual "Tools should be registered" 1 (length tools)
  assertEqual "Tool name should match" "test-tool" (head tools).toolName

testPromptRegistration :: Test
testPromptRegistration = TestCase $ do
  let serverInfo = Implementation "test-server" "1.0.0"
      serverCapabilities =
        ServerCapabilities
          { resourcesCapability = Nothing
          , toolsCapability = Nothing
          , promptsCapability = Just $ PromptsCapability True
          }
      promptArg =
        PromptArgument
          { promptArgumentName = "testArg"
          , promptArgumentDescription = Just "A test argument"
          , promptArgumentRequired = True
          }
      prompt =
        Prompt
          { promptName = "test-prompt"
          , promptDescription = Just "A test prompt"
          , promptArguments = [promptArg]
          }

  server <- createServer serverInfo serverCapabilities "No instructions."
  registerPrompts server [prompt]

  prompts <- readTVarIO server.serverPrompts
  assertEqual "Prompts should be registered" 1 (length prompts)
  assertEqual "Prompt name should match" "test-prompt" (head prompts).promptName

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
handleRequestTests =
  TestList
    [ TestLabel
        "Invalid tool-call params produce an error result, not a crash"
        testInvalidToolCallParamsIsError
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

  outcome <-
    try (handleRequest server Nothing badRequest) ::
      IO (Either SomeException (Either SomeException Response))
  case outcome of
    Left crashed -> assertFailure $ "handleRequest crashed instead of returning an error result: " ++ show crashed
    Right (Left _businessErr) -> pure () -- expected: reported, not thrown
    Right (Right resp) -> assertFailure $ "expected an error result, got: " ++ show resp

-- ---------------------------------------------------------------------------
-- Transport selection tests (Network.MCP.Server.Transport)
-- ---------------------------------------------------------------------------

transportTests :: Test
transportTests =
  TestList
    [ TestLabel "mkHttpConfig accepts a valid port" testMkHttpConfigValid
    , TestLabel "mkHttpConfigOnHost preserves an explicitly configured host" testMkHttpConfigHost
    , TestLabel "mkHttpConfig rejects an out-of-range port" testMkHttpConfigInvalidPort
    , TestLabel "TransportChoice distinguishes Stdio from Http" testTransportChoiceShape
    ]

testMkHttpConfigValid :: Test
testMkHttpConfigValid = TestCase $
  case mkHttpConfig 8080 Nothing of
    Left err -> assertFailure $ "expected a valid config, got: " ++ show err
    Right cfg -> assertEqual "port should round-trip" 8080 cfg.httpConfigPort

testMkHttpConfigHost :: Test
testMkHttpConfigHost = TestCase $
  case mkHttpConfigOnHost "0.0.0.0" 8080 Nothing of
    Left err -> assertFailure $ "expected a valid config, got: " ++ show err
    Right cfg -> assertEqual "host should round-trip" "0.0.0.0" cfg.httpConfigHost

testMkHttpConfigInvalidPort :: Test
testMkHttpConfigInvalidPort = TestCase $
  case mkHttpConfig 0 Nothing of
    Left _ -> pure ()
    Right cfg ->
      assertFailure $ "expected port 0 to be rejected, got a config on port " ++ show cfg.httpConfigPort

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
httpTransportTests =
  TestList
    [ TestLabel "POSTing a JSON-RPC request over HTTP returns a JSON-RPC response" testHttpRoundTrip
    , TestLabel "A configured bearer token rejects unauthenticated requests" testHttpUnauthorized
    , TestLabel
        "A per-request Authorization header reaches the tool-call handler"
        testHttpPerRequestTokenReachesToolHandler
    , TestLabel
        "No Authorization header means the tool-call handler sees Nothing"
        testHttpNoTokenReachesToolHandlerAsNothing
    ]

httpClientTests :: Test
httpClientTests =
  TestList
    [ TestLabel "client accepts a normal local JSON-RPC success" testHttpClientNormalSuccess
    , TestLabel "client validates response ID and JSON-RPC version" testHttpClientRejectsWrongIdentity
    , TestLabel "client enforces the response size cap" testHttpClientResponseCap
    , TestLabel "client enforces the request size cap before connecting" testHttpClientRequestCap
    , TestLabel "client preserves a present null result" testHttpClientNullResult
    , TestLabel "client preserves structured JSON-RPC errors" testHttpClientServerError
    , TestLabel "client rejects malformed and ambiguous response envelopes" testHttpClientBadEnvelopes
    , TestLabel "client maps non-2xx responses without leaking bearer data" testHttpClientHttpFailure
    , TestLabel "client completes initialize, initialized, and a request" testHttpClientHandshake
    , TestLabel "client rejects initialize version mismatch before notification" testHttpClientHandshakeMismatch
    , TestLabel "client rejects initialized notification HTTP failure" testHttpClientNotificationFailure
    , TestLabel "client keeps per-call bearer tokens separate" testHttpClientBearerIsolation
    , TestLabel "client rejects header injection before network effects" testHttpClientRejectsHeaderInjection
    , TestLabel "client rejects non-ASCII path bytes before network effects" testHttpClientRejectsNonAsciiPath
    , TestLabel "client enforces a whole-operation deadline" testHttpClientDeadline
    , TestLabel "client propagates cancellation from an in-flight operation" testHttpClientCancellation
    ]

-- These tests use a real loopback WAI/Warp application. Warp owns HTTP
-- framing; the fixture captures typed request fields without a second parser.
httpClientResponse :: Value -> BL.ByteString
httpClientResponse = encode

validRpcResponse :: Value -> BL.ByteString
validRpcResponse value =
  httpClientResponse $ object ["jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Int), "result" .= value]

data CapturedRequest = CapturedRequest BS.ByteString (Maybe BS.ByteString) BL.ByteString

capturedPath :: CapturedRequest -> BS.ByteString
capturedPath (CapturedRequest path _ _) = path

capturedAuthorization :: CapturedRequest -> Maybe BS.ByteString
capturedAuthorization (CapturedRequest _ authorization _) = authorization

capturedBody :: CapturedRequest -> BL.ByteString
capturedBody (CapturedRequest _ _ body) = body

withClientReplies :: [(Int, BL.ByteString, Int)] -> (Int -> IO a) -> IO (a, [CapturedRequest])
withClientReplies replies action = withClientRepliesObserved replies (\port _ -> action port)

withClientRepliesObserved :: [(Int, BL.ByteString, Int)] -> (Int -> IO () -> IO a) -> IO (a, [CapturedRequest])
withClientRepliesObserved replies action = do
  captured <- newIORef []
  requestSeen <- newEmptyMVar
  let app request respond = do
        body <- Wai.strictRequestBody request
        let capturedRequest =
              CapturedRequest
                (Wai.rawPathInfo request)
                (lookup hAuthorization (Wai.requestHeaders request))
                body
        index <- atomicModifyIORef' captured $ \requests -> (capturedRequest : requests, length requests)
        _ <- tryPutMVar requestSeen ()
        case drop index replies of
          (status, responseBody, delayMicros) : _ -> do
            threadDelay delayMicros
            respond $
              Wai.responseLBS
                (mkStatus status "fixture")
                [(hContentType, "application/json"), (hContentLength, BS8.pack (show (BL.length responseBody)))]
                responseBody
          [] -> respond $ Wai.responseLBS (mkStatus 500 "unexpected request") [] BL.empty
  result <- testWithApplication (pure app) $ \port -> do
    value <- action port (takeMVar requestSeen)
    requests <- reverse <$> readIORef captured
    pure (value, requests)
  pure result

requestBodyValue :: CapturedRequest -> Maybe Value
requestBodyValue = decode . capturedBody

requestHasHeader :: CapturedRequest -> String -> String -> Bool
requestHasHeader request name value =
  map toLowerAscii name == "authorization"
    && capturedAuthorization request == Just (BS8.pack value)
  where
    toLowerAscii c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c

testHttpClientRejectsWrongIdentity :: Test
testHttpClientRejectsWrongIdentity = TestCase $ do
  let wrongId = encode $ object ["jsonrpc" .= ("2.0" :: Text), "id" .= (99 :: Int), "result" .= ("unexpected" :: Text)]
      wrongVersion = encode $ object ["jsonrpc" .= ("1.0" :: Text), "id" .= (1 :: Int), "result" .= ("unexpected" :: Text)]
  (idOutcome, idRequests) <- withClientReplies [(200, wrongId, 0)] $ \port ->
    try (sendHttpClientRequest (mkHttpClient "127.0.0.1" port "/mcp" Nothing) "tools/list" Nothing)
  assertBool "wrong response ID is a protocol error" (case idOutcome of Left (ProtocolError _) -> True; _ -> False)
  assertEqual "one request reached the local fixture" 1 (length idRequests)
  (versionOutcome, _) <- withClientReplies [(200, wrongVersion, 0)] $ \port ->
    try (sendHttpClientRequest (mkHttpClient "127.0.0.1" port "/mcp" Nothing) "tools/list" Nothing)
  assertBool "wrong response version is a protocol error" (case versionOutcome of Left (ProtocolError _) -> True; _ -> False)

testHttpClientNormalSuccess :: Test
testHttpClientNormalSuccess = TestCase $ do
  (outcome, requests) <- withClientReplies [(200, validRpcResponse (String "ok"), 0)] $ \port ->
    try (sendHttpClientRequest (mkHttpClient "127.0.0.1" port "/mcp" Nothing) "tools/list" Nothing)
  case outcome of
    Right (String "ok") -> pure ()
    Left (_ :: McpClientError) -> assertFailure "ordinary JSON-RPC success was rejected"
    Right value -> assertFailure $ "unexpected ordinary result: " ++ show value
  assertEqual "ordinary request reached the fixture once" 1 (length requests)

testHttpClientResponseCap :: Test
testHttpClientResponseCap = TestCase $ do
  let oversized = validRpcResponse (String (T.replicate (1024 * 1024) "x"))
  (outcome, requests) <- withClientReplies [(200, oversized, 0)] $ \port ->
    try (sendHttpClientRequest (mkHttpClient "127.0.0.1" port "/mcp" Nothing) "tools/call" (Just (object [])))
  assertBool "oversize response is a protocol cap error" (case outcome of Left (ProtocolError _) -> True; _ -> False)
  assertEqual "response cap is checked on an actual local request" 1 (length requests)

testHttpClientRequestCap :: Test
testHttpClientRequestCap = TestCase $ do
  let hugeParams = object ["payload" .= T.replicate (1024 * 1024) "p"]
  (outcome, requests) <- withClientReplies [(200, validRpcResponse Null, 0)] $ \port ->
    try (sendHttpClientRequest (mkHttpClient "127.0.0.1" port "/mcp" Nothing) "tools/call" (Just hugeParams))
  assertBool "oversize request is a protocol cap error" (case outcome of Left (ProtocolError _) -> True; _ -> False)
  assertEqual "oversize request is rejected before connecting" 0 (length requests)

testHttpClientNullResult :: Test
testHttpClientNullResult = TestCase $ do
  let body = encode $ object ["jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Int), "result" .= Null]
  (outcome, _) <- withClientReplies [(200, body, 0)] $ \port ->
    try (sendHttpClientRequest (mkHttpClient "127.0.0.1" port "/mcp" Nothing) "tools/list" Nothing)
  case outcome of
    Left (_ :: McpClientError) -> assertFailure "present null was rejected as an error"
    Right Null -> pure ()
    Right value -> assertFailure $ "expected JSON null, got " ++ show value

testHttpClientServerError :: Test
testHttpClientServerError = TestCase $ do
  let body = encode $ object ["jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Int), "error" .= object ["code" .= (-32001 :: Int), "message" .= ("sentinel failure" :: Text), "data" .= object ["retry" .= False]]]
  (outcome, _) <- withClientReplies [(200, body, 0)] $ \port ->
    try (sendHttpClientRequest (mkHttpClient "127.0.0.1" port "/mcp" Nothing) "tools/call" (Just (object [])))
  assertBool "server error remains structured" $ case outcome of
    Left ServerError{serverErrorCode = Just (-32001), serverErrorMessage = "sentinel failure", serverErrorData = Just _} -> True
    _ -> False

testHttpClientBadEnvelopes :: Test
testHttpClientBadEnvelopes = TestCase $ do
  let both = encode $ object ["jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Int), "result" .= Null, "error" .= object ["code" .= (-1 :: Int), "message" .= ("bad" :: Text)]]
      neither = encode $ object ["jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Int)]
      malformed = "{" :: BL.ByteString
      badBodies = [both, neither, malformed]
  outcomes <- mapM (\body -> withClientReplies [(200, body, 0)] $ \port -> try (sendHttpClientRequest (mkHttpClient "127.0.0.1" port "/mcp" Nothing) "tools/list" Nothing)) badBodies
  assertBool "both/neither and malformed bodies fail as protocol errors" $ all (\(outcome, _) -> case outcome of Left (ProtocolError _) -> True; _ -> False) outcomes

testHttpClientHttpFailure :: Test
testHttpClientHttpFailure = TestCase $ do
  let canary = "not-a-real-token-canary"
  (outcome, _) <- withClientReplies [(403, "forbidden", 0)] $ \port ->
    try (sendHttpClientRequest (mkHttpClient "127.0.0.1" port "/mcp" (Just (BearerToken canary))) "tools/list" Nothing)
  case outcome of
    Left (ConnectionError message) -> assertBool "transport error does not disclose bearer" (not (canary `T.isInfixOf` message))
    _ -> assertFailure "non-2xx status should be a closed transport failure"

testHttpClientHandshake :: Test
testHttpClientHandshake = TestCase $ do
  let initializeResult = object ["protocolVersion" .= ("2025-06-18" :: Text), "serverInfo" .= object ["name" .= ("local" :: Text), "version" .= ("1" :: Text)], "capabilities" .= object [], "instructions" .= ("" :: Text)]
      initResponse = encode $ object ["jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Int), "result" .= initializeResult]
      callParams = object ["name" .= ("echo" :: Text), "arguments" .= object ["value" .= ("fixture" :: Text)]]
  (result, requests) <- withClientReplies [(200, initResponse, 0), (202, BL.empty, 0), (200, validRpcResponse (String "ok"), 0)] $ \port -> do
    let client = mkHttpClient "127.0.0.1" port "/mcp" (Just (BearerToken "token-a"))
    connectHttpClient client
    sendHttpClientRequest client "tools/call" (Just callParams)
  assertEqual "post-handshake result is returned" (String "ok") result
  assertEqual "initialize, initialized notification, and request were sent" 3 (length requests)
  assertBool "all posts use the configured MCP path" (all ((== "/mcp") . capturedPath) requests)
  assertBool "per-request auth is present" (all (\raw -> requestHasHeader raw "authorization" "Bearer token-a") requests)
  let requestValues = map requestBodyValue requests
  assertEqual "first message is initialize" (Just (String "initialize")) (requestValues !! 0 >>= \(Object o) -> KeyMap.lookup "method" o)
  assertEqual "second message is initialized notification" (Just (String "notifications/initialized")) (requestValues !! 1 >>= \(Object o) -> KeyMap.lookup "method" o)
  assertEqual "third message is tools/call" (Just (String "tools/call")) (requestValues !! 2 >>= \(Object o) -> KeyMap.lookup "method" o)
  assertEqual "third message preserves its params" (Just callParams) (requestValues !! 2 >>= \(Object o) -> KeyMap.lookup "params" o)
  assertBool "notification has no JSON-RPC ID" (case requestValues !! 1 of Just (Object o) -> not (KeyMap.member "id" o); _ -> False)

testHttpClientHandshakeMismatch :: Test
testHttpClientHandshakeMismatch = TestCase $ do
  let badResult = object ["protocolVersion" .= ("2024-11-05" :: Text), "serverInfo" .= object ["name" .= ("local" :: Text), "version" .= ("1" :: Text)], "capabilities" .= object [], "instructions" .= ("" :: Text)]
      badInit = encode $ object ["jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Int), "result" .= badResult]
  (outcome, requests) <- withClientReplies [(200, badInit, 0)] $ \port ->
    try (connectHttpClient (mkHttpClient "127.0.0.1" port "/mcp" Nothing))
  assertBool "unsupported handshake version fails" (case outcome of Left (_ :: McpClientError) -> True; Right () -> False)
  assertEqual "notification is not sent after handshake mismatch" 1 (length requests)

testHttpClientNotificationFailure :: Test
testHttpClientNotificationFailure = TestCase $ do
  let initResult = object ["protocolVersion" .= ("2025-06-18" :: Text), "serverInfo" .= object ["name" .= ("local" :: Text), "version" .= ("1" :: Text)], "capabilities" .= object [], "instructions" .= ("" :: Text)]
      initResponse = encode $ object ["jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Int), "result" .= initResult]
  (outcome, requests) <- withClientReplies [(200, initResponse, 0), (500, "failure", 0)] $ \port ->
    try (connectHttpClient (mkHttpClient "127.0.0.1" port "/mcp" Nothing))
  assertBool "notification HTTP failure fails initialization" (case outcome of Left (ConnectionError _) -> True; _ -> False)
  assertEqual "no subsequent operation occurs after notification failure" 2 (length requests)

testHttpClientBearerIsolation :: Test
testHttpClientBearerIsolation = TestCase $ do
  let replies = [(200, validRpcResponse (String "first"), 0), (200, validRpcResponse (String "second"), 0)]
  (_, requests) <- withClientReplies replies $ \port -> do
    let first = mkHttpClient "127.0.0.1" port "/mcp" (Just (BearerToken "token-one"))
        second = mkHttpClient "127.0.0.1" port "/mcp" (Just (BearerToken "token-two"))
    _ <- sendHttpClientRequest first "tools/list" Nothing
    sendHttpClientRequest second "resources/list" Nothing
  let captured = requests
  assertEqual "two independent requests were observed" 2 (length captured)
  assertBool "first request retains its own bearer" (requestHasHeader (captured !! 0) "authorization" "Bearer token-one")
  assertBool "second request retains its own bearer" (requestHasHeader (captured !! 1) "authorization" "Bearer token-two")

testHttpClientRejectsHeaderInjection :: Test
testHttpClientRejectsHeaderInjection = TestCase $ do
  let badClients port =
        [ mkHttpClient "127.0.0.1\r\nX-Injected: yes" port "/mcp" Nothing
        , mkHttpClient "127.0.0.1" port "/mcp\r\nX-Injected: yes" Nothing
        , mkHttpClient "127.0.0.1" port "/mcp" (Just (BearerToken "token\r\nX-Injected: yes"))
        ]
  (outcomes, requests) <- withClientReplies (replicate 3 (200, validRpcResponse Null, 0)) $ \port ->
    mapM (\client -> try (sendHttpClientRequest client "tools/list" Nothing)) (badClients port)
  assertBool "injected host/path/token values fail closed" (all (\outcome -> case outcome of Left (_ :: McpClientError) -> True; Right _ -> False) outcomes)
  assertEqual "invalid request data is rejected before networking" 0 (length requests)

testHttpClientRejectsNonAsciiPath :: Test
testHttpClientRejectsNonAsciiPath = TestCase $ do
  let invalidPaths = ["/" ++ "\x0161" ++ "dmin", "/" ++ "\x010d" ++ "admin", "/" ++ "\x010a" ++ "admin"]
  (outcomes, requests) <- withClientReplies (replicate (length invalidPaths) (200, validRpcResponse Null, 0)) $ \port ->
    mapM (\path -> try (sendHttpClientRequest (mkHttpClient "127.0.0.1" port path Nothing) "tools/list" Nothing)) invalidPaths
  assertBool "non-ASCII and low-byte control paths are protocol errors" (all (\outcome -> case outcome of Left (ProtocolError _) -> True; _ -> False) outcomes)
  assertEqual "rejected path produces no local HTTP request" 0 (length requests)

testHttpClientDeadline :: Test
testHttpClientDeadline = TestCase $ do
  (outcome, _) <- withClientReplies [(200, validRpcResponse Null, 11 * 1000 * 1000)] $ \port ->
    try (sendHttpClientRequest (mkHttpClient "127.0.0.1" port "/mcp" Nothing) "tools/list" Nothing)
  assertBool "whole-operation deadline is a closed timeout failure" (case outcome of Left (ConnectionError _) -> True; _ -> False)

testHttpClientCancellation :: Test
testHttpClientCancellation = TestCase $ do
  (completed, requests) <- withClientRepliesObserved [(200, validRpcResponse Null, 3 * 1000 * 1000)] $ \port awaitRequest -> do
    finished <- newEmptyMVar
    tid <- forkIO $ try (sendHttpClientRequest (mkHttpClient "127.0.0.1" port "/mcp" Nothing) "tools/list" Nothing) >>= putMVar finished
    awaitRequest
    killThread tid
    timeout 1000000 (takeMVar finished)
  assertBool "caller cancellation interrupts the in-flight request" (case completed of Just (Left (_ :: SomeException)) -> True; _ -> False)
  assertEqual "the server received the request before cancellation" 1 (length requests)

{- | Task: per-call OAuth plumbing. The server has NO configured bearer
token (so the transport's own auth *gate* is open to everyone, per
'authorized'\'s existing "Nothing -> True" behavior) -- but a caller's
own real @Authorization: Bearer ...@ header must still reach the
registered 'ToolCallHandler' for that one call, so a tool implementation
(e.g. semantic-compiler's execute_query) can build a per-call credential
from it. This is real typed plumbing through 'runHTTPServer'/
'handleMessage' -- not a global/IORef side channel.
-}
testHttpPerRequestTokenReachesToolHandler :: Test
testHttpPerRequestTokenReachesToolHandler = TestCase $ do
  case mkHttpConfig 18082 Nothing of
    Left err -> assertFailure (show err)
    Right cfg -> do
      seenTokenVar <- newTVarIO Nothing
      server <-
        createServer
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
      assertEqual
        "handler should see the real per-request bearer token"
        (Just (BearerToken "user-supplied-token"))
        seen

testHttpNoTokenReachesToolHandlerAsNothing :: Test
testHttpNoTokenReachesToolHandlerAsNothing = TestCase $ do
  case mkHttpConfig 18083 Nothing of
    Left err -> assertFailure (show err)
    Right cfg -> do
      seenTokenVar <- newTVarIO (Just (BearerToken "sentinel-should-be-overwritten"))
      server <-
        createServer
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
      assertEqual
        "no Authorization header means the handler sees Nothing, not a stale/default token"
        Nothing
        seen

toolCallRequestBody :: BL.ByteString
toolCallRequestBody =
  encode $
    object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "id" .= (1 :: Int)
      , "method" .= ("tools/call" :: Text)
      , "params"
          .= object
            [ "name" .= ("noop" :: Text)
            , "arguments" .= object []
            ]
      ]

testHttpRoundTrip :: Test
testHttpRoundTrip = TestCase $ do
  case mkHttpConfig 18080 Nothing of
    Left err -> assertFailure (show err)
    Right cfg -> do
      server <-
        createServer
          (Implementation "http-test-server" "1.0.0")
          (ServerCapabilities Nothing Nothing Nothing)
          "No instructions."
      tid <- forkIO $ runServerWithHTTP server cfg
      threadDelay 200000 -- let the listener come up
      raw <- httpPostRaw 18080 initializeRequestBody
      killThread tid
      assertBool
        ("expected a 200 status line, got: " ++ BS8.unpack raw)
        (BS8.pack "200" `BS.isInfixOf` raw)
      case decode (responseBody raw) :: Maybe Value of
        Just (Object _) -> pure ()
        other -> assertFailure $ "expected a JSON object response body, got: " ++ show other

testHttpUnauthorized :: Test
testHttpUnauthorized = TestCase $ do
  case mkHttpConfig 18081 (Just (BearerToken "secret-token")) of
    Left err -> assertFailure (show err)
    Right cfg -> do
      server <-
        createServer
          (Implementation "http-test-server" "1.0.0")
          (ServerCapabilities Nothing Nothing Nothing)
          "No instructions."
      tid <- forkIO $ runServerWithHTTP server cfg
      threadDelay 200000
      raw <- httpPostRaw 18081 initializeRequestBody
      killThread tid
      assertBool
        ("expected a 401 status line, got: " ++ BS8.unpack raw)
        (BS8.pack "401" `BS.isInfixOf` raw)

initializeRequestBody :: BL.ByteString
initializeRequestBody =
  encode $
    object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "id" .= (1 :: Int)
      , "method" .= ("initialize" :: Text)
      , "params"
          .= object
            [ "protocolVersion" .= ("2024-11-05" :: Text)
            , "clientInfo" .= object ["name" .= ("test-client" :: Text), "version" .= ("1.0.0" :: Text)]
            , "capabilities" .= object []
            ]
      ]

{- | Send a raw HTTP/1.1 POST over a real TCP socket and return the full
raw response (status line + headers + body) so tests can assert on
status codes as well as body content.
-}
httpPostRaw :: Int -> BL.ByteString -> IO BS.ByteString
httpPostRaw port body = do
  addrs <-
    getAddrInfo (Just (defaultHints {addrSocketType = Stream})) (Just "127.0.0.1") (Just (show port))
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

{- | Same as 'httpPostRaw', but optionally sets a real
@Authorization: Bearer ...@ header, for exercising per-request-token
plumbing.
-}
httpPostRawWithAuth :: Int -> Maybe Text -> BL.ByteString -> IO BS.ByteString
httpPostRawWithAuth port mToken body = do
  addrs <-
    getAddrInfo (Just (defaultHints {addrSocketType = Stream})) (Just "127.0.0.1") (Just (show port))
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

{- | Extract just the body from a raw HTTP response (everything after the
first blank line).
-}
responseBody :: BS.ByteString -> BL.ByteString
responseBody raw =
  let sep = BS8.pack "\r\n\r\n"
      (_, rest) = BS.breakSubstring sep raw
   in if BS.null rest
        then BL.fromStrict raw
        else BL.fromStrict (BS.drop (BS.length sep) rest)
