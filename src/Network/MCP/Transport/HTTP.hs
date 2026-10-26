{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | A real Streamable HTTP transport, per the MCP spec's "Streamable HTTP"
transport definition: the server accepts HTTP POST requests carrying a
single JSON-RPC message in the body, and returns either a single
@application/json@ response (the profile implemented here) or an SSE
stream (not needed by this server: it never needs to push more than one
message per request).

This is deliberately built directly on "Network.Socket" rather than
pulling in a full web framework: the protocol surface we need (parse a
request line + headers + a bounded JSON body, write back a JSON body)
is small, and hs-mcp otherwise has no HTTP dependency at all.
-}
module Network.MCP.Transport.HTTP (
  HttpConfig (httpConfigHost, httpConfigPort),
  HttpConfigError (..),
  mkHttpConfig,
  mkHttpConfigOnHost,
  BearerToken (..),
  HttpClient,
  mkHttpClient,
  connectHttpClient,
  sendHttpClientRequest,
  runHTTPServer,
)
where

import Control.Concurrent (forkFinally)
import Control.Exception (SomeException, bracket, catch, throwIO)
import Data.Aeson (Result (..), Value (..), encode, fromJSON, object, (.=))
import Data.Int (Int64)
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Aeson (eitherDecode, eitherDecodeStrict')
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types.Header (RequestHeaders, hContentType)
import Network.HTTP.Types.Status (statusCode)
import Network.MCP.Client.Types (McpClientError (..))
import Network.MCP.Transport.Types (ErrorResponse (..), Message)
import Network.MCP.Types (ServerInitializeOptions (..))
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Timeout (timeout)

{- | A bearer token an incoming request's @Authorization@ header must
present, per OAuth 2.1 Bearer Token Usage (RFC 6750). The MCP spec's
Authorization section says HTTP-based transports SHOULD, not MUST,
conform to OAuth 2.1 — so this is opt-in: a server started without a
configured token skips the check entirely (matching STDIO's trusted,
local-process security model, per spec).
-}
newtype BearerToken = BearerToken Text
  deriving (Show, Eq)

{- | Validated HTTP transport configuration. The only way to obtain one is
'mkHttpConfig', which rejects an out-of-range port — so a 'HttpConfig'
value in hand is already known-good and downstream code never re-checks
the port range.
-}
data HttpConfig = HttpConfig
  { httpConfigPort :: Int
  , httpConfigHost :: HostName
  , httpConfigBearerToken :: Maybe BearerToken
  }
  deriving (Show, Eq)

data HttpConfigError = InvalidPort Int | InvalidHost HostName
  deriving (Show, Eq)

{- | Smart constructor: the only way to build an 'HttpConfig'. Ports outside
the valid TCP range are rejected here, once, so nothing downstream has to
re-validate.
-}
mkHttpConfig :: Int -> Maybe BearerToken -> Either HttpConfigError HttpConfig
mkHttpConfig = mkHttpConfigOnHost "127.0.0.1"

mkHttpConfigOnHost :: HostName -> Int -> Maybe BearerToken -> Either HttpConfigError HttpConfig
mkHttpConfigOnHost host port token
  | port < 1 || port > 65535 = Left (InvalidPort port)
  | null host = Left (InvalidHost host)
  | otherwise = Right (HttpConfig port host token)

-- | A deliberately narrow JSON-over-HTTP client. It is separate from the
-- server configuration and keeps its bearer private to this client value.
data HttpClient = HttpClient
  { httpClientHost :: HostName
  , httpClientPort :: Int
  , httpClientPath :: String
  , httpClientBearer :: Maybe BearerToken
  }

mkHttpClient :: HostName -> Int -> String -> Maybe BearerToken -> HttpClient
mkHttpClient = HttpClient

clientOperationLimit :: Int
clientOperationLimit = 10 * 1000 * 1000

clientMessageLimit :: Int64
clientMessageLimit = 1024 * 1024

clientProtocolVersion :: Text
clientProtocolVersion = "2025-06-18"

-- | Initialize the supported JSON response profile. Session/SSE transports
-- are intentionally not negotiated by this client.
connectHttpClient :: HttpClient -> IO ()
connectHttpClient client = do
  let initializeParams =
        object
          [ "protocolVersion" .= clientProtocolVersion
          , "clientInfo" .= object ["name" .= ("hs-mcp" :: Text), "version" .= ("0.1.1.0" :: Text)]
          , "capabilities" .= object []
          ]
  result <- sendHttpClientRequest client "initialize" (Just initializeParams)
  case fromJSON result of
    Success (ServerInitializeOptions {serverInitProtocolVersion = version})
      | version == clientProtocolVersion -> sendHttpNotification client "notifications/initialized"
      | otherwise -> throwIO $ ProtocolError "MCP initialize returned an unsupported protocol version"
    Error _ -> throwIO $ ProtocolError "MCP initialize returned an invalid result"

-- | Send one JSON-RPC request and return its unwrapped result. Response
-- identity and field presence are checked before decoding the result so a
-- JSON null remains distinct from an absent result.
sendHttpClientRequest :: HttpClient -> Text -> Maybe Value -> IO Value
sendHttpClientRequest client method params = withClientDeadline $ do
  let requestValue =
        object $
          [ "jsonrpc" .= ("2.0" :: Text)
          , "id" .= (1 :: Int)
          , "method" .= method
          ]
            ++ maybe [] (\value -> ["params" .= value]) params
  body <- encodeClientMessage requestValue
  responseBody <- postClientMessage client body 200
  parseJsonRpcResponse responseBody

sendHttpNotification :: HttpClient -> Text -> IO ()
sendHttpNotification client method = withClientDeadline $ do
  let notificationValue = object ["jsonrpc" .= ("2.0" :: Text), "method" .= method]
  body <- encodeClientMessage notificationValue
  _ <- postClientMessage client body 202
  pure ()

withClientDeadline :: IO a -> IO a
withClientDeadline action = do
  result <- timeout clientOperationLimit action
  case result of
    Nothing -> throwIO $ ConnectionError "MCP HTTP operation timed out"
    Just value -> pure value

encodeClientMessage :: Value -> IO BL.ByteString
encodeClientMessage value = do
  let prefix = BL.toStrict $ BL.take (clientMessageLimit + 1) (encode value)
  if fromIntegral (BS.length prefix) > clientMessageLimit
    then throwIO $ ProtocolError "MCP HTTP request exceeds the 1 MiB limit"
    else pure $ BL.fromStrict prefix

postClientMessage :: HttpClient -> BL.ByteString -> Int -> IO BL.ByteString
postClientMessage client@HttpClient {..} body expectedStatus = do
  either (throwIO . ProtocolError) pure (validateClientAddress client)
  if fromIntegral (BL.length body) > clientMessageLimit
    then throwIO $ ProtocolError "MCP HTTP request exceeds the 1 MiB limit"
    else postClientMessageWithinDeadline client body expectedStatus

postClientMessageWithinDeadline :: HttpClient -> BL.ByteString -> Int -> IO BL.ByteString
postClientMessageWithinDeadline HttpClient {..} body expectedStatus = do
  let settings =
        HTTP.defaultManagerSettings
          { HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro clientOperationLimit
          , HTTP.managerRetryableException = const False
          }
  bracket (HTTP.newManager settings) HTTP.closeManager $ \activeManager -> do
    baseRequest <- HTTP.parseRequest "http://localhost/"
    let request =
          baseRequest
            { HTTP.host = BS8.pack httpClientHost
            , HTTP.port = httpClientPort
            , HTTP.path = BS8.pack (if null httpClientPath then "/" else httpClientPath)
            , HTTP.method = "POST"
            , HTTP.requestHeaders = clientHeaders httpClientBearer
            , HTTP.requestBody = HTTP.RequestBodyLBS body
            , HTTP.redirectCount = 0
            , HTTP.responseTimeout = HTTP.responseTimeoutMicro clientOperationLimit
            , HTTP.checkResponse = \_ _ -> pure ()
            }
    (HTTP.withResponse request activeManager (readClientResponse expectedStatus))
      `catch` \(_ :: HTTP.HttpException) -> throwIO $ ConnectionError "MCP HTTP transport failed"

clientHeaders :: Maybe BearerToken -> RequestHeaders
clientHeaders bearer =
  [ ("Content-Type", "application/json")
  , ("Accept", "application/json")
  , ("MCP-Protocol-Version", "2025-06-18")
  ]
    ++ case bearer of
      Nothing -> []
      Just (BearerToken token) -> [("Authorization", "Bearer " <> TE.encodeUtf8 token)]

readClientResponse :: Int -> HTTP.Response HTTP.BodyReader -> IO BL.ByteString
readClientResponse expectedStatus response = do
  let status = statusCode (HTTP.responseStatus response)
      contentType = lookup hContentType (HTTP.responseHeaders response)
      sessionHeader = lookup "Mcp-Session-Id" (HTTP.responseHeaders response)
  if status /= expectedStatus
    then throwIO $ ConnectionError "MCP HTTP server returned an unexpected status"
    else case sessionHeader of
      Just _ -> throwIO $ ProtocolError "MCP HTTP sessions are unsupported"
      Nothing -> pure ()
  if expectedStatus == 202
    then pure BL.empty
    else case contentType of
      Just value | normalizedContentType value == "application/json" -> BL.fromStrict <$> readBoundedResponse (HTTP.responseBody response)
      _ -> throwIO $ ProtocolError "MCP HTTP response is not application/json"

normalizedContentType :: BS.ByteString -> BS.ByteString
normalizedContentType = BS8.map toLowerAscii . trimAscii . BS.takeWhile (/= 59)
  where
    toLowerAscii c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c
    trimAscii = BS.reverse . BS.dropWhile isSpaceAscii . BS.reverse . BS.dropWhile isSpaceAscii
    isSpaceAscii c = c == 32 || c == 9

readBoundedResponse :: HTTP.BodyReader -> IO BS.ByteString
readBoundedResponse reader = go 0 []
  where
    go total chunks = do
      chunk <- HTTP.brRead reader
      if BS.null chunk
        then pure $ BS.concat (reverse chunks)
        else
          let nextTotal = total + fromIntegral (BS.length chunk)
           in if nextTotal > clientMessageLimit
                then throwIO $ ProtocolError "MCP HTTP response exceeds the 1 MiB limit"
                else go nextTotal (chunk : chunks)

parseJsonRpcResponse :: BL.ByteString -> IO Value
parseJsonRpcResponse body =
  case eitherDecodeStrict' (BL.toStrict body) of
    Left _ -> throwIO $ ProtocolError "MCP HTTP response contains invalid JSON"
    Right (Object response) -> do
      unlessField "jsonrpc" (String "2.0") response
      unlessField "id" (Number 1) response
      let hasResult = AesonKeyMap.member (AesonKey.fromText "result") response
          hasError = AesonKeyMap.member (AesonKey.fromText "error") response
      if hasResult == hasError
        then throwIO $ ProtocolError "MCP response must contain exactly one of result or error"
        else case AesonKeyMap.lookup (AesonKey.fromText "result") response of
          Just result -> pure result
          Nothing -> case AesonKeyMap.lookup (AesonKey.fromText "error") response of
            Just errorValue -> case fromJSON errorValue of
              Success (ErrorResponse {..}) -> throwIO $ ServerError (Just errorCode) errorMessage errorData
              Error _ -> throwIO $ ProtocolError "MCP response contains an invalid JSON-RPC error"
            Nothing -> throwIO $ ProtocolError "MCP response is missing its result"
    Right _ -> throwIO $ ProtocolError "MCP HTTP response must be a JSON object"
  where
    unlessField name expected response =
      case AesonKeyMap.lookup (AesonKey.fromText name) response of
        Just actual | actual == expected -> pure ()
        _ -> throwIO $ ProtocolError "MCP response has an invalid JSON-RPC identity"

validateClientAddress :: HttpClient -> Either Text ()
validateClientAddress HttpClient {..}
  | httpClientPort < 1 || httpClientPort > 65535 = Left "MCP HTTP port is invalid"
  | null httpClientHost || not (all validHostChar httpClientHost) = Left "MCP HTTP host is invalid"
  | pathIsInvalid httpClientPath = Left "MCP HTTP path is invalid"
  | maybe False (T.any invalidTokenChar . unBearerToken) httpClientBearer = Left "MCP HTTP bearer token is invalid"
  | otherwise = Right ()
  where
    validHostChar c = isAsciiAlphaNum c || c == '.' || c == '-' || c == ':'
    isAsciiAlphaNum c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')
    pathIsInvalid [] = False
    pathIsInvalid (first : rest) = first /= '/' || any invalidPathChar rest
    invalidPathChar c = fromEnum c > 127 || fromEnum c < 32 || c == '\DEL' || c == '#' || c == ' '
    invalidTokenChar c = c < ' ' || c == '\DEL'

unBearerToken :: BearerToken -> Text
unBearerToken (BearerToken token) = token

{- | Run a Streamable HTTP server on the configured port. Each accepted
connection is handled by reading one HTTP/1.1 request, decoding its body
as a single JSON-RPC 'Message', invoking @handler@ with that request's
own real per-request bearer token (if it presented one -- 'Nothing'
otherwise), and writing back the handler's response (if any) as a single
@application/json@ body — the non-streaming profile of Streamable HTTP,
which the spec explicitly permits when the server has nothing more to
push after the one reply.

The per-request token handed to @handler@ is the REQUEST's own presented
token, independent of 'httpConfigBearerToken' (the server's configured
expected token, used only by 'authorized' below to gate access). A
server started with no configured token still surfaces whatever bearer
token an individual caller presents — that's what lets a tool
implementation (e.g. semantic-compiler's execute_query) build a
per-call credential from a caller's own token, distinct from any
server-level access gate.
-}
runHTTPServer :: HttpConfig -> (Maybe BearerToken -> Message -> IO (Maybe Message)) -> IO ()
runHTTPServer HttpConfig {..} handler = do
  addr <- resolve
  bracket (open addr) close loop
  where
    resolve = do
      let hints = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}
      head' <$> getAddrInfo (Just hints) (Just httpConfigHost) (Just (show httpConfigPort))
      where
        head' (a : _) = a
        head' [] = error "getAddrInfo returned no results for a well-formed host/port"

    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      bind sock (addrAddress addr)
      listen sock 16
      return sock

    loop sock = forever' $ do
      (conn, _peer) <- accept sock
      _ <- forkFinally (serveConn conn) (\_ -> close conn)
      return ()
      where
        forever' act = act >> loop sock

    serveConn conn =
      handleOne conn `catch` \(_ :: SomeException) -> pure ()

    handleOne conn = do
      req <- readRequest conn
      case req of
        Nothing -> writeResponse conn (400 :: Int) "Bad Request" Nothing
        Just r
          | r.requestMethodOf /= "POST" ->
              writeResponse conn (405 :: Int) "Method Not Allowed" Nothing
          | not (authorized r) ->
              writeResponse conn (401 :: Int) "Unauthorized" Nothing
          | otherwise ->
              case eitherDecode (BL.fromStrict (r.requestBody)) of
                Left err -> writeResponse conn (400 :: Int) ("Bad Request: " ++ err) Nothing
                Right msg -> do
                  resp <- handler (presentedBearerToken r) msg
                  case resp of
                    Nothing -> writeResponse conn (202 :: Int) "Accepted" Nothing
                    Just m -> writeResponse conn (200 :: Int) "OK" (Just (encode m))

    -- \| The real token THIS request presented, if any -- independent of
    -- whether the server has a configured 'httpConfigBearerToken' to gate
    -- on. See 'runHTTPServer'\'s Haddock for why these are deliberately
    -- separate concerns.
    presentedBearerToken :: ParsedRequest -> Maybe BearerToken
    presentedBearerToken r = case lookup "authorization" (r.requestHeaders) of
      Just v | "Bearer " `isPrefixOf` v -> Just (BearerToken (T.pack (drop 7 v)))
      _ -> Nothing

    authorized r = case httpConfigBearerToken of
      Nothing -> True
      Just (BearerToken expected) ->
        case lookup "authorization" (r.requestHeaders) of
          Just v
            | "Bearer " `isPrefixOf` v ->
                T.pack (drop 7 v) == expected
          _ -> False

    writeResponse :: Socket -> Int -> String -> Maybe BL.ByteString -> IO ()
    writeResponse conn code reason mBody = do
      let bodyBytes = maybe BL.empty id mBody
          headers =
            [ "HTTP/1.1 " ++ show code ++ " " ++ reason
            , "Content-Type: application/json"
            , "Content-Length: " ++ show (BL.length bodyBytes)
            , "Connection: close"
            ]
              ++ ["WWW-Authenticate: Bearer" | code == 401]
          -- HTTP/1.1 requires CRLF line endings; a bare "\n" (what 'unlines'
          -- produces) is tolerated by lenient clients like curl but not by
          -- a strict CRLF-aware parser, so build the header block by hand.
          headerBlock = BS8.pack (concatMap (++ "\r\n") headers ++ "\r\n")
      sendAll conn headerBlock
      sendAll conn (BL.toStrict bodyBytes)

{- | A minimally-parsed HTTP/1.1 request: the request line's method, and
the headers and body needed to dispatch a JSON-RPC message.
-}
data ParsedRequest = ParsedRequest
  { requestMethodOf :: String
  , requestHeaders :: [(String, String)]
  , requestBody :: BS.ByteString
  }

{- | Read a full HTTP/1.1 request off the socket: the request line, headers
up to the blank line, and exactly @Content-Length@ bytes of body.
Returns 'Nothing' on any framing error (missing request line, malformed
headers, connection closed early) rather than throwing — callers see
this as an ordinary 400, not a crash.
-}
readRequest :: Socket -> IO (Maybe ParsedRequest)
readRequest conn = do
  mHead <- readUntilBlankLine conn BS.empty
  case mHead of
    Nothing -> pure Nothing
    Just (headBytes, rest) ->
      case parseHead headBytes of
        Nothing -> pure Nothing
        Just (method, headers) -> do
          let contentLength = case lookup "content-length" headers of
                Just s -> case reads s of
                  [(n, "")] -> n
                  _ -> 0
                Nothing -> 0
          body <- readExactly conn contentLength rest
          pure $ Just (ParsedRequest method headers body)

parseHead :: BS.ByteString -> Maybe (String, [(String, String)])
parseHead raw = case filter (not . null) (map stripCR (lines (BS8.unpack raw))) of
  (reqLine : headerLines) ->
    case words reqLine of
      (method : _path : _version : _) ->
        Just (method, map parseHeaderLine headerLines)
      _ -> Nothing
  [] -> Nothing
  where
    stripCR s = case reverse s of
      ('\r' : rest) -> reverse rest
      _ -> s

parseHeaderLine :: String -> (String, String)
parseHeaderLine line = case break (== ':') line of
  (k, ':' : v) -> (lowercase (trim k), trim v)
  (k, "") -> (lowercase (trim k), "")
  _ -> (lowercase (trim line), "")
  where
    lowercase = map toLowerAscii
    toLowerAscii c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c
    trim = f . f
      where
        f = reverse . dropWhile (`elem` (" \r\t" :: String))

{- | Read from the socket until a blank line (@\\r\\n\\r\\n@) is seen,
returning the header block and any bytes already read past it.
-}
readUntilBlankLine :: Socket -> BS.ByteString -> IO (Maybe (BS.ByteString, BS.ByteString))
readUntilBlankLine conn acc =
  case findSep acc of
    Just idx -> pure $ Just (BS.take idx acc, BS.drop (idx + 4) acc)
    Nothing -> do
      chunk <- recv conn 4096
      if BS.null chunk
        then pure Nothing
        else readUntilBlankLine conn (acc <> chunk)
  where
    sep = BS8.pack "\r\n\r\n"
    findSep bs = go 0
      where
        go i
          | i + BS.length sep > BS.length bs = Nothing
          | BS.take (BS.length sep) (BS.drop i bs) == sep = Just i
          | otherwise = go (i + 1)

{- | Read exactly @n@ bytes of body, given bytes already buffered from the
header read.
-}
readExactly :: Socket -> Int -> BS.ByteString -> IO BS.ByteString
readExactly _conn n buffered
  | BS.length buffered >= n = pure (BS.take n buffered)
readExactly conn n buffered = do
  chunk <- recv conn (max 4096 (n - BS.length buffered))
  if BS.null chunk
    then pure buffered
    else readExactly conn n (buffered <> chunk)
