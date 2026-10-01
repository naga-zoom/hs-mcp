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
  runHTTPServer,
)
where

import Control.Concurrent (forkFinally)
import Control.Exception (SomeException, bracket, catch)
import Data.Aeson (eitherDecode, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Network.MCP.Transport.Types (Message)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

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
