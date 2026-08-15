{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.MCP.Server
  ( Server,
    ServerHandler,
    ResourceReadHandler,
    ToolCallHandler,
    PromptHandler,
    createServer,
    registerResources,
    registerResourceReadHandler,
    registerTools,
    registerToolCallHandler,
    registerPrompts,
    registerPromptHandler,
    runServerWithTransport,
    withTransportServer,
    handleRequest,
    handleMessage,
    -- * Server-initiated notifications
    notifyToolsListChanged,
    notifyResourcesListChanged,
    notifyPromptsListChanged,
  )
where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM
import Control.Exception (SomeException, throwIO, toException, try)
import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Network.MCP.Server.Types
import Network.MCP.Transport.HTTP (BearerToken)
import Network.MCP.Transport.Types
import Network.MCP.Types
import UnliftIO.Async qualified as UnliftIO

createServer :: ServerInfo -> ServerCapabilities -> Text -> IO Server
createServer info caps instructions = do
  resourcesVar <- newTVarIO []
  toolsVar <- newTVarIO []
  promptsVar <- newTVarIO []
  resourceHandlerVar <- newTVarIO Nothing
  toolHandlerVar <- newTVarIO Nothing
  promptHandlerVar <- newTVarIO Nothing

  -- Initialize message handlers map
  handlersVar <- newTVarIO Map.empty

  -- No transport is wired yet at construction time; runServerWithTransport
  -- installs the real send-notification callback below.
  sendNotificationVar <- newTVarIO Nothing

  let server =
        Server
          { serverInfo = info,
            serverCapabilities = caps,
            serverResources = resourcesVar,
            serverTools = toolsVar,
            serverPrompts = promptsVar,
            serverInstructions = instructions,
            serverResourceReadHandler = resourceHandlerVar,
            serverToolCallHandler = toolHandlerVar,
            serverPromptHandler = promptHandlerVar,
            serverMessageHandlers = handlersVar,
            serverSendNotification = sendNotificationVar
          }

  -- Register standard request handlers
  registerInitializeHandler server
  registerListResourcesHandler server
  registerReadResourceHandler server
  registerListToolsHandler server
  registerCallToolHandler server
  registerListPromptsHandler server
  registerGetPromptHandler server

  return server

-- | Register available resources
registerResources :: Server -> [Resource] -> IO ()
registerResources server resources = atomically $ writeTVar (serverResources server) resources

-- | Register resource read handler
registerResourceReadHandler :: Server -> ResourceReadHandler -> IO ()
registerResourceReadHandler server handler = atomically $ writeTVar (serverResourceReadHandler server) (Just handler)

-- | Register available tools
registerTools :: Server -> [Tool] -> IO ()
registerTools server tools = atomically $ writeTVar (serverTools server) tools

-- | Register tool call handler
registerToolCallHandler :: Server -> ToolCallHandler -> IO ()
registerToolCallHandler server handler = atomically $ writeTVar (serverToolCallHandler server) (Just handler)

-- | Register available prompts
registerPrompts :: Server -> [Prompt] -> IO ()
registerPrompts server prompts = atomically $ writeTVar (serverPrompts server) prompts

-- | Register prompt handler
registerPromptHandler :: Server -> PromptHandler -> IO ()
registerPromptHandler server handler = atomically $ writeTVar (serverPromptHandler server) (Just handler)

-- | Run the server, handling messages until the transport mechanism shuts down.
runServerWithTransport :: (Transport t) => Server -> t -> IO ()
runServerWithTransport server t = do
  -- Install the server-initiated notification path: each 'notify*' call
  -- now encodes the spec'd notifications/... method and pushes it on
  -- the transport via 'sendMessage'. Transports that don't override
  -- 'sendMessage' silently drop these — consistent with not advertising
  -- a "listChanged: true" capability.
  let pushOne :: Value -> IO ()
      pushOne v = case fromJSON v of
        Success notif -> sendMessage t (NotificationMessage notif)
        Error _ -> pure ()
  atomically $ writeTVar (serverSendNotification server) (Just pushOne)
  -- Generic 'Transport'-driven loop (stdio today): no per-request bearer
  -- token concept exists here, so every call runs as the server's single
  -- startup-resolved identity, unchanged -- matches STDIO's existing
  -- trusted-single-process security model exactly.
  handleMessages t (handleMessage server Nothing)

-- | Notify connected clients that the server's tool list has changed.
-- Fires the spec'd @notifications/tools/list_changed@ on the active
-- transport. A no-op if 'runServerWithTransport' hasn't been started
-- yet or if the transport doesn't support server push.
notifyToolsListChanged :: Server -> IO ()
notifyToolsListChanged = sendListChanged "notifications/tools/list_changed"

-- | Notify connected clients that the server's resource list has changed.
notifyResourcesListChanged :: Server -> IO ()
notifyResourcesListChanged = sendListChanged "notifications/resources/list_changed"

-- | Notify connected clients that the server's prompt list has changed.
notifyPromptsListChanged :: Server -> IO ()
notifyPromptsListChanged = sendListChanged "notifications/prompts/list_changed"

-- | Internal: encode a list_changed notification and push it on the
-- active transport (if any).
sendListChanged :: Text -> Server -> IO ()
sendListChanged method server = do
  mPush <- readTVarIO (serverSendNotification server)
  case mPush of
    Nothing -> pure ()
    Just push ->
      push $
        object
          [ "jsonrpc" .= ("2.0" :: Text),
            "method" .= method
          ]

-- | Run a transport server while the action is executed.
-- If the action throws an exception or terminates the server will be stopped.
withTransportServer :: (Transport t) => Server -> t -> (UnliftIO.Async () -> IO a) -> IO a
withTransportServer server t action = do
  let runServer = do
        -- Run the server with the provided transport
        runServerWithTransport server t
  Async.withAsync runServer $ \handle -> do
    -- Execute the action with the server running
    -- The server will be terminated when the action completes
    action handle

-- Helper to process a message. @mToken@ is the calling request's real
-- per-request bearer token, if the active transport surfaces one (HTTP
-- only -- see "Network.MCP.Server.HTTP"); it flows straight through to
-- 'handleRequest' alongside the message it authorizes, not via a side
-- channel.
handleMessage :: Server -> Maybe BearerToken -> Message -> IO (Maybe Message)
handleMessage server mToken msg = case msg of
  RequestMessage request ->
    handleRequest server mToken request >>= \case
      Right response -> pure . Just $ (ResponseMessage response)
      Left err -> pure . Just $ mkErrorResponse request err
  NotificationMessage notification -> do
    handleNotification server notification
    pure Nothing
  _ -> do
    throwIO $ TransportError $ "Received unexpected message type in handleMessage: " <> show msg

-- Helper to send error responses
mkErrorResponse :: Request -> SomeException -> Message
mkErrorResponse request err =
  let errorResponse =
        Response
          { responseJsonrpc = JSONRPC "2.0",
            responseId = requestId request,
            responseResult = Nothing,
            responseError =
              Just $
                ErrorResponse
                  { errorCode = -32603,
                    errorMessage = T.pack $ show err,
                    errorData = Nothing
                  }
          }
   in (ResponseMessage errorResponse)

-- | Handle a request. @mToken@ is passed through to whichever registered
-- handler is dispatched to for @method@ -- only 'registerCallToolHandler'
-- actually reads it (into the 'ToolCallHandler' it was given); every other
-- registered handler ignores it.
handleRequest :: Server -> Maybe BearerToken -> Request -> IO (Either SomeException Response)
handleRequest server mToken request = do
  let method = requestMethod request
      params = fromMaybe Null (requestParams request)

  handlers <- readTVarIO (serverMessageHandlers server)
  case Map.lookup method handlers of
    Just handler -> do
      e'result <- try $ handler params server mToken
      case e'result of
        -- The handler ran without throwing, but reported a business-level
        -- error (e.g. invalid params). Treat this the same as a thrown
        -- exception: both flow through 'mkErrorResponse' in 'handleMessage'
        -- rather than crashing the server with 'error'.
        Right (Left err) -> return $ Left err
        Right (Right v) ->
          return $
            Right $
              Response
                { responseJsonrpc = JSONRPC "2.0",
                  responseId = requestId request,
                  responseResult = Just v,
                  responseError = Nothing
                }
        Left err -> return $ Left err
    Nothing ->
      return $
        Right $
          Response
            { responseJsonrpc = JSONRPC "2.0",
              responseId = requestId request,
              responseResult = Nothing,
              responseError =
                Just $
                  ErrorResponse
                    { errorCode = -32601,
                      errorMessage = "Method not found",
                      errorData = Nothing
                    }
            }

-- | Handle a notification
handleNotification :: Server -> Notification -> IO ()
handleNotification _server notification = do
  let _method = notificationMethod notification
      _params = fromMaybe Null (notificationParams notification)

  -- TODO: Implement notification handling logic

  pure ()

-- | Register a request handler
registerRequestHandler :: Server -> Text -> (Value -> Server -> Maybe BearerToken -> IO (Either SomeException Value)) -> IO ()
registerRequestHandler server method handler = atomically $ do
  handlers <- readTVar (serverMessageHandlers server)
  writeTVar (serverMessageHandlers server) (Map.insert method handler handlers)

-- | Register initialize handler
registerInitializeHandler :: Server -> IO ()
registerInitializeHandler server = registerRequestHandler server "initialize" $ \params _ _mToken -> do
  case fromJSON params of
    Success clientOptions ->
      case negotiateProtocolVersion (clientInitProtocolVersion clientOptions) of
        Left err -> return $ Left $ toException (userError (T.unpack err))
        Right negotiatedVersion -> do
          let serverOptions =
                ServerInitializeOptions
                  { serverInitProtocolVersion = negotiatedVersion,
                    serverInitInfo = serverInfo server,
                    serverInitCapabilities = serverCapabilities server,
                    serverInitInstructions = serverInstructions server
                  }

          return $ Right $ toJSON serverOptions
    Error err -> return $ Left $ toException (userError $ "Invalid initialize parameters: " ++ err)

-- | Register list resources handler
registerListResourcesHandler :: Server -> IO ()
registerListResourcesHandler server = registerRequestHandler server "resources/list" $ \_ svr _mToken -> do
  resources <- readTVarIO (serverResources svr)
  let result = ListResourcesResult resources
  return $ Right $ toJSON result

-- | Register read resource handler
registerReadResourceHandler :: Server -> IO ()
registerReadResourceHandler server = registerRequestHandler server "resources/read" $ \params svr _mToken -> do
  handlerM <- readTVarIO (serverResourceReadHandler svr)
  case handlerM of
    Just handler -> case fromJSON params of
      Success req -> do
        result <- handler req
        return $ Right $ toJSON result
      Error err -> return $ Left $ toException (userError $ "Invalid resource read parameters: " ++ err)
    Nothing -> return $ Left $ toException (userError "No resource read handler registered")

-- | Register list tools handler
registerListToolsHandler :: Server -> IO ()
registerListToolsHandler server = registerRequestHandler server "tools/list" $ \_ svr _mToken -> do
  tools <- readTVarIO (serverTools svr)
  let result = ListToolsResult tools
  return $ Right $ toJSON result

-- | Register call tool handler
registerCallToolHandler :: Server -> IO ()
registerCallToolHandler server = registerRequestHandler server "tools/call" $ \params svr mToken -> do
  handlerM <- readTVarIO (serverToolCallHandler svr)
  case handlerM of
    Just handler -> case fromJSON params of
      Success req -> do
        result <- handler mToken req
        return $ Right $ toJSON result
      Error err -> return $ Left $ toException (userError $ "Invalid tool call parameters: " ++ err)
    Nothing -> return $ Left $ toException (userError "No tool call handler registered")

-- | Register list prompts handler
registerListPromptsHandler :: Server -> IO ()
registerListPromptsHandler server = registerRequestHandler server "prompts/list" $ \_ svr _mToken -> do
  prompts <- readTVarIO (serverPrompts svr)
  let result = ListPromptsResult prompts
  return $ Right $ toJSON result

-- | Register get prompt handler
registerGetPromptHandler :: Server -> IO ()
registerGetPromptHandler server = registerRequestHandler server "prompts/get" $ \params svr _mToken -> do
  handlerM <- readTVarIO (serverPromptHandler svr)
  case handlerM of
    Just handler -> case fromJSON params of
      Success req -> do
        result <- handler req
        return $ Right $ toJSON result
      Error err -> return $ Left $ toException (userError $ "Invalid get prompt parameters: " ++ err)
    Nothing -> return $ Left $ toException (userError "No prompt handler registered")
