{-# LANGUAGE OverloadedStrings #-}

module Network.MCP.Server.Types where

import Control.Concurrent.STM
import Control.Exception (SomeException)
import Data.Aeson
import Data.Map.Strict (Map)
import Data.Text (Text)
import Network.MCP.Transport.HTTP (BearerToken)
import Network.MCP.Types

-- | Server type with resources, tools, prompts and handlers
data Server = Server
  { serverInfo :: ServerInfo
  -- ^ Server information
  , serverCapabilities :: ServerCapabilities
  -- ^ Server capabilities
  , serverResources :: TVar [Resource]
  -- ^ Available resources
  , serverTools :: TVar [Tool]
  -- ^ Available tools
  , serverPrompts :: TVar [Prompt]
  -- ^ Available prompts
  , serverInstructions :: Text
  -- ^ Instructions
  , serverResourceReadHandler :: TVar (Maybe ResourceReadHandler)
  -- ^ Resource read handler
  , serverToolCallHandler :: TVar (Maybe ToolCallHandler)
  -- ^ Tool call handler
  , serverPromptHandler :: TVar (Maybe PromptHandler)
  -- ^ Prompt handler
  , serverMessageHandlers ::
      TVar (Map Text (Value -> Server -> Maybe BearerToken -> IO (Either SomeException Value)))
  -- ^ Request handlers by method, each given the calling request's real per-request bearer token (if any transport surfaces one -- 'Nothing' for stdio and for any HTTP request that presented none)
  , serverSendNotification :: TVar (Maybe (Value -> IO ()))
  -- ^ Callback to push a JSON-RPC notification on the active transport. 'runServerWithTransport' installs the real callback; before that it's 'Nothing' and 'notify*' helpers no-op safely.
  }

-- | Server handler for message processing
type ServerHandler = Server -> Value -> IO Value

-- | Resource read handler type
type ResourceReadHandler = ReadResourceRequest -> IO ReadResourceResult

{- | Tool call handler type. Takes the calling request's real per-request
bearer token as its first argument -- 'Just' only for an HTTP request
that actually presented an @Authorization: Bearer ...@ header; 'Nothing'
for stdio (which has no per-request credential concept at all -- every
stdio call shares the server's single startup-resolved identity) and for
an HTTP request that presented none. A tool implementation that needs a
per-call credential (e.g. semantic-compiler's execute_query building a
per-call Snowflake client) reads this directly; one that doesn't care
(every other tool today) just ignores it.
-}
type ToolCallHandler = Maybe BearerToken -> CallToolRequest -> IO CallToolResult

-- | Prompt handler type
type PromptHandler = GetPromptRequest -> IO GetPromptResult
