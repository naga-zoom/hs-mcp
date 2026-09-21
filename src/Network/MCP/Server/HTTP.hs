{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | Run an MCP server over the Streamable HTTP transport. Mirrors
"Network.MCP.Server.StdIO"'s @runServerWithSTDIO@ shape, but composes
with the HTTP socket loop in "Network.MCP.Transport.HTTP" instead of the
STDIO 'Transport' instance, since HTTP's per-request model doesn't fit
the persistent-read-loop 'Transport' typeclass. Both entrypoints share
the same underlying dispatch: 'Network.MCP.Server.handleMessage'.
-}
module Network.MCP.Server.HTTP (
  runServerWithHTTP,
)
where

import Network.MCP.Server (Server, handleMessage)
import Network.MCP.Transport.HTTP (HttpConfig, runHTTPServer)

{- | Run an MCP server using the Streamable HTTP transport, listening until
the process is killed. Each request's own real per-request bearer token
(surfaced by 'runHTTPServer', independent of the server's configured
access-gate token) is threaded straight into 'handleMessage', which
passes it on to whichever 'Network.MCP.Server.Types.ToolCallHandler' is
registered for @tools/call@ -- the real typed plumbing a per-call-OAuth
tool implementation needs, not a global/IORef side channel.
-}
runServerWithHTTP :: Server -> HttpConfig -> IO ()
runServerWithHTTP server cfg = runHTTPServer cfg (handleMessage server)
