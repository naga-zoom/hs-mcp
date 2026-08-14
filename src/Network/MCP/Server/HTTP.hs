{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Run an MCP server over the Streamable HTTP transport. Mirrors
-- "Network.MCP.Server.StdIO"'s @runServerWithSTDIO@ shape, but composes
-- with the HTTP socket loop in "Network.MCP.Transport.HTTP" instead of the
-- STDIO 'Transport' instance, since HTTP's per-request model doesn't fit
-- the persistent-read-loop 'Transport' typeclass. Both entrypoints share
-- the same underlying dispatch: 'Network.MCP.Server.handleMessage'.
module Network.MCP.Server.HTTP
  ( runServerWithHTTP,
  )
where

import Network.MCP.Server (Server, handleMessage)
import Network.MCP.Transport.HTTP (HttpConfig, runHTTPServer)

-- | Run an MCP server using the Streamable HTTP transport, listening until
-- the process is killed.
runServerWithHTTP :: Server -> HttpConfig -> IO ()
runServerWithHTTP server cfg = runHTTPServer cfg (handleMessage server)
