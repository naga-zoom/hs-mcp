{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.MCP.Server.StdIO (
  runServerWithSTDIO,
  withSTDIOServer,
)
where

import Network.MCP.Server
import Network.MCP.Transport.StdIO (newSTDIOTransport)
import UnliftIO (Async)

-- | Run an MCP server using STDIO transport, handling messages until the transport is closed.
runServerWithSTDIO :: Server -> IO ()
runServerWithSTDIO server = do
  -- Create the STDIO transport
  stdioTransport <- newSTDIOTransport

  -- Start the transport
  runServerWithTransport server stdioTransport

withSTDIOServer :: Server -> (Async () -> IO a) -> IO a
withSTDIOServer server action = do
  stdioTransport <- newSTDIOTransport
  withTransportServer server stdioTransport action
