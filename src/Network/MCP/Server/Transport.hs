{-# LANGUAGE OverloadedStrings #-}

-- | Transport selection and dispatch, kept as its own concern separate
-- from tool/resource/prompt registration ("Network.MCP.Server") and from
-- the wire-level transport implementations themselves
-- ("Network.MCP.Transport.StdIO", "Network.MCP.Transport.HTTP"). A caller
-- (typically a CLI entrypoint) builds a 'TransportChoice' once and hands
-- it to 'runServerWithTransportChoice'; nothing downstream needs a boolean
-- flag or to re-derive which transport is active.
module Network.MCP.Server.Transport
  ( TransportChoice (..),
    HttpConfig,
    HttpConfigError (..),
    mkHttpConfig,
    mkHttpConfigOnHost,
    BearerToken (..),
    runServerWithTransportChoice,
  )
where

import Network.MCP.Server (Server)
import Network.MCP.Server.HTTP (runServerWithHTTP)
import Network.MCP.Server.StdIO (runServerWithSTDIO)
import Network.MCP.Transport.HTTP
  ( BearerToken (..),
    HttpConfig,
    HttpConfigError (..),
    mkHttpConfig,
    mkHttpConfigOnHost,
  )

-- | Which of the two MCP-spec-standard transports ("stdio" and
-- "Streamable HTTP") a server should run on. A real sum type rather than
-- a boolean: 'Http' can only be constructed with the 'HttpConfig' it
-- needs, so "HTTP transport with no port" is not representable.
data TransportChoice
  = Stdio
  | Http HttpConfig
  deriving (Show, Eq)

-- | Run the server on whichever transport was chosen.
runServerWithTransportChoice :: Server -> TransportChoice -> IO ()
runServerWithTransportChoice server Stdio = runServerWithSTDIO server
runServerWithTransportChoice server (Http cfg) = runServerWithHTTP server cfg
