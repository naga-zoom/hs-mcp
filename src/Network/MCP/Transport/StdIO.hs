{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.MCP.Transport.StdIO
  ( STDIOTransport (..),
    newSTDIOTransport,
  )
where

import Control.Exception (SomeException, handle)
import Data.Aeson
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Network.MCP.Transport.Types
import System.IO

-- | STDIO implementation of the Transport interface
data STDIOTransport = STDIOTransport
  { stdinHandle :: Handle,
    stdoutHandle :: Handle,
    stderrHandle :: Handle
  }

-- | Create a new STDIO transport with the given message handler
newSTDIOTransport ::
  IO STDIOTransport
newSTDIOTransport = do
  -- Configure handles for better performance
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8

  return $
    STDIOTransport
      { stdinHandle = stdin,
        stdoutHandle = stdout,
        stderrHandle = stderr
      }

-- | A simple, synchronous transport implementation using standard input/output.
instance Transport STDIOTransport where
  handleMessages (STDIOTransport {stdinHandle, stderrHandle, stdoutHandle}) handler = do
    hSetBuffering stdinHandle LineBuffering
    hSetBuffering stdoutHandle LineBuffering
    hSetEncoding stdinHandle utf8
    hSetEncoding stdoutHandle utf8
    loop
    where
      loop = do
        handle handleErr $ do
          readMessage >>= \case
            Nothing ->
              -- Transport is closed.
              pure ()
            Just msg -> do
              handler msg >>= \case
                Nothing -> loop
                Just response -> do
                  writeOneLine response
                  loop
      readMessage :: IO (Maybe Message)
      readMessage = do
        hIsEOF stdinHandle >>= \case
          True -> do
            -- If EOF is reached, return Nothing to signal termination
            pure Nothing
          False -> do
            line <- BS8.hGetLine stdinHandle
            case eitherDecode (BS8.fromStrict line) of
              Left err -> do
                -- On parse error, log and try again with a default error message
                hPutStrLn stderrHandle ("JSON decode error: " ++ err)
                -- Try again on the next line.
                readMessage
              Right msg -> pure . Just $ msg

      -- Write one JSON-RPC line on stdout.
      writeOneLine msg = do
        BL.hPut stdoutHandle (encode msg)
        BL.hPut stdoutHandle "\n"
        hFlush stdoutHandle

      handleErr :: SomeException -> IO ()
      handleErr err = do
        hPutStrLn stderrHandle $ "Error reading message: " ++ show err
        loop

  -- | Server-initiated notification path. Writes a single JSON-RPC
  -- line on stdout, matching the framing used for normal responses.
  -- Handle writes in GHC are atomic at the OS-buffer level so this
  -- composes safely with the response writer in 'handleMessages'.
  sendMessage (STDIOTransport {stdoutHandle}) msg = do
    BL.hPut stdoutHandle (encode msg)
    BL.hPut stdoutHandle "\n"
    hFlush stdoutHandle
