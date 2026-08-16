{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Network.MCP.Types
  ( -- * Server Information
    ServerInfo,
    ClientInfo,
    Implementation (..),

    -- * Capabilities
    ServerCapabilities (..),
    ClientCapabilities (..),
    ResourcesCapability (..),
    ToolsCapability (..),
    PromptsCapability (..),
    SamplingCapability (..),
    RootsCapability (..),

    -- * Resources
    Resource (..),
    ResourceContent (..),
    ResourceContentType (..),

    -- * Tools
    Tool (..),
    ToolContent (..),
    ToolContentType (..),
    ToolAnnotations (..),

    -- * Prompts
    Prompt (..),
    PromptArgument (..),
    PromptMessage (..),
    PromptContentType (..),
    PromptContent (..),

    -- * Roots
    Root (..),

    -- * Protocol Versions
    ProtocolVersion,
    supportedVersions,
    negotiateProtocolVersion,

    -- * Initialization
    ServerInitializeOptions (..),
    ClientInitializeOptions (..),
    ServerInitializeResult,
    ClientInitializeResult,

    -- * Resource Requests
    ListResourcesRequest (..),
    ListResourcesResult (..),
    ReadResourceRequest (..),
    ReadResourceResult (..),
    SubscribeResourceRequest (..),
    SubscribeResourceResult (..),
    UnsubscribeResourceRequest (..),
    UnsubscribeResourceResult (..),

    -- * Tool Requests
    ListToolsRequest (..),
    ListToolsResult (..),
    CallToolRequest (..),
    CallToolResult (..),

    -- * Prompt Requests
    ListPromptsRequest (..),
    ListPromptsResult (..),
    GetPromptRequest (..),
    GetPromptResult (..),

    -- * Roots Requests
    ListRootsRequest (..),
    ListRootsResult (..),

    -- * Notifications
    ResourcesListChangedNotification (..),
    ResourceUpdatedNotification (..),
    ToolsListChangedNotification (..),
    PromptsListChangedNotification (..),
  )
where

import Data.Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics

-- | Protocol version
type ProtocolVersion = Text

-- | Supported protocol versions
supportedVersions :: [ProtocolVersion]
supportedVersions =
  [ "2025-06-18",
    "2024-11-05"
  ]

-- | Negotiate an exact protocol version with an initializing client.
--
-- MCP versions are not silently downgraded: returning a different version
-- than the one requested can make a strict client interpret the handshake
-- under the wrong wire contract. Add a version to 'supportedVersions' before
-- accepting it here.
negotiateProtocolVersion :: ProtocolVersion -> Either Text ProtocolVersion
negotiateProtocolVersion requested
  | requested `elem` supportedVersions = Right requested
  | otherwise = Left ("unsupported protocol version: " <> requested)

-- | Implementation information
data Implementation = Implementation
  { -- | Name of the implementation
    serverName :: Text,
    -- | Version of the implementation
    serverVersion :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON Implementation where
  toJSON Implementation {..} =
    object
      [ "name" .= serverName,
        "version" .= serverVersion
      ]

instance FromJSON Implementation where
  parseJSON = withObject "Implementation" $ \o -> do
    name <- o .: "name"
    version <- o .: "version"
    return $ Implementation name version

-- | Server information sent during initialization
type ServerInfo = Implementation

-- | Client information sent during initialization
type ClientInfo = Implementation

-- | Resources capability configuration
data ResourcesCapability = ResourcesCapability
  { -- | Server can notify when resources list changes
    resourcesListChanged :: Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON ResourcesCapability where
  toJSON ResourcesCapability {..} =
    object
      [ "listChanged" .= resourcesListChanged
      ]

instance FromJSON ResourcesCapability where
  parseJSON = withObject "ResourcesCapability" $ \o -> do
    listChanged <- o .: "listChanged"
    return $ ResourcesCapability listChanged

-- | Tools capability configuration
data ToolsCapability = ToolsCapability
  { -- | Server can notify when tools list changes
    toolsListChanged :: Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON ToolsCapability where
  toJSON ToolsCapability {..} =
    object
      [ "listChanged" .= toolsListChanged
      ]

instance FromJSON ToolsCapability where
  parseJSON = withObject "ToolsCapability" $ \o -> do
    listChanged <- o .: "listChanged"
    return $ ToolsCapability listChanged

-- | Prompts capability configuration
data PromptsCapability = PromptsCapability
  { -- | Server can notify when prompts list changes
    promptsListChanged :: Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON PromptsCapability where
  toJSON PromptsCapability {..} =
    object
      [ "listChanged" .= promptsListChanged
      ]

instance FromJSON PromptsCapability where
  parseJSON = withObject "PromptsCapability" $ \o -> do
    listChanged <- o .: "listChanged"
    return $ PromptsCapability listChanged

-- | Sampling capability configuration
data SamplingCapability = SamplingCapability
  deriving (Show, Eq, Generic)

instance ToJSON SamplingCapability where
  toJSON _ = object []

instance FromJSON SamplingCapability where
  parseJSON = withObject "SamplingCapability" $ \_ ->
    return SamplingCapability

-- | Roots capability configuration
data RootsCapability = RootsCapability
  deriving (Show, Eq, Generic)

instance ToJSON RootsCapability where
  toJSON _ = object []

instance FromJSON RootsCapability where
  parseJSON = withObject "RootsCapability" $ \_ ->
    return RootsCapability

-- | Server capabilities
data ServerCapabilities = ServerCapabilities
  { -- | Resources support
    resourcesCapability :: Maybe ResourcesCapability,
    -- | Tools support
    toolsCapability :: Maybe ToolsCapability,
    -- | Prompts support
    promptsCapability :: Maybe PromptsCapability
  }
  deriving (Show, Eq, Generic)

instance ToJSON ServerCapabilities where
  toJSON ServerCapabilities {..} =
    object $
      ["resources" .= resources | resources <- maybeToList resourcesCapability]
        ++ ["tools" .= tools | tools <- maybeToList toolsCapability]
        ++ ["prompts" .= prompts | prompts <- maybeToList promptsCapability]

instance FromJSON ServerCapabilities where
  parseJSON = withObject "ServerCapabilities" $ \o -> do
    resources <- o .:? "resources"
    tools <- o .:? "tools"
    prompts <- o .:? "prompts"
    return $ ServerCapabilities resources tools prompts

-- | Client capabilities
data ClientCapabilities = ClientCapabilities
  { -- | Roots support
    clientRootsCapability :: Maybe RootsCapability,
    -- | Sampling support
    clientSamplingCapability :: Maybe SamplingCapability
  }
  deriving (Show, Eq, Generic)

instance ToJSON ClientCapabilities where
  toJSON ClientCapabilities {..} =
    object $
      ["roots" .= roots | roots <- maybeToList clientRootsCapability]
        ++ ["sampling" .= sampling | sampling <- maybeToList clientSamplingCapability]

instance FromJSON ClientCapabilities where
  parseJSON = withObject "ClientCapabilities" $ \o -> do
    roots <- o .:? "roots"
    sampling <- o .:? "sampling"
    return $ ClientCapabilities roots sampling

-- | Resource definition
data Resource = Resource
  { -- | URI of the resource
    resourceUri :: Text,
    -- | Human-readable name
    resourceName :: Text,
    -- | Optional description
    resourceDescription :: Maybe Text,
    -- | Optional MIME type
    resourceMimeType :: Maybe Text,
    -- | Optional URI template definition
    resourceTemplate :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON Resource where
  toJSON Resource {..} =
    object $
      [ "uri" .= resourceUri,
        "name" .= resourceName
      ]
        ++ ["description" .= d | d <- maybeToList resourceDescription]
        ++ ["mimeType" .= m | m <- maybeToList resourceMimeType]
        ++ ["template" .= t | t <- maybeToList resourceTemplate]

instance FromJSON Resource where
  parseJSON = withObject "Resource" $ \o -> do
    uri <- o .: "uri"
    name <- o .: "name"
    description <- o .:? "description"
    mimeType <- o .:? "mimeType"
    template <- o .:? "template"
    return $ Resource uri name description mimeType template

-- | Resource content type
data ResourceContentType = TextContent | BlobContent
  deriving (Show, Eq, Generic)

-- | Resource content
data ResourceContent = ResourceContent
  { -- | URI of the resource
    resourceContentUri :: Text,
    -- | Optional MIME type
    resourceContentMimeType :: Maybe Text,
    -- | Text content (if TextContent)
    resourceContentText :: Maybe Text,
    -- | Blob content (base64 encoded, if BlobContent)
    resourceContentBlob :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON ResourceContent where
  toJSON ResourceContent {..} =
    object $
      [ "uri" .= resourceContentUri
      ]
        ++ ["mimeType" .= m | m <- maybeToList resourceContentMimeType]
        ++ ["text" .= t | t <- maybeToList resourceContentText]
        ++ ["blob" .= b | b <- maybeToList resourceContentBlob]

instance FromJSON ResourceContent where
  parseJSON = withObject "ResourceContent" $ \o -> do
    uri <- o .: "uri"
    mimeType <- o .:? "mimeType"
    text <- o .:? "text"
    blob <- o .:? "blob"
    return $ ResourceContent uri mimeType text blob

data ToolAnnotations = ToolAnnotations
  { -- | Human-readable title for the tool
    title :: Maybe Text,
    -- | If true, the tool does not modify its environment
    readOnlyHint :: Maybe Bool,
    -- | If true, the tool may perform destructive updates
    destructiveHint :: Maybe Bool,
    -- | If true, repeated calls with same args have no additional effect
    idempotentHint :: Maybe Bool,
    -- | If true, tool interacts with external entities
    openWorldHint :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON ToolAnnotations where
  toJSON ToolAnnotations {..} =
    object $
      [ "title" .= title,
        "readOnly" .= readOnlyHint,
        "destructive" .= destructiveHint,
        "idempotent" .= idempotentHint,
        "openWorld" .= openWorldHint
      ]

instance FromJSON ToolAnnotations where
  parseJSON = withObject "ToolAnnotations" $ \o -> do
    title <- o .:? "title"
    readOnly <- o .:? "readOnly"
    destructive <- o .:? "destructive"
    idempotent <- o .:? "idempotent"
    openWorld <- o .:? "openWorld"
    return $ ToolAnnotations title readOnly destructive idempotent openWorld

-- | Tool definition
data Tool = Tool
  { -- | Name of the tool
    toolName :: Text,
    -- | Optional description
    toolDescription :: Maybe Text,
    -- | JSON schema for tool parameters
    toolInputSchema :: Value,
    -- | Optional annotations
    toolAnnotations :: Maybe ToolAnnotations
  }
  deriving (Show, Eq, Generic)

instance ToJSON Tool where
  toJSON Tool {..} =
    object $
      [ "name" .= toolName,
        "inputSchema" .= toolInputSchema
      ]
        <> ["description" .= d | d <- maybeToList toolDescription]
        <> ["annotations" .= a | a <- maybeToList toolAnnotations]

instance FromJSON Tool where
  parseJSON = withObject "Tool" $ \o -> do
    name <- o .: "name"
    description <- o .:? "description"
    inputSchema <- o .: "inputSchema"
    annotations <- o .:? "annotations"
    return $ Tool name description inputSchema annotations

-- | Tool content type
data ToolContentType = TextualContent | ImageContent | EmbeddedResource
  deriving (Show, Eq, Generic)

instance ToJSON ToolContentType where
  toJSON TextualContent = String "text"
  toJSON ImageContent = String "image"
  toJSON EmbeddedResource = String "resource"

instance FromJSON ToolContentType where
  parseJSON = withText "ToolContentType" $ \case
    "text" -> return TextualContent
    "image" -> return ImageContent
    "resource" -> return EmbeddedResource
    _ -> fail "Invalid tool content type"

-- | Tool content
data ToolContent = ToolContent
  { -- | Type of content
    toolContentType :: ToolContentType,
    -- | Text content (if TextContent)
    toolContentText :: Maybe Text,
    -- | Embedded resource content (if EmbeddedResource).
    toolContentResource :: Maybe ResourceContent
  }
  deriving (Show, Eq, Generic)

instance ToJSON ToolContent where
  toJSON ToolContent {..} =
    object $
      [ "type" .= toolContentType
      ]
        ++ ["text" .= t | t <- maybeToList toolContentText]
        ++ ["resource" .= r | r <- maybeToList toolContentResource]

instance FromJSON ToolContent where
  parseJSON = withObject "ToolContent" $ \o -> do
    contentType <- o .: "type"
    text <- o .:? "text"
    resource <- o .:? "resource"
    return $ ToolContent contentType text resource

-- | Prompt argument definition
data PromptArgument = PromptArgument
  { -- | Argument name
    promptArgumentName :: Text,
    -- | Optional description
    promptArgumentDescription :: Maybe Text,
    -- | Whether the argument is required
    promptArgumentRequired :: Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON PromptArgument where
  toJSON PromptArgument {..} =
    object $
      [ "name" .= promptArgumentName,
        "required" .= promptArgumentRequired
      ]
        ++ ["description" .= d | d <- maybeToList promptArgumentDescription]

instance FromJSON PromptArgument where
  parseJSON = withObject "PromptArgument" $ \o -> do
    name <- o .: "name"
    description <- o .:? "description"
    required <- o .: "required"
    return $ PromptArgument name description required

-- | Prompt definition
data Prompt = Prompt
  { -- | Name of the prompt
    promptName :: Text,
    -- | Optional description
    promptDescription :: Maybe Text,
    -- | Arguments for the prompt
    promptArguments :: [PromptArgument]
  }
  deriving (Show, Eq, Generic)

instance ToJSON Prompt where
  toJSON Prompt {..} =
    object $
      [ "name" .= promptName,
        "arguments" .= promptArguments
      ]
        ++ ["description" .= d | d <- maybeToList promptDescription]

instance FromJSON Prompt where
  parseJSON = withObject "Prompt" $ \o -> do
    name <- o .: "name"
    description <- o .:? "description"
    arguments <- o .: "arguments"
    return $ Prompt name description arguments

-- | Prompt content type
data PromptContentType = TextPromptContent | ResourcePromptContent
  deriving (Show, Eq, Generic)

instance ToJSON PromptContentType where
  toJSON TextPromptContent = String "text"
  toJSON ResourcePromptContent = String "resource"

instance FromJSON PromptContentType where
  parseJSON = withText "PromptContentType" $ \case
    "text" -> return TextPromptContent
    "resource" -> return ResourcePromptContent
    _ -> fail "Invalid prompt content type"

-- | Prompt content
data PromptContent = PromptContent
  { -- | Type of content
    promptContentType :: PromptContentType,
    -- | Content text
    promptContentText :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON PromptContent where
  toJSON PromptContent {..} =
    object
      [ "type" .= promptContentType,
        "text" .= promptContentText
      ]

instance FromJSON PromptContent where
  parseJSON = withObject "PromptContent" $ \o -> do
    contentType <- o .: "type"
    text <- o .: "text"
    return $ PromptContent contentType text

-- | Prompt message
data PromptMessage = PromptMessage
  { -- | Message role (user/assistant)
    promptMessageRole :: Text,
    -- | Message content
    promptMessageContent :: PromptContent
  }
  deriving (Show, Eq, Generic)

instance ToJSON PromptMessage where
  toJSON PromptMessage {..} =
    object
      [ "role" .= promptMessageRole,
        "content" .= promptMessageContent
      ]

instance FromJSON PromptMessage where
  parseJSON = withObject "PromptMessage" $ \o -> do
    role <- o .: "role"
    content <- o .: "content"
    return $ PromptMessage role content

-- | Root definition
data Root = Root
  { -- | URI for the root
    rootUri :: Text,
    -- | Human-readable name
    rootName :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON Root where
  toJSON Root {..} =
    object
      [ "uri" .= rootUri,
        "name" .= rootName
      ]

instance FromJSON Root where
  parseJSON = withObject "Root" $ \o -> do
    uri <- o .: "uri"
    name <- o .: "name"
    return $ Root uri name

-- | Server initialize options
data ServerInitializeOptions = ServerInitializeOptions
  { -- | Protocol version
    serverInitProtocolVersion :: ProtocolVersion,
    -- | Server info
    serverInitInfo :: Implementation,
    -- | Server capabilities
    serverInitCapabilities :: ServerCapabilities,
    serverInitInstructions :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON ServerInitializeOptions where
  toJSON ServerInitializeOptions {..} =
    object
      [ "protocolVersion" .= serverInitProtocolVersion,
        "serverInfo" .= serverInitInfo,
        "capabilities" .= serverInitCapabilities,
        "instructions" .= serverInitInstructions
      ]

instance FromJSON ServerInitializeOptions where
  parseJSON = withObject "ServerInitializeOptions" $ \o -> do
    version <- o .: "protocolVersion"
    impl <- o .: "serverInfo"
    capabilities <- o .: "capabilities"
    instructions <- o .: "instructions"
    return $ ServerInitializeOptions version impl capabilities instructions

-- | Client initialize options
data ClientInitializeOptions = ClientInitializeOptions
  { -- | Protocol version
    clientInitProtocolVersion :: ProtocolVersion,
    -- | Client info
    clientInitInfo :: Implementation,
    -- | Client capabilities
    clientInitCapabilities :: ClientCapabilities
  }
  deriving (Show, Eq, Generic)

instance ToJSON ClientInitializeOptions where
  toJSON ClientInitializeOptions {..} =
    object
      [ "protocolVersion" .= clientInitProtocolVersion,
        "clientInfo" .= clientInitInfo,
        "capabilities" .= clientInitCapabilities
      ]

instance FromJSON ClientInitializeOptions where
  parseJSON = withObject "ClientInitializeOptions" $ \o -> do
    version <- o .: "protocolVersion"
    impl <- o .: "clientInfo"
    capabilities <- o .: "capabilities"
    return $ ClientInitializeOptions version impl capabilities

-- | Server initialize result
type ServerInitializeResult = ClientInitializeOptions

-- | Client initialize result
type ClientInitializeResult = ServerInitializeOptions

-- | List resources request
data ListResourcesRequest = ListResourcesRequest
  deriving (Show, Eq, Generic)

instance ToJSON ListResourcesRequest where
  toJSON _ = object []

instance FromJSON ListResourcesRequest where
  parseJSON = withObject "ListResourcesRequest" $ \_ ->
    return ListResourcesRequest

-- | List resources result
data ListResourcesResult = ListResourcesResult
  { -- | Available resources
    listResourcesResult :: [Resource]
  }
  deriving (Show, Eq, Generic)

instance ToJSON ListResourcesResult where
  toJSON ListResourcesResult {..} =
    object
      [ "resources" .= listResourcesResult
      ]

instance FromJSON ListResourcesResult where
  parseJSON = withObject "ListResourcesResult" $ \o -> do
    resources <- o .: "resources"
    return $ ListResourcesResult resources

-- | Read resource request
data ReadResourceRequest = ReadResourceRequest
  { -- | URI of the resource to read
    resourceReadUri :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON ReadResourceRequest where
  toJSON ReadResourceRequest {..} =
    object
      [ "uri" .= resourceReadUri
      ]

instance FromJSON ReadResourceRequest where
  parseJSON = withObject "ReadResourceRequest" $ \o -> do
    uri <- o .: "uri"
    return $ ReadResourceRequest uri

-- | Read resource result
data ReadResourceResult = ReadResourceResult
  { -- | Resource contents
    readResourceContents :: [ResourceContent]
  }
  deriving (Show, Eq, Generic)

instance ToJSON ReadResourceResult where
  toJSON ReadResourceResult {..} =
    object
      [ "contents" .= readResourceContents
      ]

instance FromJSON ReadResourceResult where
  parseJSON = withObject "ReadResourceResult" $ \o -> do
    contents <- o .: "contents"
    return $ ReadResourceResult contents

-- | Subscribe resource request
data SubscribeResourceRequest = SubscribeResourceRequest
  { -- | URI of the resource to subscribe to
    subscribeResourceUri :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON SubscribeResourceRequest where
  toJSON SubscribeResourceRequest {..} =
    object
      [ "uri" .= subscribeResourceUri
      ]

instance FromJSON SubscribeResourceRequest where
  parseJSON = withObject "SubscribeResourceRequest" $ \o -> do
    uri <- o .: "uri"
    return $ SubscribeResourceRequest uri

-- | Subscribe resource result
data SubscribeResourceResult = SubscribeResourceResult
  deriving (Show, Eq, Generic)

instance ToJSON SubscribeResourceResult where
  toJSON _ = object []

instance FromJSON SubscribeResourceResult where
  parseJSON = withObject "SubscribeResourceResult" $ \_ ->
    return SubscribeResourceResult

-- | Unsubscribe resource request
data UnsubscribeResourceRequest = UnsubscribeResourceRequest
  { -- | URI of the resource to unsubscribe from
    unsubscribeResourceUri :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON UnsubscribeResourceRequest where
  toJSON UnsubscribeResourceRequest {..} =
    object
      [ "uri" .= unsubscribeResourceUri
      ]

instance FromJSON UnsubscribeResourceRequest where
  parseJSON = withObject "UnsubscribeResourceRequest" $ \o -> do
    uri <- o .: "uri"
    return $ UnsubscribeResourceRequest uri

-- | Unsubscribe resource result
data UnsubscribeResourceResult = UnsubscribeResourceResult
  deriving (Show, Eq, Generic)

instance ToJSON UnsubscribeResourceResult where
  toJSON _ = object []

instance FromJSON UnsubscribeResourceResult where
  parseJSON = withObject "UnsubscribeResourceResult" $ \_ ->
    return UnsubscribeResourceResult

-- | List tools request
data ListToolsRequest = ListToolsRequest
  deriving (Show, Eq, Generic)

instance ToJSON ListToolsRequest where
  toJSON _ = object []

instance FromJSON ListToolsRequest where
  parseJSON = withObject "ListToolsRequest" $ \_ ->
    return ListToolsRequest

-- | List tools result
data ListToolsResult = ListToolsResult
  { -- | Available tools
    listToolsResult :: [Tool]
  }
  deriving (Show, Eq, Generic)

instance ToJSON ListToolsResult where
  toJSON ListToolsResult {..} =
    object
      [ "tools" .= listToolsResult
      ]

instance FromJSON ListToolsResult where
  parseJSON = withObject "ListToolsResult" $ \o -> do
    tools <- o .: "tools"
    return $ ListToolsResult tools

-- | Call tool request
data CallToolRequest = CallToolRequest
  { -- | Name of the tool to call
    callToolName :: Text,
    -- | Tool arguments
    callToolArguments :: Value
  }
  deriving (Show, Eq, Generic)

instance ToJSON CallToolRequest where
  toJSON CallToolRequest {..} =
    object
      [ "name" .= callToolName,
        "arguments" .= callToolArguments
      ]

instance FromJSON CallToolRequest where
  parseJSON = withObject "CallToolRequest" $ \o -> do
    name <- o .: "name"
    arguments <- o .: "arguments"
    return $ CallToolRequest name arguments

-- | Call tool result
data CallToolResult = CallToolResult
  { -- | Tool execution result content
    callToolContent :: [ToolContent],
    -- | Whether the result is an error
    callToolIsError :: Bool,
    -- | Structured tool output, when the tool has a machine-readable result.
    callToolStructuredContent :: Maybe Value
  }
  deriving (Show, Eq, Generic)

instance ToJSON CallToolResult where
  toJSON CallToolResult {..} =
    object $
      [ "content" .= callToolContent,
        "isError" .= callToolIsError
      ]
        ++ ["structuredContent" .= value | value <- maybeToList callToolStructuredContent]

instance FromJSON CallToolResult where
  parseJSON = withObject "CallToolResult" $ \o -> do
    content <- o .: "content"
    isError <- o .: "isError"
    structuredContent <- o .:? "structuredContent"
    return $ CallToolResult content isError structuredContent

-- | List prompts request
data ListPromptsRequest = ListPromptsRequest
  deriving (Show, Eq, Generic)

instance ToJSON ListPromptsRequest where
  toJSON _ = object []

instance FromJSON ListPromptsRequest where
  parseJSON = withObject "ListPromptsRequest" $ \_ ->
    return ListPromptsRequest

-- | List prompts result
data ListPromptsResult = ListPromptsResult
  { -- | Available prompts
    listPromptsResult :: [Prompt]
  }
  deriving (Show, Eq, Generic)

instance ToJSON ListPromptsResult where
  toJSON ListPromptsResult {..} =
    object
      [ "prompts" .= listPromptsResult
      ]

instance FromJSON ListPromptsResult where
  parseJSON = withObject "ListPromptsResult" $ \o -> do
    prompts <- o .: "prompts"
    return $ ListPromptsResult prompts

-- | Get prompt request
data GetPromptRequest = GetPromptRequest
  { -- | Name of the prompt to get
    getPromptName :: Text,
    -- | Prompt arguments
    getPromptArguments :: Map Text Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON GetPromptRequest where
  toJSON GetPromptRequest {..} =
    object
      [ "name" .= getPromptName,
        "arguments" .= getPromptArguments
      ]

instance FromJSON GetPromptRequest where
  parseJSON = withObject "GetPromptRequest" $ \o -> do
    name <- o .: "name"
    arguments <- o .:? "arguments" .!= Map.empty
    return $ GetPromptRequest name arguments

-- | Get prompt result
data GetPromptResult = GetPromptResult
  { -- | Optional description
    getPromptDescription :: Maybe Text,
    -- | Prompt messages
    getPromptMessages :: [PromptMessage]
  }
  deriving (Show, Eq, Generic)

instance ToJSON GetPromptResult where
  toJSON GetPromptResult {..} =
    object $
      [ "messages" .= getPromptMessages
      ]
        ++ ["description" .= d | d <- maybeToList getPromptDescription]

instance FromJSON GetPromptResult where
  parseJSON = withObject "GetPromptResult" $ \o -> do
    description <- o .:? "description"
    messages <- o .: "messages"
    return $ GetPromptResult description messages

-- | List roots request
data ListRootsRequest = ListRootsRequest
  deriving (Show, Eq, Generic)

instance ToJSON ListRootsRequest where
  toJSON _ = object []

instance FromJSON ListRootsRequest where
  parseJSON = withObject "ListRootsRequest" $ \_ ->
    return ListRootsRequest

-- | List roots result
data ListRootsResult = ListRootsResult
  { -- | Available roots
    listRootsResult :: [Root]
  }
  deriving (Show, Eq, Generic)

instance ToJSON ListRootsResult where
  toJSON ListRootsResult {..} =
    object
      [ "roots" .= listRootsResult
      ]

instance FromJSON ListRootsResult where
  parseJSON = withObject "ListRootsResult" $ \o -> do
    roots <- o .: "roots"
    return $ ListRootsResult roots

-- | Resources list changed notification
data ResourcesListChangedNotification = ResourcesListChangedNotification
  deriving (Show, Eq, Generic)

instance ToJSON ResourcesListChangedNotification where
  toJSON _ = object []

instance FromJSON ResourcesListChangedNotification where
  parseJSON = withObject "ResourcesListChangedNotification" $ \_ ->
    return ResourcesListChangedNotification

-- | Resource updated notification
data ResourceUpdatedNotification = ResourceUpdatedNotification
  { -- | URI of the updated resource
    resourceUpdatedUri :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON ResourceUpdatedNotification where
  toJSON ResourceUpdatedNotification {..} =
    object
      [ "uri" .= resourceUpdatedUri
      ]

instance FromJSON ResourceUpdatedNotification where
  parseJSON = withObject "ResourceUpdatedNotification" $ \o -> do
    uri <- o .: "uri"
    return $ ResourceUpdatedNotification uri

-- | Tools list changed notification
data ToolsListChangedNotification = ToolsListChangedNotification
  deriving (Show, Eq, Generic)

instance ToJSON ToolsListChangedNotification where
  toJSON _ = object []

instance FromJSON ToolsListChangedNotification where
  parseJSON = withObject "ToolsListChangedNotification" $ \_ ->
    return ToolsListChangedNotification

-- | Prompts list changed notification
data PromptsListChangedNotification = PromptsListChangedNotification
  deriving (Show, Eq, Generic)

instance ToJSON PromptsListChangedNotification where
  toJSON _ = object []

instance FromJSON PromptsListChangedNotification where
  parseJSON = withObject "PromptsListChangedNotification" $ \_ ->
    return PromptsListChangedNotification

-- Helper function
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]
