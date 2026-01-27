{-# LANGUAGE StrictData #-}

module Oasis.Types
  ( ModelType(..)
  , Provider(..)
  , Config(..)
  , Defaults(..)
  , RequestResponse(..)
  , ToolFunctionSpec(..)
  , Tool(..)
  , ToolCallFunction(..)
  , StopParam(..)
  , ToolCall(..)
  , MessageContent(..)
  , Message(..)
  , messageContentText
  , dropTrailingUnderscore
  ) where

import Relude
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), defaultOptions, fieldLabelModifier, genericToJSON, genericParseJSON, withArray, withObject, withText, (.:), (.:?), (.=), object)
import qualified Data.Vector as V
import Toml (decode)
import Toml.Schema (FromValue(..), parseTableFromValue, reqKey, optKey)
import Toml.Schema.Generic (GenericTomlTable(..))
import GHC.Generics (Generic)
import qualified Data.Aeson.KeyMap as KM

data ModelType
  = Chat
  | Coder
  | Reasoner
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance FromValue ModelType where
  fromValue v = do
    str <- fromValue @Text v
    case str of
      "chat"     -> pure Chat
      "coder"    -> pure Coder
      "reasoner" -> pure Reasoner
      _          -> fail "Invalid model type"

data Provider = Provider
  { description        :: Text
  , api_key_name       :: Text
  , base_url           :: Text
  , beta_base_url      :: Maybe Text
  , chat_model_id      :: Text
  , coder_model_id     :: Text
  , reasoner_model_id  :: Text
  , embedding_model_id :: Maybe Text
  , default_model_type :: Maybe Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)
    deriving FromValue via GenericTomlTable Provider

data Config = Config
  { providers :: Map Text Provider
  , defaults  :: Defaults
  , aliases   :: Map Text Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)
    deriving FromValue via GenericTomlTable Config

data Defaults = Defaults
  { provider   :: Text
  , model_type :: Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)
    deriving FromValue via GenericTomlTable Defaults

data RequestResponse a = RequestResponse
  { requestJson  :: Text
  , responseJson :: Text
  , response     :: Maybe a
  } deriving (Show, Eq)

data ToolFunctionSpec = ToolFunctionSpec
  { name        :: Text
  , description :: Maybe Text
  , parameters  :: Value
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Tool = Tool
  { type_    :: Text
  , function :: ToolFunctionSpec
  } deriving (Show, Eq, Generic)

instance ToJSON Tool where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = dropTrailingUnderscore }

instance FromJSON Tool where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = dropTrailingUnderscore }

data ToolCallFunction = ToolCallFunction
  { name      :: Text
  , arguments :: Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data StopParam
  = StopText Text
  | StopList [Text]
  deriving (Show, Eq)

instance ToJSON StopParam where
  toJSON = \case
    StopText t -> String t
    StopList xs -> toJSON xs

instance FromJSON StopParam where
  parseJSON v =
    withText "StopParam" (pure . StopText) v
    <|> withArray "StopParam" (\arr -> StopList <$> traverse parseJSON (V.toList arr)) v

data ToolCall = ToolCall
  { id       :: Text
  , type_    :: Text
  , function :: ToolCallFunction
  } deriving (Show, Eq, Generic)

instance ToJSON ToolCall where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = dropTrailingUnderscore }

instance FromJSON ToolCall where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = dropTrailingUnderscore }

data MessageContent
  = ContentText Text
  | ContentParts [MessageContentPart]
  deriving (Show, Eq, Generic)

data MessageContentPart
  = ContentTextPart Text
  | ContentObjectPart Value
  deriving (Show, Eq, Generic)

instance FromJSON MessageContentPart where
  parseJSON v =
    case v of
      String t -> pure (ContentTextPart t)
      Object o ->
        case KM.lookup "type" o of
          Just (String "text") -> ContentTextPart <$> (o .: "text")
          _ -> pure (ContentObjectPart v)
      _ -> pure (ContentObjectPart v)

instance ToJSON MessageContentPart where
  toJSON = \case
    ContentTextPart t -> object ["type" .= ("text" :: Text), "text" .= t]
    ContentObjectPart v -> v

instance FromJSON MessageContent where
  parseJSON v =
    withText "MessageContent" (pure . ContentText) v
    <|> withArray "MessageContent" (\arr -> ContentParts <$> traverse parseJSON (V.toList arr)) v
    <|> case v of
          Null -> pure (ContentText "")
          _ -> fail "Invalid MessageContent"

instance ToJSON MessageContent where
  toJSON = \case
    ContentText t -> String t
    ContentParts parts -> toJSON parts

data Message = Message
  { role          :: Text
  , content       :: MessageContent
  , reasoning_content :: Maybe Text
  , tool_call_id  :: Maybe Text
  , tool_calls    :: Maybe [ToolCall]
  , prefix        :: Maybe Bool
  , partial       :: Maybe Bool
  } deriving (Show, Eq, Generic)

instance FromJSON Message where
  parseJSON = withObject "Message" $ \o -> do
    role <- o .: "role"
    contentValue <- o .:? "content"
    content <- case contentValue of
      Nothing -> pure (ContentText "")
      Just Null -> pure (ContentText "")
      Just v -> parseJSON v
    toolCallId <- o .:? "tool_call_id"
    toolCalls <- o .:? "tool_calls"
    prefix <- o .:? "prefix"
    partial <- o .:? "partial"
    reasoningContent <- o .:? "reasoning_content"
    pure Message
      { role
      , content
      , reasoning_content = reasoningContent
      , tool_call_id = toolCallId
      , tool_calls = toolCalls
      , prefix = prefix
      , partial = partial
      }

instance ToJSON Message where
  toJSON Message{..} = object $
    [ "role" .= role
    , "content" .= content
    ] <> maybe [] (\v -> ["tool_call_id" .= v]) tool_call_id
      <> maybe [] (\v -> ["tool_calls" .= v]) tool_calls
      <> maybe [] (\v -> ["prefix" .= v]) prefix
      <> maybe [] (\v -> ["partial" .= v]) partial
      <> maybe [] (\v -> ["reasoning_content" .= v]) reasoning_content

messageContentText :: MessageContent -> Text
messageContentText = \case
  ContentText t -> t
  ContentParts parts ->
    let pieces = mapMaybe partText parts
    in mconcat pieces
  where
    partText = \case
      ContentTextPart t -> Just t
      ContentObjectPart _ -> Nothing

dropTrailingUnderscore :: String -> String
dropTrailingUnderscore field =
  case reverse field of
    '_':rest -> reverse rest
    _ -> field
