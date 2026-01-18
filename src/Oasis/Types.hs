{-# LANGUAGE StrictData #-}

module Oasis.Types where

import Relude
import Data.Aeson (ToJSON(..), FromJSON(..), Value, defaultOptions, fieldLabelModifier, genericToJSON, genericParseJSON, withObject, (.:), (.:?), (.=), object)
import Toml (decode)
import Toml.Schema (FromValue(..), parseTableFromValue, reqKey, optKey)
import Toml.Schema.Generic (GenericTomlTable(..))
import GHC.Generics (Generic)

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

data ToolCall = ToolCall
  { id       :: Text
  , type_    :: Text
  , function :: ToolCallFunction
  } deriving (Show, Eq, Generic)

instance ToJSON ToolCall where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = dropTrailingUnderscore }

instance FromJSON ToolCall where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = dropTrailingUnderscore }

data Message = Message
  { role          :: Text
  , content       :: Text
  , tool_call_id  :: Maybe Text
  , tool_calls    :: Maybe [ToolCall]
  } deriving (Show, Eq, Generic)

instance FromJSON Message where
  parseJSON = withObject "Message" $ \o -> do
    role <- o .: "role"
    content <- fromMaybe "" <$> o .:? "content"
    toolCallId <- o .:? "tool_call_id"
    toolCalls <- o .:? "tool_calls"
    pure Message
      { role
      , content
      , tool_call_id = toolCallId
      , tool_calls = toolCalls
      }

instance ToJSON Message where
  toJSON Message{..} = object $
    [ "role" .= role
    , "content" .= content
    ] <> maybe [] (\v -> ["tool_call_id" .= v]) tool_call_id
      <> maybe [] (\v -> ["tool_calls" .= v]) tool_calls

dropTrailingUnderscore :: String -> String
dropTrailingUnderscore field =
  case reverse field of
    '_':rest -> reverse rest
    _ -> field
