{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Oasis.Client.OpenAI.Types
  ( ChatCompletionRequest(..)
  , ChatCompletionResponse(..)
  , ChatChoice(..)
  , Usage(..)
  , ChatCompletionStreamChunk(..)
  , StreamChoice(..)
  , StreamDelta(..)
  , defaultChatRequest
  , setChatStream
  , setChatResponseFormat
  , EmbeddingRequest(..)
  , EmbeddingResponse(..)
  , EmbeddingData(..)
  , EmbeddingUsage(..)
  , ResponsesRequest(..)
  , ResponsesResponse(..)
  , ErrorDetail(..)
  , ErrorResponse(..)
  , ClientError(..)
  ) where

import Relude
import Oasis.Types
import Data.Aeson

data ChatCompletionRequest = ChatCompletionRequest
  { model       :: Text
  , messages    :: [Message]
  , temperature :: Maybe Double
  , top_p       :: Maybe Double
  , max_completion_tokens :: Maybe Int
  , stop        :: Maybe StopParam
  , presence_penalty :: Maybe Double
  , frequency_penalty :: Maybe Double
  , seed        :: Maybe Int
  , logit_bias  :: Maybe Value
  , user        :: Maybe Text
  , service_tier :: Maybe Text
  , reasoning_effort :: Maybe Text
  , stream_options :: Maybe Value
  , stream      :: Bool
  , response_format :: Maybe Value
  , tools       :: Maybe [Tool]
  , tool_choice :: Maybe Value
  , parallel_tool_calls :: Maybe Bool
  } deriving (Show, Eq, Generic)

instance ToJSON ChatCompletionRequest where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

instance FromJSON ChatCompletionRequest where
  parseJSON = genericParseJSON defaultOptions

defaultChatRequest :: Text -> [Message] -> ChatCompletionRequest
defaultChatRequest modelId msgs =
  ChatCompletionRequest
    { model = modelId
    , messages = msgs
    , temperature = Nothing
    , top_p = Nothing
    , max_completion_tokens = Nothing
    , stop = Nothing
    , presence_penalty = Nothing
    , frequency_penalty = Nothing
    , seed = Nothing
    , logit_bias = Nothing
    , user = Nothing
    , service_tier = Nothing
    , reasoning_effort = Nothing
    , stream_options = Nothing
    , stream = False
    , response_format = Nothing
    , tools = Nothing
    , tool_choice = Nothing
    , parallel_tool_calls = Nothing
    }

setChatStream :: Bool -> ChatCompletionRequest -> ChatCompletionRequest
setChatStream enabled ChatCompletionRequest{..} =
  ChatCompletionRequest
    { stream = enabled
    , ..
    }

setChatResponseFormat :: Maybe Value -> ChatCompletionRequest -> ChatCompletionRequest
setChatResponseFormat fmt ChatCompletionRequest{..} =
  ChatCompletionRequest
    { response_format = fmt
    , ..
    }

data ChatCompletionResponse = ChatCompletionResponse
  { id      :: Maybe Text
  , object  :: Maybe Text
  , created :: Maybe Int
  , model   :: Maybe Text
  , choices :: [ChatChoice]
  , usage   :: Maybe Usage
  } deriving (Show, Eq, Generic)

instance FromJSON ChatCompletionResponse where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ChatCompletionResponse where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

data ChatChoice = ChatChoice
  { index         :: Maybe Int
  , message       :: Maybe Message
  , finish_reason :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON ChatChoice where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ChatChoice where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

data Usage = Usage
  { prompt_tokens     :: Maybe Int
  , completion_tokens :: Maybe Int
  , total_tokens      :: Maybe Int
  } deriving (Show, Eq, Generic)

instance FromJSON Usage where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Usage where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

data StreamDelta = StreamDelta
  { role    :: Maybe Text
  , reasoning :: Maybe Text
  , thinking :: Maybe Text
  , reasoning_content :: Maybe Text
  , content :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON StreamDelta where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON StreamDelta where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

data StreamChoice = StreamChoice
  { index         :: Maybe Int
  , delta         :: Maybe StreamDelta
  , finish_reason :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON StreamChoice where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON StreamChoice where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

data ChatCompletionStreamChunk = ChatCompletionStreamChunk
  { id      :: Maybe Text
  , object  :: Maybe Text
  , created :: Maybe Int
  , model   :: Maybe Text
  , choices :: [StreamChoice]
  } deriving (Show, Eq, Generic)

instance FromJSON ChatCompletionStreamChunk where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ChatCompletionStreamChunk where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

data EmbeddingRequest = EmbeddingRequest
  { model :: Text
  , input :: Value
  , encoding_format :: Maybe Text
  , dimensions :: Maybe Int
  , user :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON EmbeddingRequest where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

instance FromJSON EmbeddingRequest where
  parseJSON = genericParseJSON defaultOptions

data EmbeddingData = EmbeddingData
  { index :: Int
  , embedding :: [Double]
  , object :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON EmbeddingData where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON EmbeddingData where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

data EmbeddingUsage = EmbeddingUsage
  { prompt_tokens :: Maybe Int
  , total_tokens :: Maybe Int
  } deriving (Show, Eq, Generic)

instance FromJSON EmbeddingUsage where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON EmbeddingUsage where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

data EmbeddingResponse = EmbeddingResponse
  { object :: Maybe Text
  , data_ :: [EmbeddingData]
  , model :: Maybe Text
  , usage :: Maybe EmbeddingUsage
  } deriving (Show, Eq, Generic)

instance FromJSON EmbeddingResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = dropTrailingUnderscore }

instance ToJSON EmbeddingResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = dropTrailingUnderscore, omitNothingFields = True }

data ResponsesRequest = ResponsesRequest
  { model :: Text
  , input :: Value
  , stream :: Maybe Bool
  , max_output_tokens :: Maybe Int
  , temperature :: Maybe Double
  , top_p :: Maybe Double
  , user :: Maybe Text
  , response_format :: Maybe Value
  } deriving (Show, Eq, Generic)

instance ToJSON ResponsesRequest where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

instance FromJSON ResponsesRequest where
  parseJSON = genericParseJSON defaultOptions

data ResponsesResponse = ResponsesResponse
  { id :: Maybe Text
  , object :: Maybe Text
  , created :: Maybe Int
  , model :: Maybe Text
  , output :: Maybe Value
  , output_text :: Maybe Text
  , usage :: Maybe Value
  } deriving (Show, Eq, Generic)

instance FromJSON ResponsesResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = dropTrailingUnderscore }

instance ToJSON ResponsesResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = dropTrailingUnderscore, omitNothingFields = True }

data ErrorDetail = ErrorDetail
  { message :: Text
  , type_   :: Maybe Text
  , param   :: Maybe Value
  , code    :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON ErrorDetail where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = dropTrailingUnderscore }

instance ToJSON ErrorDetail where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = dropTrailingUnderscore, omitNothingFields = True }

newtype ErrorResponse = ErrorResponse
  { error :: ErrorDetail
  } deriving (Show, Eq, Generic)

instance FromJSON ErrorResponse where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ErrorResponse where
  toJSON = genericToJSON defaultOptions

data ClientError = ClientError
  { status       :: Int
  , statusText   :: Text
  , requestId    :: Maybe Text
  , errorResponse :: Maybe ErrorResponse
  , rawBody      :: Text
  } deriving (Show, Eq, Generic)
