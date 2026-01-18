{-# LANGUAGE StrictData #-}

module Oasis.Client.OpenAI.Types
  ( ChatCompletionRequest(..)
  , ChatCompletionResponse(..)
  , ChatChoice(..)
  , Usage(..)
  , ChatCompletionStreamChunk(..)
  , StreamChoice(..)
  , StreamDelta(..)
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
