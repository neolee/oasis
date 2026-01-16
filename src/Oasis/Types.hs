{-# LANGUAGE StrictData #-}

module Oasis.Types where

import Relude
import Data.Aeson (ToJSON, FromJSON)
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

data Message = Message
  { role    :: Text
  , content :: Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)
