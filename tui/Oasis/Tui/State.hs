module Oasis.Tui.State
  ( Name(..)
  , AppState(..)
  , mkState
  ) where

import Relude
import qualified Brick.Widgets.List as L
import qualified Data.Vector as V
import Oasis.Types (Config)


data Name
  = ProviderList
  | ModelList
  | RunnerList
  deriving (Eq, Ord, Show)

data AppState = AppState
  { config :: Config
  , providerList :: L.List Name Text
  , modelList :: L.List Name Text
  , runnerList :: L.List Name Text
  , activeList :: Name
  , selectedProvider :: Maybe Text
  , selectedModel :: Maybe Text
  , selectedRunner :: Maybe Text
  , statusText :: Text
  }

mkState :: Config -> [Text] -> [Text] -> [Text] -> Text -> AppState
mkState cfg providers models runners statusText =
  AppState
    { config = cfg
    , providerList = L.list ProviderList (V.fromList providers) 1
    , modelList = L.list ModelList (V.fromList models) 1
    , runnerList = L.list RunnerList (V.fromList runners) 1
    , activeList = ProviderList
    , selectedProvider = Nothing
    , selectedModel = Nothing
    , selectedRunner = Nothing
    , statusText
    }
