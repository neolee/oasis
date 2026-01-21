module Oasis.Tui.State
  ( Name(..)
  , TuiEvent(..)
  , AppState(..)
  , mkState
  ) where

import Relude
import Brick.BChan (BChan)
import qualified Brick.Widgets.List as L
import qualified Data.Vector as V
import Oasis.Types (Config)

data TuiEvent
  = BasicCompleted
      { eventStatus :: Text
      , eventOutput :: Text
      }


data Name
  = ProviderList
  | ModelList
  | RunnerList
  | MainViewport
  deriving (Eq, Ord, Show)

data AppState = AppState
  { config :: Config
  , eventChan :: BChan TuiEvent
  , providerList :: L.List Name Text
  , modelList :: L.List Name Text
  , runnerList :: L.List Name Text
  , activeList :: Name
  , selectedProvider :: Maybe Text
  , selectedModel :: Maybe Text
  , selectedRunner :: Maybe Text
  , outputText :: Text
  , statusText :: Text
  }

mkState :: BChan TuiEvent -> Config -> [Text] -> [Text] -> [Text] -> Text -> Text -> AppState
mkState chan cfg providers models runners outputText statusText =
  AppState
    { config = cfg
    , eventChan = chan
    , providerList = L.list ProviderList (V.fromList providers) 1
    , modelList = L.list ModelList (V.fromList models) 1
    , runnerList = L.list RunnerList (V.fromList runners) 1
    , activeList = ProviderList
    , selectedProvider = Nothing
    , selectedModel = Nothing
    , selectedRunner = Nothing
    , outputText
    , statusText
    }
