module Oasis.Tui.State
  ( Name(..)
  , TuiEvent(..)
  , AppState(..)
  , mkState
  ) where

import Relude
import Brick.BChan (BChan)
import Brick.Widgets.Edit (Editor, editor)
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
  | PromptEditor
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
  , runnerStarted :: Bool
  , promptEditor :: Editor Text Name
  , promptDialogOpen :: Bool
  , promptDefault :: Text
  , promptPristine :: Bool
  , lastPrompt :: Text
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
    , runnerStarted = False
    , promptEditor = editor PromptEditor (Just 5) defaultPrompt
    , promptDialogOpen = False
    , promptDefault = defaultPrompt
    , promptPristine = False
    , lastPrompt = defaultPrompt
    , outputText
    , statusText
    }

defaultPrompt :: Text
defaultPrompt = "Hello from oasis-tui basic runner."
