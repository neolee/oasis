module Oasis.Tui.State
  ( Name(..)
  , ParamField(..)
  , TuiEvent(..)
  , AppState(..)
  , mkState
  , defaultOutputText
  ) where

import Relude
import Brick.BChan (BChan)
import Brick.Widgets.Edit (Editor, editor)
import qualified Brick.Widgets.List as L
import qualified Data.Vector as V
import Oasis.Client.OpenAI.Param (ChatParams, emptyChatParams)
import Oasis.Types (Config)

data TuiEvent
  = BasicCompleted
      { eventStatus :: Text
      , eventOutput :: Text
      }
  | ResponsesCompleted
      { eventStatus :: Text
      , eventOutput :: Text
      }
  | ModelsCompleted
    { eventStatus :: Text
    , eventOutput :: Text
    }
  | EmbeddingsCompleted
    { eventStatus :: Text
    , eventOutput :: Text
    }
  | HooksCompleted
    { eventStatus :: Text
    , eventOutput :: Text
    }
  | StructuredCompleted
    { eventStatus :: Text
    , eventOutput :: Text
    }
  | StructuredStreaming
    { eventOutput :: Text
    }
  | ToolCallingCompleted
    { eventStatus :: Text
    , eventOutput :: Text
    }


data Name
  = ProviderList
  | ModelList
  | RunnerList
  | MainViewport
  | PromptEditor
  | ParamBetaUrlEditor
  | ParamTemperatureEditor
  | ParamTopPEditor
  | ParamMaxCompletionTokensEditor
  | ParamStopEditor
  deriving (Eq, Ord, Show)

data ParamField
  = ParamBetaUrl
  | ParamTemperature
  | ParamTopP
  | ParamMaxCompletionTokens
  | ParamStop
  deriving (Eq, Ord, Show, Enum, Bounded)

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
  , chatParams :: ChatParams
  , betaUrlSetting :: Bool
  , paramDialogOpen :: Bool
  , paramDialogFocus :: ParamField
  , paramDialogError :: Maybe Text
  , paramDialogReturnFocus :: Name
  , paramDialogBetaValue :: Bool
  , paramTemperatureEditor :: Editor Text Name
  , paramTopPEditor :: Editor Text Name
  , paramMaxCompletionTokensEditor :: Editor Text Name
  , paramStopEditor :: Editor Text Name
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
    , chatParams = emptyChatParams
    , betaUrlSetting = False
    , paramDialogOpen = False
    , paramDialogFocus = ParamBetaUrl
    , paramDialogError = Nothing
    , paramDialogReturnFocus = ProviderList
    , paramDialogBetaValue = False
    , paramTemperatureEditor = editor ParamTemperatureEditor (Just 1) ""
    , paramTopPEditor = editor ParamTopPEditor (Just 1) ""
    , paramMaxCompletionTokensEditor = editor ParamMaxCompletionTokensEditor (Just 1) ""
    , paramStopEditor = editor ParamStopEditor (Just 1) ""
    , outputText
    , statusText
    }

defaultPrompt :: Text
defaultPrompt = "I'm Neo and how are you?"

defaultOutputText :: Text
defaultOutputText = "Select provider, model and runner, the output will appear here."
