module Oasis.Tui.State
  ( Name(..)
  , ParamField(..)
  , TuiEvent(..)
  , AppState(..)
  , DebugRequestInfo(..)
  , DebugRequestHandler
  , mkState
  , defaultOutputText
  ) where

import Relude
import Brick.BChan (BChan)
import Brick.Widgets.Edit (Editor, editor)
import qualified Brick.Widgets.List as L
import qualified Data.Vector as V
import Oasis.Client.OpenAI.Param (ChatParams, emptyChatParams)
import Oasis.Types (Config, Message)

data TuiEvent
  = BasicCompleted
      { eventStatus :: Text
      , eventOutput :: Text
      }
  | DebugRequestOpen
      { eventDebugInfo :: DebugRequestInfo
      , eventDebugOriginal :: Text
      , eventDebugHandler :: DebugRequestHandler
      , eventDebugReturnFocus :: Name
      }
  | ChatStreaming
    { eventDelta :: Text
    }
  | ChatCompleted
    { eventStatus :: Text
    }
  | MessageListSynced
    { eventMessages :: [Message]
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
  | PartialModeCompleted
    { eventStatus :: Text
    , eventOutput :: Text
    }
  | PrefixCompletionCompleted
    { eventStatus :: Text
    , eventOutput :: Text
    }
  | FimCompletionCompleted
    { eventStatus :: Text
    , eventOutput :: Text
    }


data Name
  = ProviderList
  | ModelList
  | RunnerList
  | MainViewport
  | PromptEditor
  | ModelInputEditor
  | ChatViewport
  | ChatInputEditor
  | VerboseMessageList
  | VerboseRoleList
  | VerboseContentEditor
  | DebugRequestEditor
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
  , customModels :: [Text]
  , runnerList :: L.List Name Text
  , verboseMessageList :: L.List Name Message
  , activeList :: Name
  , selectedProvider :: Maybe Text
  , selectedModel :: Maybe Text
  , selectedRunner :: Maybe Text
  , runnerStarted :: Bool
  , promptEditor :: Editor Text Name
  , chatInputEditor :: Editor Text Name
  , chatMessages :: [Message]
  , verboseEnabled :: Bool
  , debugEnabled :: Bool
  , debugDialogOpen :: Bool
  , verboseContentEditor :: Editor Text Name
  , verboseRoleDialogOpen :: Bool
  , verboseRoleList :: L.List Name Text
  , verbosePendingInsertIndex :: Maybe Int
  , verboseEditIndex :: Maybe Int
  , verboseDeleteConfirm :: Maybe Int
  , debugRequestEditor :: Editor Text Name
  , debugRequestOriginal :: Text
  , debugRequestDraft :: Text
  , debugRequestError :: Maybe Text
  , debugRequestInfo :: Maybe DebugRequestInfo
  , debugPendingAction :: Maybe DebugRequestHandler
  , debugDialogReturnFocus :: Name
  , testPaneOpen :: Bool
  , promptDialogOpen :: Bool
  , modelInputDialogOpen :: Bool
  , modelInputDefault :: Text
  , modelInputEditor :: Editor Text Name
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

data DebugRequestInfo = DebugRequestInfo
  { debugProviderName :: Text
  , debugModelName :: Text
  , debugEndpoint :: Text
  , debugHeaders :: [Text]
  } deriving (Show, Eq)

type DebugRequestHandler = Text -> Either Text (IO TuiEvent)

mkState :: BChan TuiEvent -> Config -> [Text] -> [Text] -> [Text] -> Text -> Text -> AppState
mkState chan cfg providers models runners outputText statusText =
  AppState
    { config = cfg
    , eventChan = chan
    , providerList = L.list ProviderList (V.fromList providers) 1
    , modelList = L.list ModelList (V.fromList models) 1
    , customModels = []
    , runnerList = L.list RunnerList (V.fromList runners) 1
    , verboseMessageList = L.list VerboseMessageList V.empty 1
    , activeList = ProviderList
    , selectedProvider = Nothing
    , selectedModel = Nothing
    , selectedRunner = Nothing
    , runnerStarted = False
    , promptEditor = editor PromptEditor (Just 5) defaultPrompt
    , chatInputEditor = editor ChatInputEditor (Just 3) ""
    , chatMessages = []
    , verboseEnabled = False
    , debugEnabled = False
    , debugDialogOpen = False
    , verboseContentEditor = editor VerboseContentEditor (Just 8) ""
    , verboseRoleDialogOpen = False
    , verboseRoleList = L.list VerboseRoleList (V.fromList ["system", "user", "assistant", "tool"]) 1
    , verbosePendingInsertIndex = Nothing
    , verboseEditIndex = Nothing
    , verboseDeleteConfirm = Nothing
    , debugRequestEditor = editor DebugRequestEditor (Just 10) ""
    , debugRequestOriginal = ""
    , debugRequestDraft = ""
    , debugRequestError = Nothing
    , debugRequestInfo = Nothing
    , debugPendingAction = Nothing
    , debugDialogReturnFocus = MainViewport
    , testPaneOpen = False
    , promptDialogOpen = False
    , modelInputDialogOpen = False
    , modelInputDefault = ""
    , modelInputEditor = editor ModelInputEditor (Just 1) ""
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
