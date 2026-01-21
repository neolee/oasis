module Oasis.Tui.State
  ( Name(..)
  , AppState(..)
  , mkState
  ) where

import Relude
import qualified Brick.Widgets.List as L
import qualified Data.Vector as V


data Name
  = ProviderList
  deriving (Eq, Ord, Show)

data AppState = AppState
  { providerList :: L.List Name Text
  , statusText :: Text
  }

mkState :: [Text] -> Text -> AppState
mkState providers statusText =
  AppState
    { providerList = L.list ProviderList (V.fromList providers) 1
    , statusText
    }
