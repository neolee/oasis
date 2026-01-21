module Oasis.Tui.Keymap
  ( keyProvider
  , keyModel
  , keyRunner
  , keyMain
  , keySidebar
  , PaneKind(..)
  , paneKind
  , tipsFor
  ) where

import Relude
import Oasis.Tui.State (AppState(..), Name(..))

data PaneKind
  = ListPane
  | OutputPane
  | InputPane

keyProvider :: Text
keyProvider = "P"

keyModel :: Text
keyModel = "M"

keyRunner :: Text
keyRunner = "R"

keyMain :: Text
keyMain = "V"

keySidebar :: Text
keySidebar = "L/D"

tipsFor :: AppState -> Text
tipsFor st =
  case paneKind (activeList st) of
    ListPane -> "[↑/↓] Move  [Enter] Select"
    OutputPane -> "[↑/↓] Scroll  [Ctrl+V/Alt+V] Page"
    InputPane -> "[Enter] Submit  [Esc] Cancel"

paneKind :: Name -> PaneKind
paneKind = \case
  ProviderList -> ListPane
  ModelList -> ListPane
  RunnerList -> ListPane
  MainViewport -> OutputPane
