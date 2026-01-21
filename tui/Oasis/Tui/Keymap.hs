module Oasis.Tui.Keymap
  ( keyProvider
  , keyModel
  , keyRunner
  , keyMain
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
keyProvider = "p"

keyModel :: Text
keyModel = "m"

keyRunner :: Text
keyRunner = "r"

keyMain :: Text
keyMain = "v"

tipsFor :: AppState -> Text
tipsFor st =
  case paneKind (activeList st) of
    ListPane -> "[↑/↓] Move  [Enter] Select"
    OutputPane -> "[↓/↑/→/←] Scroll  [Ctrl+V/Alt+V/Alt+,/Alt+.] Page"
    InputPane -> "[Enter] Submit  [Esc] Cancel"

paneKind :: Name -> PaneKind
paneKind = \case
  ProviderList -> ListPane
  ModelList -> ListPane
  RunnerList -> ListPane
  MainViewport -> OutputPane
  PromptEditor -> InputPane
