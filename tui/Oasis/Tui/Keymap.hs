module Oasis.Tui.Keymap
  ( keyProvider
  , keyModel
  , keyRunner
  , keyMain
  , keyHistory
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

keyHistory :: Text
keyHistory = "l"

tipsFor :: AppState -> Text
tipsFor st =
  if verboseRoleDialogOpen st
    then "[↑/↓] Move  [Enter] Select  [Esc] Cancel"
    else if isJust (verboseDeleteConfirm st)
      then "[y] Confirm  [n] Cancel"
      else if paramDialogOpen st
    then "[Tab/Shift+Tab] Next/Prev  [Space] Toggle  [Enter] Save  [Esc] Cancel"
    else case paneKind (activeList st) of
      ListPane | activeList st == VerboseMessageList -> "[↑/↓] Move  [a] Append  [i] Insert  [e] Edit  [Del/BS] Delete"
      ListPane -> "[↑/↓] Move  [Enter] Select"
      OutputPane -> "[↓/↑/→/←] Scroll  [Ctrl+V/Alt+V/Alt+,/Alt+.] Page  [x] Lab"
      InputPane | activeList st == VerboseContentEditor -> "[Enter] Newline  [Ctrl+S] Save  [Esc] Cancel"
      InputPane -> "[Enter] Newline  [Ctrl+S] Submit  [Esc] Cancel"

paneKind :: Name -> PaneKind
paneKind = \case
  ProviderList -> ListPane
  ModelList -> ListPane
  RunnerList -> ListPane
  VerboseMessageList -> ListPane
  VerboseRoleList -> ListPane
  MainViewport -> OutputPane
  ChatViewport -> OutputPane
  PromptEditor -> InputPane
  ChatInputEditor -> InputPane
  VerboseContentEditor -> InputPane
  DebugRequestEditor -> InputPane
  ParamBetaUrlEditor -> InputPane
  ParamTemperatureEditor -> InputPane
  ParamTopPEditor -> InputPane
  ParamMaxCompletionTokensEditor -> InputPane
  ParamStopEditor -> InputPane
