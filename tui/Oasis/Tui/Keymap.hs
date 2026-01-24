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

commonInputTips :: Text
commonInputTips = "[enter] submit  [esc] cancel  [ctrl-c] copy  [ctrl-r] restore"

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
    then "[↑/↓] move  [enter] select  [esc] cancel"
    else if isJust (verboseDeleteConfirm st)
      then "[y] confirm  [n] cancel"
      else if paramDialogOpen st
    then "[space] toggle " <> commonInputTips
    else if modelInputDialogOpen st
    then commonInputTips
    else case paneKind (activeList st) of
      ListPane | activeList st == VerboseMessageList -> "[↑/↓] move  [a] append  [i] insert  [e] edit  [del/bs] delete"
      ListPane -> "[↑/↓] move  [enter] select"
      OutputPane -> "[↓/↑/→/←] scroll  [ctrl-v/alt-v/alt+,/alt+.] page  [x] lab"
      InputPane -> commonInputTips

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
  ModelInputEditor -> InputPane
  ChatInputEditor -> InputPane
  VerboseContentEditor -> InputPane
  DebugRequestEditor -> InputPane
  ParamBetaUrlEditor -> InputPane
  ParamTemperatureEditor -> InputPane
  ParamTopPEditor -> InputPane
  ParamMaxCompletionTokensEditor -> InputPane
  ParamStopEditor -> InputPane
