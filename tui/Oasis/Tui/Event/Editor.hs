module Oasis.Tui.Event.Editor
  ( editorText
  , editorTextRaw
  , isPromptInputStart
  , handleSimpleEditorEvent
  ) where

import Relude
import Brick.Types (BrickEvent(..), EventM, nestEventM)
import Brick.Widgets.Edit (Editor, getEditContents, handleEditorEvent)
import Control.Monad.State.Class (modify)
import qualified Data.Text as T
import qualified Graphics.Vty as Vty
import Oasis.Tui.State (AppState, Name)

editorText :: Editor Text n -> Text
editorText = T.strip . mconcat . getEditContents

editorTextRaw :: Editor Text n -> Text
editorTextRaw = T.intercalate "\n" . getEditContents

isPromptInputStart :: Vty.Event -> Bool
isPromptInputStart = \case
  Vty.EvKey (Vty.KChar _) _ -> True
  Vty.EvKey Vty.KBS _ -> True
  Vty.EvKey Vty.KDel _ -> True
  _ -> False

handleSimpleEditorEvent
  :: Vty.Event
  -> Editor Text Name
  -> (Editor Text Name -> AppState -> AppState)
  -> EventM Name AppState ()
  -> EventM Name AppState ()
  -> EventM Name AppState ()
  -> EventM Name AppState ()
  -> EventM Name AppState ()
handleSimpleEditorEvent ev currentEditor setEditor onCopy onRestore onSubmit onCancel =
  case ev of
    Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl] -> onCopy
    Vty.EvKey (Vty.KChar 'r') [Vty.MCtrl] -> onRestore
    Vty.EvKey Vty.KEnter [] -> onSubmit
    Vty.EvKey Vty.KEsc [] -> onCancel
    _ -> do
      (editor', _) <- nestEventM currentEditor (handleEditorEvent (VtyEvent ev))
      modify (setEditor editor')
