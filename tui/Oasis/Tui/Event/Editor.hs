module Oasis.Tui.Event.Editor
  ( editorText
  , editorTextRaw
  , isPromptInputStart
  ) where

import Relude
import Brick.Widgets.Edit (Editor, getEditContents)
import qualified Data.Text as T
import qualified Graphics.Vty as Vty

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
