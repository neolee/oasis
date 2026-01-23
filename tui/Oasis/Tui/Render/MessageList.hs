module Oasis.Tui.Render.MessageList
  ( renderMessageList
  , messageListLine
  ) where

import Relude
import Brick.Types (Widget)
import Brick.Widgets.Core (txt)
import qualified Brick.Widgets.List as L
import qualified Data.Text as T
import Oasis.Types (Message(..), messageContentText)

renderMessageList :: (Ord n, Show n) => Int -> Bool -> L.List n Message -> Widget n
renderMessageList maxWidth =
  L.renderListWithIndex (flip (drawMessageRow maxWidth))

messageListLine :: Int -> Message -> Text
messageListLine idx msg =
  let idxText = show (idx + 1)
      roleText = role msg
      preview = messageContentText (content msg)
  in idxText <> ". " <> roleText <> ": " <> preview

drawMessageRow :: Int -> Bool -> Int -> Message -> Widget n
drawMessageRow maxWidth _ idx msg =
  txt (truncateText maxWidth (messageListLine idx msg))

truncateText :: Int -> Text -> Text
truncateText maxLen t
  | maxLen <= 0 = ""
  | T.length t > maxLen = T.take (maxLen - 1) t <> "…"
  | otherwise = t
