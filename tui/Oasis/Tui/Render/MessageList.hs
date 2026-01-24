module Oasis.Tui.Render.MessageList
  ( renderMessageList
  ) where

import Relude
import Brick.Types (Widget(..), getContext, availWidthL, Size(..))
import Brick.Widgets.Core (txt)
import qualified Brick.Widgets.List as L
import Oasis.Types (Message(..), messageContentText)
import Lens.Micro ((^.))
import Oasis.Tui.Util.Text (truncateTextEllipsis)

renderMessageList :: (Ord n, Show n) => Bool -> L.List n Message -> Widget n
renderMessageList isFocused lst =
  Widget Greedy Fixed $ do
    ctx <- getContext
    let maxWidth = max 1 (ctx ^. availWidthL)
    render (L.renderListWithIndex (flip (drawMessageRow maxWidth)) isFocused lst)

messageListLine :: Int -> Message -> Text
messageListLine idx msg =
  let idxText = show (idx + 1)
      roleText = role msg
      preview = messageContentText (content msg)
  in idxText <> ". " <> roleText <> ": " <> preview

drawMessageRow :: Int -> Bool -> Int -> Message -> Widget n
drawMessageRow maxWidth _ idx msg =
  txt (truncateTextEllipsis maxWidth (messageListLine idx msg))
