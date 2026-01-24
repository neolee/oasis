module Oasis.Tui.Util.Text
  ( truncateTextEllipsis
  , truncateTextDots
  ) where

import Relude
import qualified Data.Text as T

truncateTextEllipsis :: Int -> Text -> Text
truncateTextEllipsis maxLen t
  | maxLen <= 0 = ""
  | T.length t > maxLen = T.take (maxLen - 1) t <> "…"
  | otherwise = t

truncateTextDots :: Int -> Text -> Text
truncateTextDots maxLen t
  | T.length t <= maxLen = t
  | otherwise = T.take maxLen t <> "..."
