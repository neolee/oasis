module Oasis.Tui.X
  ( renderTestPane
  ) where

import Relude
import Brick.Types (Widget)
import Brick.Widgets.Border
import Brick.Widgets.Center (centerLayer)
import Brick.Widgets.Core
import qualified Data.Text as T
import qualified Graphics.Text.Width as W
import Oasis.Tui.State (AppState, Name)

renderTestPane :: AppState -> Widget Name
renderTestPane _ =
  centerLayer $
    hLimit 90 $
      vLimit 22 $
        borderWithLabel (txt "lab: width diagnostics") $
          padAll 1 $
            vBox
              ( header
                : instruction
                : padTop (Pad 1) (txt "Sample lines:")
                : map renderSample samples
              )
  where
    header = txt "Vty width vs terminal rendering"
    instruction = txt "[Esc/x] Close  (Use this pane to visually verify width alignment)"

samples :: [Text]
samples =
  [ "plain ascii"
  , "中文测试"
  , "emoji 🙂"
  , "emoji 😀😀😀"
  , "mixed: ok😀中🙂"
  , "zwj: \x1F468\&\x200D\&\x1F469\&\x200D\&\x1F467\&\x200D\&\x1F466"
  , "flags: 🇨🇳 🇺🇸 🇯🇵"
  ]

renderSample :: Text -> Widget Name
renderSample t =
  let w = W.safeWctwidth t
      label = "w=" <> show w <> "  " <> t
      ruler = "|" <> T.take 40 (T.replicate 4 "1234567890") <> "|"
  in vBox
      [ txt ruler
      , txt label
      ]
