module Oasis.Tui.View
  ( drawUI
  ) where

import Relude
import Brick.AttrMap (attrName)
import Brick.Types (Widget, ViewportType(..))
import Brick.Widgets.Border
import Brick.Widgets.Core
import qualified Brick.Widgets.List as L
import Oasis.Tui.Keymap (keyMain, keyModel, keyProvider, keyRunner, keySidebar, tipsFor)
import Oasis.Tui.State (AppState(..), Name(..))

drawUI :: AppState -> [Widget Name]
drawUI st =
  [ vBox
      [ hBox
          [ leftPane
          , centerPane
          , rightPane
          ]
      , statusBar
      ]
  ]
  where
    leftPane =
      hLimit 28 $
        vBox
          [ focusBorder (activeList st == ProviderList) $
              borderWithLabel (txt ("Providers [" <> keyProvider <> "]")) $
                padAll 1 $
                  L.renderList drawProvider (activeList st == ProviderList) (providerList st)
          , focusBorder (activeList st == ModelList) $
              borderWithLabel (txt ("Models [" <> keyModel <> "]")) $
                padAll 1 $
                  L.renderList drawProvider (activeList st == ModelList) (modelList st)
          , focusBorder (activeList st == RunnerList) $
              borderWithLabel (txt ("Runners [" <> keyRunner <> "]")) $
                padAll 1 $
                  L.renderList drawProvider (activeList st == RunnerList) (runnerList st)
          ]
    centerPane =
      focusBorder (activeList st == MainViewport) $
        borderWithLabel (txt ("Main [" <> keyMain <> "]")) $
          padAll 1 $
            vBox
              [ txt ("Provider: " <> fromMaybe "-" (selectedProvider st))
              , txt ("Model: " <> fromMaybe "-" (selectedModel st))
              , txt ("Runner: " <> fromMaybe "-" (selectedRunner st))
              , padTop (Pad 1) hBorder
              , vLimit 12 $
                  viewport MainViewport Vertical (txtWrap (outputText st))
              ]
    rightPane =
      hLimit 28 $
        borderWithLabel (txt ("Sidebar [" <> keySidebar <> "]")) $
          padAll 1 $
            txt ""
    statusBar =
      vLimit 3 $
        hBox
          [ hLimitPercent 50 $
              borderWithLabel (txt "status") $
                padLeftRight 1 (txt (statusText st))
          , hLimitPercent 100 $
              borderWithLabel (txt "info") $
                padLeftRight 1 (txt (tipsFor st))
          ]

drawProvider :: Bool -> Text -> Widget Name
drawProvider _ = txt

focusBorder :: Bool -> Widget Name -> Widget Name
focusBorder isActive =
  if isActive
    then withAttr (attrName "focusBorder")
    else id
