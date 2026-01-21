module Oasis.Tui.View
  ( drawUI
  ) where

import Relude
import Brick.AttrMap (attrName)
import Brick.Types (Widget, ViewportType(..))
import Brick.Widgets.Border
import Brick.Widgets.Center (centerLayer)
import Brick.Widgets.Core
import qualified Brick.Widgets.List as L
import Brick.Widgets.Edit (renderEditor)
import qualified Data.Text as T
import Oasis.Tui.Keymap (keyMain, keyModel, keyProvider, keyRunner, tipsFor)
import Oasis.Tui.Render.Markdown (renderMarkdown)
import Oasis.Tui.State (AppState(..), Name(..))

drawUI :: AppState -> [Widget Name]
drawUI st =
  let baseUi =
        vBox
          [ hBox
              [ leftPane
              , centerPane
              , rightPane
              ]
          , statusBar
          ]
  in if promptDialogOpen st
       then [promptDialog st, baseUi]
       else [baseUi]
  where
    leftPane =
      hLimit 25 $
        vBox
          [ focusBorder (activeList st == ProviderList) $
              borderWithLabel (txt ("providers [" <> keyProvider <> "]")) $
                withAttr (attrName "paneContent") $
                  padAll 1 $
                    L.renderList drawProvider (activeList st == ProviderList) (providerList st)
          , focusBorder (activeList st == ModelList) $
              borderWithLabel (txt ("models [" <> keyModel <> "]")) $
                withAttr (attrName "paneContent") $
                  padAll 1 $
                    L.renderList drawProvider (activeList st == ModelList) (modelList st)
          , focusBorder (activeList st == RunnerList) $
              borderWithLabel (txt ("runners [" <> keyRunner <> "]")) $
                withAttr (attrName "paneContent") $
                  padAll 1 $
                    L.renderList drawProvider (activeList st == RunnerList) (runnerList st)
          ]
    centerPane =
      focusBorder (activeList st == MainViewport) $
        borderWithLabel (txt ("main [" <> keyMain <> "]")) $
          withAttr (attrName "paneContent") $
            padAll 1 $
              vBox
                [ txt ("Provider: " <> fromMaybe "-" (selectedProvider st))
                , txt ("Model: " <> fromMaybe "-" (selectedModel st))
                , txt ("Runner: " <> fromMaybe "-" (selectedRunner st))
                , txt ("Prompt: " <> promptSummary st)
                , padTop (Pad 1) hBorder
                , viewport MainViewport Both (renderMarkdown (outputText st))
                ]
    rightPane =
      hLimit 25 $
        borderWithLabel (txt "sidebar") $
          withAttr (attrName "paneContent") $
            padAll 1 $
              txt ""
    statusBar =
      vLimit 3 $
        hBox
          [ hLimitPercent 50 $
              borderWithLabel (txt "status") $
                withAttr (attrName "paneContent") $
                  padLeftRight 1 (txt (statusText st))
          , hLimitPercent 100 $
              borderWithLabel (txt "info") $
                withAttr (attrName "paneContent") $
                  padLeftRight 1 (txt (tipsFor st))
          ]

    promptDialog st' =
      centerLayer $
        hLimit 80 $
          vLimit 12 $
            withAttr (attrName "promptDialog") $
              overrideAttr borderAttr (attrName "promptDialogBorder") $
                borderWithLabel (txt "basic prompt") $
                  padAll 1 $
                    vBox
                      [ withAttr (attrName "promptEditor") $
                        renderEditor (txt . unlines) True (promptEditor st')
                      , padTop (Pad 1) $
                        txt "[Enter] Run  [Esc] Cancel"
                      ]

promptSummary :: AppState -> Text
promptSummary st =
  let raw = lastPrompt st
      cleaned = if T.null (T.strip raw) then "-" else raw
  in truncateText 80 cleaned

truncateText :: Int -> Text -> Text
truncateText maxLen t =
  if T.length t > maxLen
    then T.take (maxLen - 1) t <> "…"
    else t

drawProvider :: Bool -> Text -> Widget Name
drawProvider _ = txt

focusBorder :: Bool -> Widget Name -> Widget Name
focusBorder isActive =
  if isActive
    then withAttr (attrName "focusBorder") . overrideAttr borderAttr (attrName "focusBorder")
    else id
