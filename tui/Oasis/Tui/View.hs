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
import qualified Brick.Widgets.Edit as E
import qualified Data.Text as T
import Oasis.Tui.Keymap (keyMain, keyModel, keyProvider, keyRunner, tipsFor)
import Oasis.Tui.Render.Markdown (renderMarkdown)
import Oasis.Tui.State (AppState(..), Name(..), ParamField(..))
import Oasis.Tui.RunnerRegistry (runnerRequiresPrompt)
import Oasis.Types (Message(..), messageContentText)

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
  in if paramDialogOpen st
       then [paramDialog st, baseUi]
       else if promptDialogOpen st
         then [promptDialog st, baseUi]
         else if debugDialogOpen st
           then [debugDialog st, baseUi]
           else [baseUi]
  where
    leftPane =
      hLimit 25 $
        vBox
          [ vLimitPercent 30 $
              focusBorder (activeList st == ProviderList) $
                borderWithLabel (txt ("providers [" <> keyProvider <> "]")) $
                  withAttr (attrName "paneContent") $
                    padAll 1 $
                      L.renderList drawProvider (activeList st == ProviderList) (providerList st)
          , vLimitPercent 30 $
              focusBorder (activeList st == ModelList) $
                borderWithLabel (txt ("models [" <> keyModel <> "]")) $
                  withAttr (attrName "paneContent") $
                    padAll 1 $
                      L.renderList drawProvider (activeList st == ModelList) (modelList st)
          , vLimitPercent 100 $
              focusBorder (activeList st == RunnerList) $
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
                ( [ txt "Advanced: [h] Message History  [d] Debug Mode"
                  , txt ("Provider: " <> fromMaybe "-" (selectedProvider st))
                  , txt ("Model: " <> fromMaybe "-" (selectedModel st))
                  ]
                  <> runnerPromptLines st
                  <> [ padTop (Pad 1) hBorder
                     , viewport MainViewport Both (renderMarkdown MainViewport (outputText st))
                     ]
                )
    rightPane =
      if verboseEnabled st
        then hLimit 25 $
          borderWithLabel (txt "history") $
            withAttr (attrName "paneContent") $
              padAll 1 $
                vBox
                  [ txt "Message History"
                  , padTop (Pad 1) $
                      L.renderList drawMessageRow (activeList st == VerboseMessageList) (verboseMessageList st)
                  ]
        else hLimit 1 emptyWidget
    statusBar =
      vLimit 3 $
        hBox
          [ hLimitPercent 40 $
              borderWithLabel (txt "status") $
                withAttr (attrName "paneContent") $
                  padLeftRight 1 (txt (statusText st))
          , hLimitPercent 25 $
              borderWithLabel (txt "modes") $
                withAttr (attrName "paneContent") $
                  padLeftRight 1 (txt (modeIndicators st))
          , hLimitPercent 100 $
              borderWithLabel (txt "info") $
                withAttr (attrName "paneContent") $
                  padLeftRight 1 (txt (tipsFor st))
          ]

    promptDialog st' =
      let runnerLabel = fromMaybe "runner" (selectedRunner st')
          dialogTitle = runnerLabel <> " prompt"
      in centerLayer $
        hLimit 80 $
          vLimit 12 $
            withAttr (attrName "promptDialog") $
              overrideAttr borderAttr (attrName "promptDialogBorder") $
                borderWithLabel (txt dialogTitle) $
                  padAll 1 $
                    vBox
                      [ renderEditor (txt . unlines) True (promptEditor st')
                      ]

    paramDialog st' =
      centerLayer $
        hLimit 90 $
          vLimit 18 $
            withAttr (attrName "promptDialog") $
              overrideAttr borderAttr (attrName "promptDialogBorder") $
                borderWithLabel (txt "chat params") $
                  padAll 1 $
                    vBox
                      ( [ renderParamCheckbox "beta_url" ParamBetaUrl (paramDialogBetaValue st')
                        , renderParamLine "temperature" ParamTemperature (paramTemperatureEditor st')
                        , renderParamLine "top_p" ParamTopP (paramTopPEditor st')
                        , renderParamLine "max_completion_tokens" ParamMaxCompletionTokens (paramMaxCompletionTokensEditor st')
                        , renderParamLine "stop *" ParamStop (paramStopEditor st')
                        , padTop (Pad 1) $
                          txt "* stop input format: \"a\", \"b\""
                        ]
                        <> paramErrorLine st'
                      )

    debugDialog st' =
      centerLayer $
        hLimit 90 $
          vLimit 20 $
            withAttr (attrName "promptDialog") $
              overrideAttr borderAttr (attrName "promptDialogBorder") $
                borderWithLabel (txt "debug: request preview") $
                  padAll 1 $
                    vBox
                      [ renderEditor (txt . unlines) True (debugRequestEditor st')
                      , padTop (Pad 1) $
                          txt "[Enter] Send  [Esc] Cancel  [Ctrl+R] Restore"
                      ]

    renderParamLine label field editorWidget =
      let isFocused = paramDialogFocus st == field
          labelText = txt (label <> ":")
          labelWidget = hLimit 24 (if isFocused then withAttr (attrName "paramLabelFocus") labelText else labelText)
      in hBox
          [ labelWidget
            , padLeft (Pad 1) $
              renderEditor (txt . unlines) isFocused editorWidget
          ]

    renderParamCheckbox label field isChecked =
      let isFocused = paramDialogFocus st == field
          labelText = txt (label <> ":")
          labelWidget = hLimit 24 (if isFocused then withAttr (attrName "paramLabelFocus") labelText else labelText)
          box = if isChecked then "[x]" else "[ ]"
          boxWidget = if isFocused
            then withAttr E.editFocusedAttr (txt box)
            else txt box
      in hBox
          [ labelWidget
          , padLeft (Pad 1) boxWidget
          ]

    paramErrorLine st' =
      case paramDialogError st' of
        Nothing -> []
        Just err -> [ padTop (Pad 1) $ txt ("Error: " <> err) ]


promptSummary :: AppState -> Text
promptSummary st =
  let raw = lastPrompt st
      cleaned = if T.null (T.strip raw) then "-" else raw
  in truncateText 80 cleaned

runnerPromptLines :: AppState -> [Widget Name]
runnerPromptLines st =
  if runnerStarted st
    then
      let runnerName = fromMaybe "-" (selectedRunner st)
          promptLines =
            [txt ("Prompt: " <> promptSummary st) | runnerRequiresPrompt (selectedRunner st)]
      in [txt ("Runner: " <> runnerName)] <> promptLines
    else []

truncateText :: Int -> Text -> Text
truncateText maxLen t =
  if T.length t > maxLen
    then T.take (maxLen - 1) t <> "…"
    else t

modeIndicators :: AppState -> Text
modeIndicators st =
  if debugEnabled st
    then "debug: on"
    else "debug: off"

drawProvider :: Bool -> Text -> Widget Name
drawProvider _ = txt

drawMessageRow :: Bool -> Message -> Widget Name
drawMessageRow _ msg =
  let roleTag = case role msg of
        "system" -> "[system] "
        "user" -> "[user] "
        "assistant" -> "[assistant] "
        "tool" -> "[tool] "
        _ -> "[role] "
      preview = truncateText 40 (messageContentText (content msg))
  in txt (roleTag <> preview)

focusBorder :: Bool -> Widget Name -> Widget Name
focusBorder isActive =
  if isActive
    then withAttr (attrName "focusBorder") . overrideAttr borderAttr (attrName "focusBorder")
    else id
