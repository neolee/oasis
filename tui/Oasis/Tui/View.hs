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
import Oasis.Tui.Keymap (keyMain, keyModel, keyProvider, keyRunner, keyHistory, tipsFor)
import Oasis.Tui.Render.Markdown (renderMarkdown)
import Oasis.Tui.Render.MessageList (renderMessageList)
import Oasis.Tui.State (AppState(..), Name(..), ParamField(..), DebugRequestInfo(..))
import Oasis.Tui.Registry (runnerRequiresPrompt)
import Oasis.Tui.X (renderTestPane)
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
  in if testPaneOpen st
       then [renderTestPane st, baseUi]
       else if verboseRoleDialogOpen st
         then [roleDialog st, baseUi]
         else if paramDialogOpen st
         then [paramDialog st, baseUi]
         else if promptDialogOpen st
           then [promptDialog st, baseUi]
           else if debugDialogOpen st
             then [debugDialog st, baseUi]
             else [baseUi]
  where
    leftPane =
      hLimitPercent 20 $
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
      if selectedRunner st == Just "chat"
        then chatPane
        else standardPane
    rightPane =
      if verboseEnabled st
        then
          let isHistoryFocused = activeList st == VerboseMessageList
              isDetailFocused = activeList st == VerboseContentEditor
          in hLimitPercent 25 $
              case L.listSelectedElement (verboseMessageList st) of
                Nothing ->
                  vBox
                    [ vLimitPercent 100 $
                        focusBorder isHistoryFocused $
                          borderWithLabel (txt ("history [" <> keyHistory <> "]")) $
                            withAttr (attrName "paneContent") $
                              padAll 1 $
                                renderMessageList isHistoryFocused (verboseMessageList st)
                    ]
                Just (_, msg) ->
                  let detailRoleLabel = "role: " <> role msg
                  in vBox
                      [ vLimitPercent 50 $
                          focusBorder isHistoryFocused $
                            borderWithLabel (txt ("history [" <> keyHistory <> "]")) $
                              withAttr (attrName "paneContent") $
                                padAll 1 $
                                  renderMessageList isHistoryFocused (verboseMessageList st)
                      , vLimitPercent 100 $
                          focusBorder isDetailFocused $
                            borderWithLabel (txt detailRoleLabel) $
                              withAttr (attrName "paneContent") $
                                padAll 1 $
                                  renderVerboseDetail st
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

    standardPane =
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

    chatPane =
      let isChatFocused = activeList st == ChatViewport || activeList st == ChatInputEditor
      in focusBorder isChatFocused $
        borderWithLabel (txt ("chat [" <> keyMain <> "]")) $
          withAttr (attrName "paneContent") $
            padAll 1 $
              vBox
                [ txt "Advanced: [h] Message History  [d] Debug Mode"
                , txt ("Provider: " <> fromMaybe "-" (selectedProvider st))
                , txt ("Model: " <> fromMaybe "-" (selectedModel st))
                , padTop (Pad 1) hBorder
                , viewport ChatViewport Vertical (renderChatHistory (chatMessages st))
                , padTop (Pad 1) hBorder
                , vLimit 3 $ renderEditor (txt . unlines) (activeList st == ChatInputEditor) (chatInputEditor st)
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
      let infoLines =
            case debugRequestInfo st' of
              Nothing -> []
              Just info ->
                [ txt ("provider: " <> debugProviderName info)
                , txt ("model: " <> debugModelName info)
                , txt ("endpoint: " <> debugEndpoint info)
                , txt "headers:"
                ] <> map txt (debugHeaders info)
          errorLines =
            case debugRequestError st' of
              Nothing -> []
              Just err -> [ padTop (Pad 1) $ txt ("Error: " <> err) ]
      in centerLayer $
        hLimit 110 $
          vLimit 28 $
            withAttr (attrName "promptDialog") $
              overrideAttr borderAttr (attrName "promptDialogBorder") $
                borderWithLabel (txt "debug: request preview") $
                  padAll 1 $
                    vBox
                      ( infoLines
                      <> [ padTop (Pad 1) $ renderEditor (txt . unlines) True (debugRequestEditor st')
                         , padTop (Pad 1) $
                             txt "[Enter] Send  [Esc] Cancel  [Ctrl+R] Restore"
                         ]
                      <> errorLines
                      )

    roleDialog st' =
      centerLayer $
        hLimit 40 $
          vLimit 12 $
            withAttr (attrName "promptDialog") $
              overrideAttr borderAttr (attrName "promptDialogBorder") $
                borderWithLabel (txt "select role") $
                  padAll 1 $
                    L.renderList drawProvider True (verboseRoleList st')

    renderVerboseDetail st' =
      case L.listSelectedElement (verboseMessageList st') of
        Nothing -> txt " "
        Just (_, msg) ->
          if activeList st' == VerboseContentEditor
            then renderEditor (txt . unlines) True (verboseContentEditor st')
            else txtWrap (messageContentText (content msg))

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

renderChatHistory :: [Message] -> Widget Name
renderChatHistory msgs =
  vBox (map renderChatMessage msgs)

renderChatMessage :: Message -> Widget Name
renderChatMessage msg =
  let roleLabel = case role msg of
        "user" -> "USER"
        "assistant" -> "ASSISTANT"
        "system" -> "SYSTEM"
        "tool" -> "TOOL"
        _ -> "ROLE"
      header = roleLabel <> ": " <> messageContentText (content msg)
  in padBottom (Pad 1) $ renderMarkdown ChatViewport header

focusBorder :: Bool -> Widget Name -> Widget Name
focusBorder isActive =
  if isActive
    then withAttr (attrName "focusBorder") . overrideAttr borderAttr (attrName "focusBorder")
    else id
