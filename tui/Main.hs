module Main where

import Relude
import Brick.AttrMap (attrMap, attrName)
import Brick.Main (App(..), defaultMain, halt, showFirstCursor, viewportScroll, vScrollBy)
import Brick.Types (BrickEvent(..), EventM, Widget, nestEventM, ViewportType(..))
import Brick.Widgets.Border
import Brick.Widgets.Core
import qualified Brick.Widgets.List as L
import Control.Monad.State.Class (get, modify)
import qualified Data.Map.Strict as M
import qualified Data.List as List
import qualified Data.Vector as V
import qualified Graphics.Vty as Vty
import Oasis.Config
import Oasis.Tui.State (AppState(..), Name(..), mkState)
import Oasis.Types (Config(..), Defaults(..), Provider(..))

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
            , padTop (Pad 1) (txt "Output:")
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
        borderWithLabel (txt "Status") $
          padLeftRight 1 (txt (statusText st))

drawProvider :: Bool -> Text -> Widget Name
drawProvider _ = txt


appEvent :: BrickEvent Name e -> EventM Name AppState ()
appEvent (VtyEvent ev) =
  case ev of
    Vty.EvKey (Vty.KChar 'q') [] -> halt
    Vty.EvKey (Vty.KChar 'Q') [] -> halt
    Vty.EvKey (Vty.KChar 'p') [] -> setActive ProviderList
    Vty.EvKey (Vty.KChar 'P') [] -> setActive ProviderList
    Vty.EvKey (Vty.KChar 'm') [] -> setActive ModelList
    Vty.EvKey (Vty.KChar 'M') [] -> setActive ModelList
    Vty.EvKey (Vty.KChar 'r') [] -> setActive RunnerList
    Vty.EvKey (Vty.KChar 'R') [] -> setActive RunnerList
    Vty.EvKey (Vty.KChar 'v') [] -> setActive MainViewport
    Vty.EvKey (Vty.KChar 'V') [] -> setActive MainViewport
    Vty.EvKey Vty.KEnter [] -> applySelection
    Vty.EvKey Vty.KPageUp [] -> vScrollBy (viewportScroll MainViewport) (-3)
    Vty.EvKey Vty.KPageDown [] -> vScrollBy (viewportScroll MainViewport) 3
    _ -> handleActiveListEvent ev
appEvent _ = pure ()

setActive :: Name -> EventM Name AppState ()
setActive name = modify (\s -> s { activeList = name })

handleActiveListEvent :: Vty.Event -> EventM Name AppState ()
handleActiveListEvent ev = do
  st <- get
  case activeList st of
    ProviderList -> do
      (lst, _) <- nestEventM (providerList st) (L.handleListEvent ev)
      modify (\s -> s { providerList = lst })
    ModelList -> do
      (lst, _) <- nestEventM (modelList st) (L.handleListEvent ev)
      modify (\s -> s { modelList = lst })
    RunnerList -> do
      (lst, _) <- nestEventM (runnerList st) (L.handleListEvent ev)
      modify (\s -> s { runnerList = lst })
    MainViewport -> pure ()

applySelection :: EventM Name AppState ()
applySelection = do
  st <- get
  case activeList st of
    ProviderList ->
      case L.listSelectedElement (providerList st) of
        Nothing -> pure ()
        Just (_, providerName) -> do
          let models = providerModels (config st) providerName
          let modelList' = L.list ModelList (V.fromList models) 1
          modify (\s -> s
            { selectedProvider = Just providerName
            , selectedModel = listToMaybe models
            , modelList = modelList'
            , statusText = "Selected provider: " <> providerName
            })
    ModelList ->
      case L.listSelectedElement (modelList st) of
        Nothing -> pure ()
        Just (_, modelName) ->
          modify (\s -> s
            { selectedModel = Just modelName
            , statusText = "Selected model: " <> modelName
            })
    RunnerList ->
      case L.listSelectedElement (runnerList st) of
        Nothing -> pure ()
        Just (_, runnerName) ->
          modify (\s -> s
            { selectedRunner = Just runnerName
            , statusText = "Selected runner: " <> runnerName
            })

providerModels :: Config -> Text -> [Text]
providerModels cfg providerName =
  case M.lookup providerName (providers cfg) of
    Nothing -> []
    Just Provider{chat_model_id, coder_model_id, reasoner_model_id} ->
      List.nub [chat_model_id, coder_model_id, reasoner_model_id]

app :: App AppState e Name
app =
  App
    { appDraw = drawUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent = appEvent
    , appStartEvent = pure ()
    , appAttrMap = const (attrMap Vty.defAttr
      [ (L.listAttr, Vty.defAttr)
      , (L.listSelectedAttr, Vty.defAttr `Vty.withStyle` Vty.reverseVideo)
      , (attrName "focusBorder", Vty.defAttr `Vty.withForeColor` Vty.cyan)
      ])
    }

main :: IO ()
main = do
  mPath <- findConfig
  case mPath of
    Nothing -> do
      let emptyCfg = Config mempty (Defaults "" "") mempty
      let st = mkState emptyCfg [] [] defaultRunners "Output will appear here." "providers.toml not found"
      void $ defaultMain app st
    Just path -> do
      cfgResult <- loadConfig path
      case cfgResult of
        Left err -> do
          let emptyCfg = Config mempty (Defaults "" "") mempty
          let st = mkState emptyCfg [] [] defaultRunners "Output will appear here." ("Failed to load config: " <> err)
          void $ defaultMain app st
        Right cfg -> do
          let providerNames = sort (M.keys (providers cfg))
          let st = mkState cfg providerNames [] defaultRunners "Output will appear here." ("Loaded providers from " <> toText path)
          void $ defaultMain app st

defaultRunners :: [Text]
defaultRunners =
  [ "basic"
  , "chat"
  , "embeddings"
  , "structured"
  , "tool-calling"
  , "partial"
  , "prefix"
  , "fim"
  , "get-models"
  ]

keyProvider :: Text
keyProvider = "P"

keyModel :: Text
keyModel = "M"

keyRunner :: Text
keyRunner = "R"

keyMain :: Text
keyMain = "V"

keySidebar :: Text
keySidebar = "L/D"

focusBorder :: Bool -> Widget Name -> Widget Name
focusBorder isActive =
  if isActive
    then withAttr (attrName "focusBorder")
    else id
