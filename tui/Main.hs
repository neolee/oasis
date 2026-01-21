module Main where

import Relude
import Brick.AttrMap (attrMap, attrName)
import Brick.BChan (newBChan, writeBChan)
import Brick.Main (App(..), customMainWithDefaultVty, halt, showFirstCursor, viewportScroll, vScrollBy)
import Brick.Types (BrickEvent(..), EventM, Widget, nestEventM, ViewportType(..))
import Brick.Widgets.Border
import Brick.Widgets.Core
import qualified Brick.Widgets.List as L
import Control.Concurrent (forkIO)
import Control.Monad.State.Class (get, modify)
import qualified Data.Map.Strict as M
import qualified Data.List as List
import qualified Data.Vector as V
import qualified Graphics.Vty as Vty
import Oasis.Config
import Oasis.Tui.State (AppState(..), Name(..), TuiEvent(..), mkState)
import Oasis.Types (Config(..), Defaults(..), Provider(..), RequestResponse(..))
import Oasis.Client.OpenAI.Param (emptyChatParams)
import Oasis.Runner.Basic (runBasic)

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
            , padTop (Pad 1) (hBorder)
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


appEvent :: BrickEvent Name TuiEvent -> EventM Name AppState ()
appEvent (AppEvent evt) =
  modify (\s -> s
    { statusText = eventStatus evt
    , outputText = eventOutput evt
    })
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
    Vty.EvKey Vty.KUp [] -> handleUpDown (-1)
    Vty.EvKey Vty.KDown [] -> handleUpDown 1
    Vty.EvKey (Vty.KChar 'v') [Vty.MCtrl] -> vScrollBy (viewportScroll MainViewport) 6
    Vty.EvKey (Vty.KChar 'V') [Vty.MCtrl] -> vScrollBy (viewportScroll MainViewport) 6
    Vty.EvKey (Vty.KChar 'v') [Vty.MMeta] -> vScrollBy (viewportScroll MainViewport) (-6)
    Vty.EvKey (Vty.KChar 'V') [Vty.MMeta] -> vScrollBy (viewportScroll MainViewport) (-6)
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

scrollMain :: Int -> EventM Name AppState ()
scrollMain amount = do
  st <- get
  when (activeList st == MainViewport) $ vScrollBy (viewportScroll MainViewport) amount

handleUpDown :: Int -> EventM Name AppState ()
handleUpDown amount = do
  st <- get
  if activeList st == MainViewport
    then scrollMain amount
    else handleActiveListEvent (if amount < 0 then Vty.EvKey Vty.KUp [] else Vty.EvKey Vty.KDown [])

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
            , activeList = ModelList
            , statusText = "Selected provider: " <> providerName
            })
    ModelList ->
      case L.listSelectedElement (modelList st) of
        Nothing -> pure ()
        Just (_, modelName) ->
          modify (\s -> s
            { selectedModel = Just modelName
            , activeList = RunnerList
            , statusText = "Selected model: " <> modelName
            })
    RunnerList ->
      case L.listSelectedElement (runnerList st) of
        Nothing -> pure ()
        Just (_, runnerName) ->
          if runnerName == "basic"
            then do
              modify (\s -> s
                { selectedRunner = Just runnerName
                , statusText = "Selected runner: " <> runnerName
                })
              runBasicAction
            else
              modify (\s -> s
                { selectedRunner = Just runnerName
                , statusText = "Selected runner: " <> runnerName
                })

runBasicAction :: EventM Name AppState ()
runBasicAction = do
  st <- get
  case selectedProvider st of
    Nothing ->
      modify (\s -> s { statusText = "Select a provider first." })
    Just providerName -> do
      resolved <- liftIO (resolveProvider (config st) providerName)
      case resolved of
        Nothing ->
          modify (\s -> s { statusText = "Provider not found: " <> providerName })
        Just (provider, apiKey) -> do
          let modelOverride = selectedModel st
          let prompt = "Hello from oasis-tui basic runner."
          modify (\s -> s
            { statusText = "Running basic runner..."
            , outputText = ""
            })
          let chan = eventChan st
          void $ liftIO $ forkIO $ do
            result <- runBasic provider apiKey modelOverride emptyChatParams prompt False
            let (statusMsg, outputMsg) =
                  case result of
                    Left err ->
                      ("Basic runner failed.", "Error:\n" <> err)
                    Right rr ->
                      ("Basic runner completed.", "Request:\n" <> requestJson rr <> "\n\nResponse:\n" <> responseJson rr)
            writeBChan chan (BasicCompleted statusMsg outputMsg)

providerModels :: Config -> Text -> [Text]
providerModels cfg providerName =
  case M.lookup providerName (providers cfg) of
    Nothing -> []
    Just Provider{chat_model_id, coder_model_id, reasoner_model_id} ->
      List.nub [chat_model_id, coder_model_id, reasoner_model_id]

app :: App AppState TuiEvent Name
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
  chan <- newBChan 10
  mPath <- findConfig
  case mPath of
    Nothing -> do
      let emptyCfg = Config mempty (Defaults "" "") mempty
      let st = mkState chan emptyCfg [] [] defaultRunners "Output will appear here." "providers.toml not found"
      void $ customMainWithDefaultVty (Just chan) app st
    Just path -> do
      cfgResult <- loadConfig path
      case cfgResult of
        Left err -> do
          let emptyCfg = Config mempty (Defaults "" "") mempty
          let st = mkState chan emptyCfg [] [] defaultRunners "Output will appear here." ("Failed to load config: " <> err)
          void $ customMainWithDefaultVty (Just chan) app st
        Right cfg -> do
          let providerNames = sort (M.keys (providers cfg))
          let st = mkState chan cfg providerNames [] defaultRunners "Output will appear here." ("Loaded providers from " <> toText path)
          void $ customMainWithDefaultVty (Just chan) app st

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

data PaneKind
  = ListPane
  | OutputPane
  | InputPane

tipsFor :: AppState -> Text
tipsFor st =
  case paneKind (activeList st) of
    ListPane -> "[↑/↓] Move  [Enter] Select"
    OutputPane -> "[↑/↓] Scroll  [Ctrl+V/Alt+V] Page"
    InputPane -> "[Enter] Submit  [Esc] Cancel"

paneKind :: Name -> PaneKind
paneKind = \case
  ProviderList -> ListPane
  ModelList -> ListPane
  RunnerList -> ListPane
  MainViewport -> OutputPane

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
