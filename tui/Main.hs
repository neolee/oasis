module Main where

import Relude
import Brick.AttrMap (attrMap, attrName)
import Brick.BChan (newBChan)
import Brick.Main (App(..), customMainWithVty, showCursorNamed)
import qualified Brick.Widgets.List as L
import Brick.Widgets.Skylighting (attrMappingsForStyle, highlightedCodeBlockAttr)
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.Config as VtyConfig
import qualified Graphics.Vty.CrossPlatform as VtyPlatform
import Oasis.Config
import Oasis.Tui.Event (appEvent)
import Oasis.Tui.State (AppState(..), Name(..), TuiEvent(..), mkState, defaultOutputText)
import qualified Data.Map.Strict as M
import Oasis.Tui.View (drawUI)
import Oasis.Tui.Registry (runnerNames)
import Oasis.Types (Config(..), Defaults(..))
import Skylighting.Styles (pygments)

app :: App AppState TuiEvent Name
app =
  App
    { appDraw = drawUI
    , appChooseCursor = showCursorNamed . activeList
    , appHandleEvent = appEvent
    , appStartEvent = pure ()
    , appAttrMap = const (attrMap Vty.defAttr
      ( [ (L.listAttr, Vty.defAttr)
        , (L.listSelectedAttr, Vty.defAttr `Vty.withStyle` Vty.reverseVideo)
        , (attrName "focusBorder", Vty.defAttr `Vty.withForeColor` Vty.cyan)
        , (attrName "paneContent", Vty.defAttr)
        , (attrName "mdHeading", Vty.defAttr `Vty.withStyle` Vty.bold)
        , (attrName "promptDialog", Vty.defAttr)
        , (attrName "promptDialogBorder", Vty.defAttr)
        , (attrName "promptEditor", Vty.defAttr)
        , (highlightedCodeBlockAttr, Vty.defAttr)
        , (E.editAttr, Vty.defAttr)
        , (E.editFocusedAttr, Vty.defAttr)
        , (attrName "paramLabelFocus", Vty.defAttr `Vty.withBackColor` Vty.white `Vty.withForeColor` Vty.black)
        ] <> attrMappingsForStyle pygments
      ))
    }

main :: IO ()
main = do
  chan <- newBChan 10
  mPath <- findConfig
  case mPath of
    Nothing -> do
      let emptyCfg = Config mempty (Defaults "" "") mempty
      let st = mkState chan emptyCfg [] [] runnerNames defaultOutputText "providers.toml not found"
      runTui chan st
    Just path -> do
      cfgResult <- loadConfig path
      case cfgResult of
        Left err -> do
          let emptyCfg = Config mempty (Defaults "" "") mempty
          let st = mkState chan emptyCfg [] [] runnerNames defaultOutputText ("Failed to load config: " <> err)
          runTui chan st
        Right cfg -> do
          let providerNames = sort (M.keys (providers cfg))
          let st = mkState chan cfg providerNames [] runnerNames defaultOutputText ("Loaded providers from " <> toText path)
          runTui chan st
  where
    runTui chan st = do
      userCfg <- VtyConfig.userConfig
      vty0 <- VtyPlatform.mkVty userCfg
      (_, vty) <- customMainWithVty vty0 (VtyPlatform.mkVty userCfg) (Just chan) app st
      Vty.shutdown vty
