module Main where

import Relude
import Brick.AttrMap (attrMap, attrName)
import Brick.BChan (newBChan)
import Brick.Main (App(..), customMainWithDefaultVty, showFirstCursor)
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as Vty
import Oasis.Config
import Oasis.Tui.Event (appEvent)
import Oasis.Tui.State (AppState(..), Name(..), TuiEvent(..), mkState)
import qualified Data.Map.Strict as M
import Oasis.Tui.View (drawUI)
import Oasis.Types (Config(..), Defaults(..))

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
      , (attrName "paneContent", Vty.defAttr)
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
