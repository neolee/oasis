module Main where

import Relude
import Brick.AttrMap (attrMap)
import Brick.Main (App(..), defaultMain, halt, showFirstCursor)
import Brick.Types (BrickEvent(..), EventM, Widget, nestEventM)
import Brick.Widgets.Border
import Brick.Widgets.Core
import qualified Brick.Widgets.List as L
import Control.Monad.State.Class (get, modify)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Graphics.Vty as Vty
import Oasis.Config
import Oasis.Types (Config(..))

data Name
  = ProviderList
  deriving (Eq, Ord, Show)

data AppState = AppState
  { providerList :: L.List Name Text
  , statusText :: Text
  }

mkState :: [Text] -> Text -> AppState
mkState providers statusText =
  AppState
    { providerList = L.list ProviderList (V.fromList providers) 1
    , statusText
    }

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
        borderWithLabel (txt "Providers") $
          padAll 1 $
            L.renderList drawProvider True (providerList st)
    centerPane =
      borderWithLabel (txt "Main") $
        padAll 1 $
          txt "Stage 1: layout ready"
    rightPane =
      hLimit 28 $
        borderWithLabel (txt "Sidebar") $
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
    Vty.EvKey Vty.KUp [] -> modifyList L.listMoveUp
    Vty.EvKey Vty.KDown [] -> modifyList L.listMoveDown
    _ -> do
      st <- get
      (lst, _) <- nestEventM (providerList st) (L.handleListEvent ev)
      modify (\s -> s { providerList = lst })
appEvent _ = pure ()

modifyList :: (L.List Name Text -> L.List Name Text) -> EventM Name AppState ()
modifyList f = modify (\s -> s { providerList = f (providerList s) })

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
      ])
    }

main :: IO ()
main = do
  mPath <- findConfig
  case mPath of
    Nothing -> do
      let st = mkState [] "providers.toml not found"
      void $ defaultMain app st
    Just path -> do
      cfgResult <- loadConfig path
      case cfgResult of
        Left err -> do
          let st = mkState [] ("Failed to load config: " <> err)
          void $ defaultMain app st
        Right cfg -> do
          let providerNames = sort (M.keys (providers cfg))
          let st = mkState providerNames ("Loaded providers from " <> toText path)
          void $ defaultMain app st
