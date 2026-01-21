module Oasis.Tui.Event
  ( appEvent
  , setActive
  , handleActiveListEvent
  , handleUpDown
  , applySelection
  ) where

import Relude
import Brick.Main (halt, viewportScroll, vScrollBy, hScrollBy)
import Brick.Types (BrickEvent(..), EventM, nestEventM)
import Brick.Widgets.Edit (editor, getEditContents, handleEditorEvent)
import qualified Brick.Widgets.List as L
import Control.Monad.State.Class (get, modify)
import Data.Char (isSpace)
import qualified Data.Vector as V
import qualified Graphics.Vty as Vty
import Oasis.Tui.Actions (providerModels, runBasicAction)
import Oasis.Tui.State (AppState(..), Name(..), TuiEvent(..))

appEvent :: BrickEvent Name TuiEvent -> EventM Name AppState ()
appEvent (AppEvent evt) =
  modify (\s -> s
    { statusText = eventStatus evt
    , outputText = eventOutput evt
    , promptDialogOpen = False
    , activeList = MainViewport
    })
appEvent (VtyEvent ev) =
  do
    st <- get
    if promptDialogOpen st
      then handlePromptEvent ev
      else case ev of
        Vty.EvKey (Vty.KChar 'q') [] -> halt
        Vty.EvKey (Vty.KChar 'p') [] -> setActive ProviderList
        Vty.EvKey (Vty.KChar 'm') [] -> setActive ModelList
        Vty.EvKey (Vty.KChar 'r') [] -> setActive RunnerList
        Vty.EvKey (Vty.KChar 'v') [] -> setActive MainViewport
        Vty.EvKey Vty.KEnter [] -> applySelection
        Vty.EvKey Vty.KUp [] -> handleUpDown (-1)
        Vty.EvKey Vty.KDown [] -> handleUpDown 1
        Vty.EvKey Vty.KLeft [] -> handleLeftRight (-1)
        Vty.EvKey Vty.KRight [] -> handleLeftRight 1
        Vty.EvKey (Vty.KChar 'v') [Vty.MCtrl] -> vScrollBy (viewportScroll MainViewport) 6
        Vty.EvKey (Vty.KChar 'v') [Vty.MMeta] -> vScrollBy (viewportScroll MainViewport) (-6)
        Vty.EvKey (Vty.KChar '.') [Vty.MMeta] -> hScrollBy (viewportScroll MainViewport) 6
        Vty.EvKey (Vty.KChar ',') [Vty.MMeta] -> hScrollBy (viewportScroll MainViewport) (-6)
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
    PromptEditor -> pure ()

scrollMain :: Int -> EventM Name AppState ()
scrollMain amount = do
  st <- get
  when (activeList st == MainViewport) $ vScrollBy (viewportScroll MainViewport) amount

scrollMainHoriz :: Int -> EventM Name AppState ()
scrollMainHoriz amount = do
  st <- get
  when (activeList st == MainViewport) $ hScrollBy (viewportScroll MainViewport) amount

handleUpDown :: Int -> EventM Name AppState ()
handleUpDown amount = do
  st <- get
  if activeList st == MainViewport
    then scrollMain amount
    else handleActiveListEvent (if amount < 0 then Vty.EvKey Vty.KUp [] else Vty.EvKey Vty.KDown [])

handleLeftRight :: Int -> EventM Name AppState ()
handleLeftRight amount = do
  st <- get
  when (activeList st == MainViewport) $ scrollMainHoriz amount

handlePromptEvent :: Vty.Event -> EventM Name AppState ()
handlePromptEvent ev =
  case ev of
    Vty.EvKey Vty.KEnter [] -> submitPrompt
    Vty.EvKey Vty.KEsc [] -> cancelPrompt
    _ -> do
      st <- get
      let shouldClear = promptPristine st && isPromptInputStart ev
          baseEditor = if shouldClear
            then editor PromptEditor (Just 5) ""
            else promptEditor st
      (editor', _) <- nestEventM baseEditor (handleEditorEvent (VtyEvent ev))
      let pristine' = not (isPromptInputStart ev) && promptPristine st
      modify (\s -> s { promptEditor = editor', promptPristine = pristine' })

submitPrompt :: EventM Name AppState ()
submitPrompt = do
  st <- get
  let rawPrompt = unlines (getEditContents (promptEditor st))
      prompt = if isBlank rawPrompt then promptDefault st else rawPrompt
  modify (\s -> s
    { promptDialogOpen = False
    , activeList = RunnerList
    , lastPrompt = prompt
    , promptPristine = False
    })
  runBasicAction prompt

cancelPrompt :: EventM Name AppState ()
cancelPrompt =
  modify (\s -> s
    { promptDialogOpen = False
    , activeList = RunnerList
    , statusText = "Prompt cancelled."
    })

isBlank :: Text -> Bool
isBlank = all isSpace . toString

isPromptInputStart :: Vty.Event -> Bool
isPromptInputStart = \case
  Vty.EvKey (Vty.KChar _) _ -> True
  Vty.EvKey Vty.KBS _ -> True
  Vty.EvKey Vty.KDel _ -> True
  _ -> False

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
            then
              modify (\s -> s
                { selectedRunner = Just runnerName
                , activeList = PromptEditor
                , promptDialogOpen = True
                , promptEditor = editor PromptEditor (Just 5) (promptDefault s)
                , promptPristine = True
                , statusText = "Enter prompt for basic runner."
                })
            else
              modify (\s -> s
                { selectedRunner = Just runnerName
                , activeList = MainViewport
                , statusText = "Selected runner: " <> runnerName
                })
    MainViewport -> pure ()
