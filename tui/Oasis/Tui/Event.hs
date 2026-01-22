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
import Brick.Widgets.Edit (Editor, editor, getEditContents, handleEditorEvent)
import qualified Brick.Widgets.List as L
import Control.Monad.State.Class (get, modify)
import Data.Char (isSpace)
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Graphics.Vty as Vty
import Oasis.Client.OpenAI.Param (ChatParams(..))
import Oasis.Tui.Actions (providerModels, runBasicAction, runResponsesAction, runModelsAction, runEmbeddingsAction, runHooksAction, runStructuredJsonAction, runStructuredSchemaAction)
import Oasis.Tui.State (AppState(..), Name(..), ParamField(..), TuiEvent(..))
import Oasis.Types (StopParam(..))

appEvent :: BrickEvent Name TuiEvent -> EventM Name AppState ()
appEvent (AppEvent evt) =
  case evt of
    StructuredStreaming{eventOutput} ->
      modify (\s -> s { outputText = eventOutput })
    _ ->
      modify (\s -> s
        { statusText = eventStatus evt
        , outputText = eventOutput evt
        , promptDialogOpen = False
        , paramDialogOpen = False
        , activeList = MainViewport
        })
appEvent (VtyEvent ev) =
  do
    st <- get
    if paramDialogOpen st
      then handleParamDialogEvent ev
      else if promptDialogOpen st
        then handlePromptEvent ev
      else case ev of
        Vty.EvKey (Vty.KChar 'q') [] -> halt
        Vty.EvKey (Vty.KChar 'p') [] -> setActive ProviderList
        Vty.EvKey (Vty.KChar 'm') [] -> setActive ModelList
        Vty.EvKey (Vty.KChar 'r') [] -> setActive RunnerList
        Vty.EvKey (Vty.KChar 'v') [] -> setActive MainViewport
        Vty.EvKey (Vty.KChar 'a') [] -> openParamDialog
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
    ParamBetaUrlEditor -> pure ()
    ParamTemperatureEditor -> pure ()
    ParamTopPEditor -> pure ()
    ParamMaxCompletionTokensEditor -> pure ()
    ParamStopEditor -> pure ()

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

handleParamEditorEvent :: Vty.Event -> EventM Name AppState ()
handleParamEditorEvent ev = do
  st <- get
  case paramDialogFocus st of
    ParamBetaUrl -> pure ()
    ParamTemperature -> do
      (editor', _) <- nestEventM (paramTemperatureEditor st) (handleEditorEvent (VtyEvent ev))
      modify (\s -> s { paramTemperatureEditor = editor' })
    ParamTopP -> do
      (editor', _) <- nestEventM (paramTopPEditor st) (handleEditorEvent (VtyEvent ev))
      modify (\s -> s { paramTopPEditor = editor' })
    ParamMaxCompletionTokens -> do
      (editor', _) <- nestEventM (paramMaxCompletionTokensEditor st) (handleEditorEvent (VtyEvent ev))
      modify (\s -> s { paramMaxCompletionTokensEditor = editor' })
    ParamStop -> do
      (editor', _) <- nestEventM (paramStopEditor st) (handleEditorEvent (VtyEvent ev))
      modify (\s -> s { paramStopEditor = editor' })

handleParamDialogEvent :: Vty.Event -> EventM Name AppState ()
handleParamDialogEvent ev =
  case ev of
    Vty.EvKey Vty.KEnter [] -> submitParamDialog
    Vty.EvKey Vty.KEsc [] -> cancelParamDialog
    Vty.EvKey Vty.KUp [] -> moveParamFocus (-1)
    Vty.EvKey Vty.KDown [] -> moveParamFocus 1
    Vty.EvKey (Vty.KChar '\t') [] -> moveParamFocus 1
    Vty.EvKey Vty.KBackTab [] -> moveParamFocus (-1)
    Vty.EvKey (Vty.KChar ' ') [] -> toggleBetaWhenFocused
    _ -> handleParamEditorEvent ev

toggleBetaWhenFocused :: EventM Name AppState ()
toggleBetaWhenFocused = do
  st <- get
  when (paramDialogFocus st == ParamBetaUrl) $
    modify (\s -> s { paramDialogBetaValue = not (paramDialogBetaValue s) })

moveParamFocus :: Int -> EventM Name AppState ()
moveParamFocus delta =
  modify (\s ->
    let order = [ParamBetaUrl, ParamTemperature, ParamTopP, ParamMaxCompletionTokens, ParamStop]
        current = paramDialogFocus s
        idx = fromMaybe 0 (List.elemIndex current order)
        nextIdx = (idx + delta) `mod` length order
        nextField = order List.!! nextIdx
        nextName = paramFieldName nextField
    in s { paramDialogFocus = nextField, activeList = nextName }
  )

paramFieldName :: ParamField -> Name
paramFieldName = \case
  ParamBetaUrl -> ParamBetaUrlEditor
  ParamTemperature -> ParamTemperatureEditor
  ParamTopP -> ParamTopPEditor
  ParamMaxCompletionTokens -> ParamMaxCompletionTokensEditor
  ParamStop -> ParamStopEditor

openParamDialog :: EventM Name AppState ()
openParamDialog = do
  st <- get
  let ChatParams{..} = chatParams st
      tempText = maybe "" show paramTemperature
      topPText = maybe "" show paramTopP
      maxTokensText = maybe "" show paramMaxCompletionTokens
      stopText = maybe "" renderStopParam paramStop
  modify (\s -> s
    { paramDialogOpen = True
    , paramDialogError = Nothing
    , paramDialogFocus = ParamBetaUrl
    , paramDialogReturnFocus = activeList s
    , activeList = ParamBetaUrlEditor
    , promptDialogOpen = False
    , paramDialogBetaValue = betaUrlSetting s
    , paramTemperatureEditor = editor ParamTemperatureEditor (Just 1) tempText
    , paramTopPEditor = editor ParamTopPEditor (Just 1) topPText
    , paramMaxCompletionTokensEditor = editor ParamMaxCompletionTokensEditor (Just 1) maxTokensText
    , paramStopEditor = editor ParamStopEditor (Just 1) stopText
    })

renderStopParam :: StopParam -> Text
renderStopParam = \case
  StopText t -> quote t
  StopList xs -> T.intercalate ", " (map quote xs)
  where
    quote t = "\"" <> t <> "\""

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
  case selectedRunner st of
    Just "basic" -> runBasicAction prompt
    Just "responses" -> runResponsesAction prompt
    Just "embeddings" -> runEmbeddingsAction prompt
    Just "hooks" -> runHooksAction prompt
    _ -> modify (\s -> s { statusText = "Runner not supported yet." })

cancelPrompt :: EventM Name AppState ()
cancelPrompt =
  modify (\s -> s
    { promptDialogOpen = False
    , activeList = RunnerList
    , statusText = "Prompt cancelled."
    })

submitParamDialog :: EventM Name AppState ()
submitParamDialog = do
  st <- get
  let tempRaw = editorText (paramTemperatureEditor st)
      topPRaw = editorText (paramTopPEditor st)
      maxTokensRaw = editorText (paramMaxCompletionTokensEditor st)
      stopRaw = editorText (paramStopEditor st)
  case parseParamInputs tempRaw topPRaw maxTokensRaw stopRaw of
    Left err ->
      modify (\s -> s { paramDialogError = Just err })
    Right (tempVal, topPVal, maxTokensVal, stopVal) -> do
      let params0 = chatParams st
          params1 = params0
            { paramTemperature = tempVal
            , paramTopP = topPVal
            , paramMaxCompletionTokens = maxTokensVal
            , paramStop = stopVal
            }
      modify (\s -> s
        { chatParams = params1
        , betaUrlSetting = paramDialogBetaValue s
        , paramDialogOpen = False
        , paramDialogError = Nothing
        , activeList = paramDialogReturnFocus s
        , statusText = "Chat parameters updated."
        })

cancelParamDialog :: EventM Name AppState ()
cancelParamDialog =
  modify (\s -> s
    { paramDialogOpen = False
    , paramDialogError = Nothing
    , activeList = paramDialogReturnFocus s
    , statusText = "Chat parameter update cancelled."
    })

parseParamInputs
  :: Text
  -> Text
  -> Text
  -> Text
  -> Either Text (Maybe Double, Maybe Double, Maybe Int, Maybe StopParam)
parseParamInputs tempRaw topPRaw maxTokensRaw stopRaw = do
  tempVal <- parseMaybeDouble "temperature" tempRaw
  topPVal <- parseMaybeDouble "top_p" topPRaw
  maxTokensVal <- parseMaybeInt "max_completion_tokens" maxTokensRaw
  stopVal <- parseMaybeStop stopRaw
  pure (tempVal, topPVal, maxTokensVal, stopVal)

parseMaybeDouble :: Text -> Text -> Either Text (Maybe Double)
parseMaybeDouble label raw =
  let trimmed = T.strip raw
  in if T.null trimmed
      then Right Nothing
      else case readMaybe (toString trimmed) of
        Nothing -> Left (label <> ": expected a number")
        Just val -> Right (Just val)

parseMaybeInt :: Text -> Text -> Either Text (Maybe Int)
parseMaybeInt label raw =
  let trimmed = T.strip raw
  in if T.null trimmed
      then Right Nothing
      else case readMaybe (toString trimmed) of
        Nothing -> Left (label <> ": expected an integer")
        Just val -> Right (Just val)

parseMaybeStop :: Text -> Either Text (Maybe StopParam)
parseMaybeStop raw =
  let trimmed = T.strip raw
  in if T.null trimmed
      then Right Nothing
      else do
        parts <- traverse parseQuoted (filter (not . T.null) (map T.strip (T.splitOn "," trimmed)))
        case parts of
          [] -> Right Nothing
          [x] -> Right (Just (StopText x))
          xs -> Right (Just (StopList xs))

parseQuoted :: Text -> Either Text Text
parseQuoted t
  | T.length t >= 2 && T.head t == '"' && T.last t == '"' =
      Right (T.dropEnd 1 (T.drop 1 t))
  | otherwise = Left "stop: expected quoted strings like \"foo\", \"bar\""

editorText :: Editor Text Name -> Text
editorText = T.strip . mconcat . getEditContents


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
          if runnerName `elem` ["basic", "responses", "embeddings", "hooks"]
            then
              modify (\s -> s
                { selectedRunner = Just runnerName
                , activeList = PromptEditor
                , promptDialogOpen = True
                , promptEditor = editor PromptEditor (Just 5) (promptDefault s)
                , promptPristine = True
                , statusText = "Enter prompt for " <> runnerName <> " runner."
                })
            else if runnerName == "models"
              then do
                modify (\s -> s
                  { selectedRunner = Just runnerName
                  , activeList = MainViewport
                  })
                runModelsAction
              else if runnerName == "structured-json"
                then do
                  modify (\s -> s
                    { selectedRunner = Just runnerName
                    , activeList = MainViewport
                    })
                  runStructuredJsonAction
                else if runnerName == "structured-schema"
                  then do
                    modify (\s -> s
                      { selectedRunner = Just runnerName
                      , activeList = MainViewport
                      })
                    runStructuredSchemaAction
              else
                modify (\s -> s
                  { selectedRunner = Just runnerName
                  , activeList = MainViewport
                  , statusText = "Selected runner: " <> runnerName
                  })
    MainViewport -> pure ()
