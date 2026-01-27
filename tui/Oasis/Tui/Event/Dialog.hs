module Oasis.Tui.Event.Dialog
  ( handlePromptEvent
  , submitPrompt
  , cancelPrompt
  , restorePromptEditor
  , handleModelInputEvent
  , submitModelInput
  , cancelModelInput
  , restoreModelInput
  , openModelInputDialog
  , handleDebugRequestEvent
  , submitDebugRequest
  , cancelDebugRequest
  , restoreDebugRequest
  , applyDebugRequestUpdate
  , handleParamDialogEvent
  , handleParamEditorEvent
  , openParamDialog
  , restoreParamDialog
  , submitParamDialog
  , cancelParamDialog
  ) where

import Relude
import Brick.Types (BrickEvent(..), EventM, nestEventM)
import Brick.Widgets.Edit (Editor, editor, getEditContents, handleEditorEvent)
import qualified Brick.Widgets.List as L
import Control.Monad.State.Class (get, modify)
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Graphics.Vty as Vty
import Oasis.Chat.Message (assistantMessage)
import Oasis.Client.OpenAI.Types (ChatCompletionRequest(..))
import Oasis.Client.OpenAI.Param (ChatParams(..))
import Oasis.Tui.Actions.Common (decodeJsonText, runInBackground)
import Oasis.Tui.Actions.Models (customModelItem)
import Oasis.Tui.Event.Editor (editorText, editorTextRaw, isPromptInputStart, handleSimpleEditorEvent)
import Oasis.Tui.Event.Utils
  ( copyAllFromEditor
  , isBlank
  , syncVerboseList
  , lastUserPrompt
  , hasAssistantTail
  , buildModelItems
  )
import Oasis.Tui.Registry (RunnerAction(..), RunnerSpec(..), lookupRunner)
import Oasis.Tui.State (AppState(..), Name(..), ParamField(..))
import Oasis.Types (StopParam(..))

handlePromptEvent :: Vty.Event -> EventM Name AppState ()
handlePromptEvent ev =
  case ev of
    Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl] -> do
      st <- get
      copyAllFromEditor (promptEditor st)
    Vty.EvKey (Vty.KChar 'r') [Vty.MCtrl] -> restorePromptEditor
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

handleModelInputEvent :: Vty.Event -> EventM Name AppState ()
handleModelInputEvent ev = do
  st <- get
  handleSimpleEditorEvent
    ev
    (modelInputEditor st)
    (\ed s -> s { modelInputEditor = ed })
    (copyAllFromEditor (modelInputEditor st))
    restoreModelInput
    submitModelInput
    cancelModelInput

handleDebugRequestEvent :: Vty.Event -> EventM Name AppState ()
handleDebugRequestEvent ev = do
  st <- get
  handleSimpleEditorEvent
    ev
    (debugRequestEditor st)
    (\ed s -> s
      { debugRequestEditor = ed
      , debugRequestDraft = editorTextRaw ed
      })
    (copyAllFromEditor (debugRequestEditor st))
    restoreDebugRequest
    submitDebugRequest
    cancelDebugRequest

submitDebugRequest :: EventM Name AppState ()
submitDebugRequest = do
  st <- get
  let draft = editorTextRaw (debugRequestEditor st)
      payload = if T.null (T.strip draft)
        then debugRequestOriginal st
        else draft
  applyDebugRequestUpdate payload
  case debugPendingAction st of
    Nothing ->
      modify (\s -> s
        { debugDialogOpen = False
        , debugRequestError = Nothing
        , debugRequestInfo = Nothing
        , activeList = debugDialogReturnFocus s
        })
    Just handler ->
      case handler payload of
        Left err ->
          modify (\s -> s { debugRequestError = Just err })
        Right action -> do
          modify (\s -> s
            { debugDialogOpen = False
            , debugRequestError = Nothing
            , debugRequestInfo = Nothing
            , debugPendingAction = Nothing
            , activeList = debugDialogReturnFocus s
            })
          runInBackground st action

cancelDebugRequest :: EventM Name AppState ()
cancelDebugRequest =
  do
    st <- get
    case debugPendingAction st of
      Nothing ->
        modify (\s -> s
          { debugDialogOpen = False
          , debugRequestError = Nothing
          , debugRequestInfo = Nothing
          , debugPendingAction = Nothing
          , activeList = RunnerList
          , statusText = "Debug request cancelled."
          })
      Just handler ->
        case handler "" of
          Left err ->
            modify (\s -> s
              { debugRequestError = Just err
              , statusText = "Debug request cancelled."
              })
          Right action -> do
            modify (\s -> s
              { debugDialogOpen = False
              , debugRequestError = Nothing
              , debugRequestInfo = Nothing
              , debugPendingAction = Nothing
              , activeList = RunnerList
              , statusText = "Debug request cancelled."
              })
            runInBackground st action

restoreDebugRequest :: EventM Name AppState ()
restoreDebugRequest = do
  st <- get
  let original = debugRequestOriginal st
  modify (\s -> s
    { debugRequestEditor = editor DebugRequestEditor (Just 12) original
    , debugRequestDraft = original
    , debugRequestError = Nothing
    })

restorePromptEditor :: EventM Name AppState ()
restorePromptEditor = do
  st <- get
  modify (\s -> s
    { promptEditor = editor PromptEditor (Just 5) (promptDefault st)
    , promptPristine = True
    })

restoreModelInput :: EventM Name AppState ()
restoreModelInput = do
  st <- get
  modify (\s -> s { modelInputEditor = editor ModelInputEditor (Just 1) (modelInputDefault st) })

applyDebugRequestUpdate :: Text -> EventM Name AppState ()
applyDebugRequestUpdate payload =
  case (decodeJsonText payload :: Either Text ChatCompletionRequest) of
    Left _ -> pure ()
    Right req -> do
      let reqMessages = messages req
          needsAssistant = stream req && not (hasAssistantTail reqMessages)
          nextMessages = if needsAssistant
            then reqMessages <> [assistantMessage ""]
            else reqMessages
          promptText = lastUserPrompt reqMessages
      modify (\s ->
        let list' = syncVerboseList nextMessages (verboseMessageList s)
        in s
          { chatMessages = nextMessages
          , verboseMessageList = list'
          , lastPrompt = fromMaybe (lastPrompt s) promptText
          })

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
    Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl] -> copyParamField
    Vty.EvKey (Vty.KChar 'r') [Vty.MCtrl] -> restoreParamDialog
    Vty.EvKey Vty.KEnter [] -> submitParamDialog
    Vty.EvKey Vty.KEsc [] -> cancelParamDialog
    Vty.EvKey Vty.KUp [] -> moveParamFocus (-1)
    Vty.EvKey Vty.KDown [] -> moveParamFocus 1
    Vty.EvKey (Vty.KChar ' ') [] -> toggleBetaWhenFocused
    _ -> handleParamEditorEvent ev

copyParamField :: EventM Name AppState ()
copyParamField = do
  st <- get
  case paramDialogFocus st of
    ParamBetaUrl ->
      let val = if paramDialogBetaValue st then "true" else "false"
      in copyAllFromEditor (editor ParamBetaUrlEditor (Just 1) val)
    ParamTemperature -> copyAllFromEditor (paramTemperatureEditor st)
    ParamTopP -> copyAllFromEditor (paramTopPEditor st)
    ParamMaxCompletionTokens -> copyAllFromEditor (paramMaxCompletionTokensEditor st)
    ParamStop -> copyAllFromEditor (paramStopEditor st)

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

openModelInputDialog :: EventM Name AppState ()
openModelInputDialog = do
  st <- get
  let defaultText = fromMaybe "" (selectedModel st)
  modify (\s -> s
    { modelInputDialogOpen = True
    , modelInputDefault = defaultText
    , modelInputEditor = editor ModelInputEditor (Just 1) defaultText
    , activeList = ModelInputEditor
    })

restoreParamDialog :: EventM Name AppState ()
restoreParamDialog = do
  st <- get
  let ChatParams{..} = chatParams st
      tempText = maybe "" show paramTemperature
      topPText = maybe "" show paramTopP
      maxTokensText = maybe "" show paramMaxCompletionTokens
      stopText = maybe "" renderStopParam paramStop
  modify (\s -> s
    { paramDialogBetaValue = betaUrlSetting s
    , paramTemperatureEditor = editor ParamTemperatureEditor (Just 1) tempText
    , paramTopPEditor = editor ParamTopPEditor (Just 1) topPText
    , paramMaxCompletionTokensEditor = editor ParamMaxCompletionTokensEditor (Just 1) maxTokensText
    , paramStopEditor = editor ParamStopEditor (Just 1) stopText
    , paramDialogError = Nothing
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
  case selectedRunner st >>= lookupRunner of
    Just RunnerSpec{runnerAction = NeedsPrompt action} -> action prompt
    Just RunnerSpec{runnerAction = Unsupported} ->
      modify (\s -> s { statusText = "Runner not supported yet." })
    _ -> modify (\s -> s { statusText = "Runner not supported yet." })

cancelPrompt :: EventM Name AppState ()
cancelPrompt =
  modify (\s -> s
    { promptDialogOpen = False
    , activeList = RunnerList
    , statusText = "Prompt cancelled."
    })

submitModelInput :: EventM Name AppState ()
submitModelInput = do
  st <- get
  let raw = editorTextRaw (modelInputEditor st)
      trimmed = T.strip raw
  if T.null trimmed
    then cancelModelInput
    else if trimmed == customModelItem
      then modify (\s -> s { statusText = "Invalid model name." })
      else do
        let customModels' = List.nub (customModels st <> [trimmed])
        let models = case selectedProvider st of
              Nothing -> customModels'
              Just providerName -> buildModelItems (config st) providerName customModels'
        let modelListBase = L.list ModelList (V.fromList models) 1
        let modelList' = case List.elemIndex trimmed models of
              Nothing -> modelListBase
              Just idx -> L.listMoveTo idx modelListBase
        modify (\s -> s
          { modelInputDialogOpen = False
          , selectedModel = Just trimmed
          , customModels = customModels'
          , modelList = modelList'
          , activeList = RunnerList
          , statusText = "Model override: " <> trimmed
          })

cancelModelInput :: EventM Name AppState ()
cancelModelInput =
  modify (\s -> s
    { modelInputDialogOpen = False
    , activeList = ModelList
    , statusText = "Model override cancelled."
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
