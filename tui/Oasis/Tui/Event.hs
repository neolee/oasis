module Oasis.Tui.Event
  ( appEvent
  , setActive
  , handleActiveListEvent
  , handleUpDown
  , applySelection
  ) where

import Relude
import Brick.Main (halt, viewportScroll, vScrollBy, hScrollBy, vScrollToEnd)
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
import Oasis.Tui.Actions.Models ( providerModels )
import Oasis.Tui.Actions.Chat (runChatAction)
import Oasis.Tui.Registry (RunnerAction(..), RunnerSpec(..), lookupRunner)
import Oasis.Tui.State (AppState(..), Name(..), ParamField(..), TuiEvent(..))
import Oasis.Chat.Message (assistantMessage, userMessage)
import Oasis.Types (Message(..), MessageContent(..), StopParam(..), messageContentText)

appEvent :: BrickEvent Name TuiEvent -> EventM Name AppState ()
appEvent (AppEvent evt) =
  case evt of
    ChatStreaming{eventDelta} ->
      do
        modify (applyChatDelta eventDelta)
        scrollChatToBottom
    ChatCompleted{eventStatus} ->
      modify (\s -> s { statusText = eventStatus })
    MessageListSynced{eventMessages} ->
      modify (\s -> applyMessages eventMessages (L.listSelected (verboseMessageList s)) s)
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
    if isJust (verboseDeleteConfirm st)
      then handleDeleteConfirm ev
      else if verboseRoleDialogOpen st
        then handleRoleDialogEvent ev
        else if testPaneOpen st
          then handleTestPaneEvent ev
          else if paramDialogOpen st
          then handleParamDialogEvent ev
          else if promptDialogOpen st
            then handlePromptEvent ev
            else if activeList st == VerboseContentEditor
              then handleVerboseContentEvent ev
              else if activeList st == ChatInputEditor
                then handleChatInputEvent ev
                else (case ev of
        Vty.EvKey (Vty.KChar 'q') [] -> halt
        Vty.EvKey (Vty.KChar 'p') [] -> setActive ProviderList
        Vty.EvKey (Vty.KChar 'm') [] -> setActive ModelList
        Vty.EvKey (Vty.KChar 'r') [] -> setActive RunnerList
        Vty.EvKey (Vty.KChar 'v') [] -> focusMainViewport
        Vty.EvKey (Vty.KChar 'e') [] ->
          if activeList st == VerboseMessageList
            then openVerboseEditor
            else focusChatInput
        Vty.EvKey (Vty.KChar 'l') [] -> focusVerboseList
        Vty.EvKey (Vty.KChar 'h') [] -> toggleVerbose
        Vty.EvKey (Vty.KChar 'd') [] -> toggleDebug
        Vty.EvKey (Vty.KChar 'a') [] ->
          if activeList st == VerboseMessageList
            then openRoleDialogForAppend
            else openParamDialog
        Vty.EvKey (Vty.KChar 'i') [] ->
          if activeList st == VerboseMessageList
            then openRoleDialogForInsert
            else pure ()
        Vty.EvKey Vty.KDel [] ->
          if activeList st == VerboseMessageList
            then startDeleteConfirm
            else pure ()
        Vty.EvKey Vty.KBS [] ->
          if activeList st == VerboseMessageList
            then startDeleteConfirm
            else pure ()
        Vty.EvKey (Vty.KChar 'x') [] -> toggleTestPane
        Vty.EvKey Vty.KEnter [] -> applySelection
        Vty.EvKey Vty.KUp [] -> handleUpDown (-1)
        Vty.EvKey Vty.KDown [] -> handleUpDown 1
        Vty.EvKey Vty.KLeft [] -> handleLeftRight (-1)
        Vty.EvKey Vty.KRight [] -> handleLeftRight 1
        Vty.EvKey (Vty.KChar 'v') [Vty.MCtrl] -> scrollPage 6
        Vty.EvKey (Vty.KChar 'v') [Vty.MMeta] -> scrollPage (-6)
        Vty.EvKey (Vty.KChar '.') [Vty.MMeta] -> scrollPageHoriz 6
        Vty.EvKey (Vty.KChar ',') [Vty.MMeta] -> scrollPageHoriz (-6)
        _ -> handleActiveListEvent ev
        )
appEvent _ = pure ()

handleTestPaneEvent :: Vty.Event -> EventM Name AppState ()
handleTestPaneEvent ev =
  case ev of
    Vty.EvKey (Vty.KChar 'x') [] -> toggleTestPane
    Vty.EvKey Vty.KEsc [] -> toggleTestPane
    _ -> pure ()

focusMainViewport :: EventM Name AppState ()
focusMainViewport = do
  st <- get
  if selectedRunner st == Just "chat"
    then setActive ChatViewport
    else setActive MainViewport

focusChatInput :: EventM Name AppState ()
focusChatInput = do
  st <- get
  when (selectedRunner st == Just "chat") (setActive ChatInputEditor)

focusVerboseList :: EventM Name AppState ()
focusVerboseList = do
  st <- get
  when (verboseEnabled st) $ do
    let lst = verboseMessageList st
        hasSelection = isJust (L.listSelected lst)
        lst' = if hasSelection || V.null (L.listElements lst)
          then lst
          else L.listMoveTo 0 lst
    modify (\s -> s { activeList = VerboseMessageList, verboseMessageList = lst' })

toggleVerbose :: EventM Name AppState ()
toggleVerbose = modify (\s ->
  let enabled' = not (verboseEnabled s)
      active' = if enabled'
        then activeList s
        else case activeList s of
          VerboseMessageList -> MainViewport
          VerboseContentEditor -> MainViewport
          _ -> activeList s
  in s { verboseEnabled = enabled', activeList = active' }
  )

toggleDebug :: EventM Name AppState ()
toggleDebug = modify (\s -> s { debugEnabled = not (debugEnabled s) })

toggleTestPane :: EventM Name AppState ()
toggleTestPane = modify (\s -> s { testPaneOpen = not (testPaneOpen s) })

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
    VerboseMessageList -> do
      (lst, _) <- nestEventM (verboseMessageList st) (L.handleListEvent ev)
      modify (\s -> s { verboseMessageList = lst })
    VerboseRoleList -> do
      (lst, _) <- nestEventM (verboseRoleList st) (L.handleListEvent ev)
      modify (\s -> s { verboseRoleList = lst })
    _ -> pure ()

scrollMain :: Int -> EventM Name AppState ()
scrollMain amount = do
  st <- get
  when (activeList st == MainViewport) $ vScrollBy (viewportScroll MainViewport) amount

scrollChat :: Int -> EventM Name AppState ()
scrollChat amount = do
  st <- get
  when (activeList st == ChatViewport) $ vScrollBy (viewportScroll ChatViewport) amount

scrollMainHoriz :: Int -> EventM Name AppState ()
scrollMainHoriz amount = do
  st <- get
  when (activeList st == MainViewport) $ hScrollBy (viewportScroll MainViewport) amount

scrollPage :: Int -> EventM Name AppState ()
scrollPage amount = do
  st <- get
  case activeList st of
    MainViewport -> vScrollBy (viewportScroll MainViewport) amount
    ChatViewport -> vScrollBy (viewportScroll ChatViewport) amount
    _ -> pure ()

scrollPageHoriz :: Int -> EventM Name AppState ()
scrollPageHoriz amount = do
  st <- get
  case activeList st of
    MainViewport -> hScrollBy (viewportScroll MainViewport) amount
    ChatViewport -> hScrollBy (viewportScroll ChatViewport) amount
    _ -> pure ()

scrollChatToBottom :: EventM Name AppState ()
scrollChatToBottom =
  vScrollToEnd (viewportScroll ChatViewport)

handleUpDown :: Int -> EventM Name AppState ()
handleUpDown amount = do
  st <- get
  case activeList st of
    MainViewport -> scrollMain amount
    ChatViewport -> scrollChat amount
    ProviderList ->
      modify (\t -> t { providerList = moveListWrap amount (providerList t) })
    ModelList ->
      modify (\t -> t { modelList = moveListWrap amount (modelList t) })
    RunnerList ->
      modify (\t -> t { runnerList = moveListWrap amount (runnerList t) })
    _ -> handleActiveListEvent (if amount < 0 then Vty.EvKey Vty.KUp [] else Vty.EvKey Vty.KDown [])

handleLeftRight :: Int -> EventM Name AppState ()
handleLeftRight amount = do
  st <- get
  when (activeList st == MainViewport) $ scrollMainHoriz amount
  when (activeList st == ChatViewport) $ hScrollBy (viewportScroll ChatViewport) amount

moveListWrap :: Int -> L.List Name Text -> L.List Name Text
moveListWrap delta lst =
  let len = V.length (L.listElements lst)
  in if len == 0
      then lst
      else
        let current = fromMaybe 0 (L.listSelected lst)
            next
              | delta < 0 = if current <= 0 then len - 1 else current - 1
              | delta > 0 = if current >= len - 1 then 0 else current + 1
              | otherwise = current
        in L.listMoveTo next lst

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

handleChatInputEvent :: Vty.Event -> EventM Name AppState ()
handleChatInputEvent ev =
  case ev of
    Vty.EvKey Vty.KEnter [] -> insertChatNewline ev
    Vty.EvKey (Vty.KChar 's') [Vty.MCtrl] -> submitChatInput
    Vty.EvKey Vty.KEsc [] -> cancelChatInput
    _ -> do
      st <- get
      (editor', _) <- nestEventM (chatInputEditor st) (handleEditorEvent (VtyEvent ev))
      modify (\s -> s { chatInputEditor = editor' })

insertChatNewline :: Vty.Event -> EventM Name AppState ()
insertChatNewline _ = do
  st <- get
  let newlineEvent = Vty.EvKey Vty.KEnter []
  (editor', _) <- nestEventM (chatInputEditor st) (handleEditorEvent (VtyEvent newlineEvent))
  modify (\s -> s { chatInputEditor = editor' })

cancelChatInput :: EventM Name AppState ()
cancelChatInput =
  modify (\s -> s { activeList = ChatViewport })

submitChatInput :: EventM Name AppState ()
submitChatInput = do
  st <- get
  let inputText = editorText (chatInputEditor st)
  if isBlank inputText
    then pure ()
    else do
      let newMessages = chatMessages st <> [userMessage inputText, assistantMessage ""]
          verboseList' = syncVerboseList newMessages (verboseMessageList st)
      modify (\s -> s
        { chatMessages = newMessages
        , verboseMessageList = verboseList'
        , chatInputEditor = editor ChatInputEditor (Just 3) ""
        , activeList = ChatInputEditor
        })
      scrollChatToBottom
      runChatAction newMessages

applyChatDelta :: Text -> AppState -> AppState
applyChatDelta deltaText st =
  let updated = appendAssistantDelta deltaText (chatMessages st)
      verboseList' = syncVerboseList updated (verboseMessageList st)
  in st { chatMessages = updated, verboseMessageList = verboseList' }

appendAssistantDelta :: Text -> [Message] -> [Message]
appendAssistantDelta deltaText msgs =
  case reverse msgs of
    (msg:rest)
      | role msg == "assistant" ->
          let current = messageContentText (content msg)
              nextMsg = msg { content = ContentText (current <> deltaText) }
          in reverse (nextMsg : rest)
    _ -> msgs

syncVerboseList :: [Message] -> L.List Name Message -> L.List Name Message
syncVerboseList msgs lst =
  L.listReplace (V.fromList msgs) (L.listSelected lst) lst

clearVerboseList :: L.List Name Message -> L.List Name Message
clearVerboseList = L.listReplace V.empty Nothing

applyMessages :: [Message] -> Maybe Int -> AppState -> AppState
applyMessages msgs sel s =
  s
    { chatMessages = msgs
    , verboseMessageList = L.listReplace (V.fromList msgs) sel (verboseMessageList s)
    }

openRoleDialogForAppend :: EventM Name AppState ()
openRoleDialogForAppend = do
  st <- get
  let idx = length (chatMessages st)
  modify (\s -> s
    { verboseRoleDialogOpen = True
    , verbosePendingInsertIndex = Just idx
    , activeList = VerboseRoleList
    , statusText = "Select role to append."
    })

openRoleDialogForInsert :: EventM Name AppState ()
openRoleDialogForInsert = do
  st <- get
  let idx = fromMaybe 0 (L.listSelected (verboseMessageList st))
  modify (\s -> s
    { verboseRoleDialogOpen = True
    , verbosePendingInsertIndex = Just idx
    , activeList = VerboseRoleList
    , statusText = "Select role to insert."
    })

handleRoleDialogEvent :: Vty.Event -> EventM Name AppState ()
handleRoleDialogEvent ev =
  case ev of
    Vty.EvKey Vty.KEnter [] -> confirmRoleDialog
    Vty.EvKey Vty.KEsc [] -> cancelRoleDialog
    Vty.EvKey Vty.KUp [] -> handleActiveListEvent ev
    Vty.EvKey Vty.KDown [] -> handleActiveListEvent ev
    _ -> handleActiveListEvent ev

confirmRoleDialog :: EventM Name AppState ()
confirmRoleDialog = do
  st <- get
  case L.listSelectedElement (verboseRoleList st) of
    Nothing -> pure ()
    Just (_, roleText) ->
      case verbosePendingInsertIndex st of
        Nothing -> pure ()
        Just idx -> do
          let newMsg = Message roleText (ContentText "") Nothing Nothing Nothing Nothing
              msgs = chatMessages st
              newMsgs = insertMessageAt idx newMsg msgs
          modify (\s ->
            applyMessages newMsgs (Just idx) s
              { verboseRoleDialogOpen = False
              , verbosePendingInsertIndex = Nothing
              , verboseEditIndex = Just idx
              , verboseContentEditor = editor VerboseContentEditor (Just 8) ""
              , activeList = VerboseContentEditor
              , statusText = "Edit message content."
              })

cancelRoleDialog :: EventM Name AppState ()
cancelRoleDialog =
  modify (\s -> s
    { verboseRoleDialogOpen = False
    , verbosePendingInsertIndex = Nothing
    , activeList = VerboseMessageList
    , statusText = "Role selection cancelled."
    })

openVerboseEditor :: EventM Name AppState ()
openVerboseEditor = do
  st <- get
  case L.listSelectedElement (verboseMessageList st) of
    Nothing -> modify (\s -> s { statusText = "No message selected." })
    Just (idx, msg) ->
      modify (\s -> s
        { verboseEditIndex = Just idx
        , verboseContentEditor = editor VerboseContentEditor (Just 8) (messageContentText (content msg))
        , activeList = VerboseContentEditor
        , statusText = "Editing message."
        })

handleVerboseContentEvent :: Vty.Event -> EventM Name AppState ()
handleVerboseContentEvent ev =
  case ev of
    Vty.EvKey (Vty.KChar 's') [Vty.MCtrl] -> saveVerboseContent
    Vty.EvKey Vty.KEsc [] -> cancelVerboseContent
    _ -> do
      st <- get
      (editor', _) <- nestEventM (verboseContentEditor st) (handleEditorEvent (VtyEvent ev))
      modify (\s -> s { verboseContentEditor = editor' })

saveVerboseContent :: EventM Name AppState ()
saveVerboseContent = do
  st <- get
  case verboseEditIndex st of
    Nothing -> pure ()
    Just idx -> do
      let msgs = chatMessages st
          contentText = editorTextRaw (verboseContentEditor st)
          updatedMsgs = updateMessageAt idx contentText msgs
      modify (\s ->
        applyMessages updatedMsgs (Just idx) s
          { verboseEditIndex = Nothing
          , activeList = VerboseMessageList
          , statusText = "Message saved."
          })

cancelVerboseContent :: EventM Name AppState ()
cancelVerboseContent =
  modify (\s -> s
    { verboseEditIndex = Nothing
    , activeList = VerboseMessageList
    , statusText = "Edit cancelled."
    })

startDeleteConfirm :: EventM Name AppState ()
startDeleteConfirm = do
  st <- get
  case L.listSelectedElement (verboseMessageList st) of
    Nothing -> modify (\s -> s { statusText = "No message selected." })
    Just (idx, _) ->
      modify (\s -> s
        { verboseDeleteConfirm = Just idx
        , statusText = "Delete message? [y/n]"
        })

handleDeleteConfirm :: Vty.Event -> EventM Name AppState ()
handleDeleteConfirm ev =
  case ev of
    Vty.EvKey (Vty.KChar 'y') [] -> confirmDelete
    Vty.EvKey (Vty.KChar 'n') [] -> cancelDelete
    _ -> pure ()

confirmDelete :: EventM Name AppState ()
confirmDelete = do
  st <- get
  case verboseDeleteConfirm st of
    Nothing -> pure ()
    Just idx -> do
      let msgs = deleteMessageAt idx (chatMessages st)
          newSel
            | null msgs = Nothing
            | idx >= length msgs = Just (length msgs - 1)
            | otherwise = Just idx
      modify (\s ->
        applyMessages msgs newSel s
          { verboseDeleteConfirm = Nothing
          , statusText = "Message deleted."
          })

cancelDelete :: EventM Name AppState ()
cancelDelete =
  modify (\s -> s
    { verboseDeleteConfirm = Nothing
    , statusText = "Delete cancelled."
    })

insertMessageAt :: Int -> Message -> [Message] -> [Message]
insertMessageAt idx msg msgs =
  let (before, after) = splitAt idx msgs
  in before <> [msg] <> after

updateMessageAt :: Int -> Text -> [Message] -> [Message]
updateMessageAt idx contentText msgs =
  if idx < 0 || idx >= length msgs
    then msgs
    else
      let (before, after) = splitAt idx msgs
      in case after of
           [] -> msgs
           (m:rest) -> before <> [m { content = ContentText contentText }] <> rest

deleteMessageAt :: Int -> [Message] -> [Message]
deleteMessageAt idx msgs =
  if idx < 0 || idx >= length msgs
    then msgs
    else
      let (before, after) = splitAt idx msgs
      in case after of
           [] -> msgs
           (_:rest) -> before <> rest

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

editorTextRaw :: Editor Text Name -> Text
editorTextRaw = T.intercalate "\n" . getEditContents


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
          case lookupRunner runnerName of
            Just RunnerSpec{runnerAction = NeedsPrompt _} ->
              modify (\s -> s
                { selectedRunner = Just runnerName
                , chatMessages = []
                , verboseMessageList = clearVerboseList (verboseMessageList s)
                , activeList = PromptEditor
                , promptDialogOpen = True
                , promptEditor = editor PromptEditor (Just 5) (promptDefault s)
                , promptPristine = True
                , statusText = "Enter prompt for " <> runnerName <> " runner."
                })
            Just RunnerSpec{runnerAction = NoPrompt action} -> do
              modify (\s -> s
                { selectedRunner = Just runnerName
                , chatMessages = []
                , verboseMessageList = clearVerboseList (verboseMessageList s)
                , activeList = MainViewport
                })
              action
            Just RunnerSpec{runnerAction = Unsupported} ->
              modify (\s -> s
                { selectedRunner = Just runnerName
                , chatMessages = []
                , verboseMessageList = clearVerboseList (verboseMessageList s)
                , activeList = MainViewport
                , statusText = "Runner not supported yet."
                })
            Nothing ->
              modify (\s -> s
                { selectedRunner = Just runnerName
                , activeList = MainViewport
                , statusText = "Runner not supported yet."
                })
    MainViewport -> pure ()
    _ -> pure ()
