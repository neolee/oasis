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
import Brick.Widgets.Edit (editor)
import qualified Brick.Widgets.List as L
import Control.Monad.State.Class (get, modify)
import qualified Data.Vector as V
import qualified Graphics.Vty as Vty
import Oasis.Tui.Actions.Models (customModelItem)
import Oasis.Tui.Actions.Chat (runChatAction)
import Oasis.Tui.Actions.Common (openDebugDialog)
import Oasis.Tui.Event.Dialog
  ( handlePromptEvent
  , handleModelInputEvent
  , handleDebugRequestEvent
  , handleParamDialogEvent
  , openParamDialog
  , openModelInputDialog
  )
import Oasis.Tui.Event.Editor (editorText, editorTextRaw, handleSimpleEditorEvent)
import Oasis.Tui.Event.Utils
  ( copyAllFromEditor
  , isBlank
  , syncVerboseList
  , clearVerboseList
  , insertMessageAt
  , updateMessageAt
  , deleteMessageAt
  , buildModelItems
  )
import Oasis.Tui.Registry (RunnerAction(..), RunnerSpec(..), lookupRunner)
import Oasis.Tui.State (AppState(..), Name(..), TuiEvent(..))
import Oasis.Chat.Message (assistantMessage, userMessage)
import Oasis.Types (Message(..), MessageContent(..), messageContentText)

appEvent :: BrickEvent Name TuiEvent -> EventM Name AppState ()
appEvent (AppEvent evt) =
  case evt of
    DebugRequestOpen{eventDebugInfo, eventDebugOriginal, eventDebugHandler, eventDebugReturnFocus} ->
      openDebugDialog eventDebugReturnFocus eventDebugInfo eventDebugOriginal eventDebugHandler
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
          else if modelInputDialogOpen st
            then handleModelInputEvent ev
          else if promptDialogOpen st
            then handlePromptEvent ev
            else if debugDialogOpen st
              then handleDebugRequestEvent ev
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
          when (activeList st == VerboseMessageList) openRoleDialogForInsert
        Vty.EvKey Vty.KDel [] ->
          when (activeList st == VerboseMessageList) startDeleteConfirm
        Vty.EvKey Vty.KBS [] ->
          when (activeList st == VerboseMessageList) startDeleteConfirm
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

restoreChatInput :: EventM Name AppState ()
restoreChatInput =
  modify (\s -> s { chatInputEditor = editor ChatInputEditor (Just 3) "" })

restoreVerboseContent :: EventM Name AppState ()
restoreVerboseContent = do
  st <- get
  case verboseEditIndex st of
    Nothing -> pure ()
    Just idx ->
      case drop idx (chatMessages st) of
        (msg:_) ->
          let original = messageContentText (content msg)
          in modify (\s -> s { verboseContentEditor = editor VerboseContentEditor (Just 8) original })
        _ -> pure ()

handleChatInputEvent :: Vty.Event -> EventM Name AppState ()
handleChatInputEvent ev = do
  st <- get
  handleSimpleEditorEvent
    ev
    (chatInputEditor st)
    (\ed s -> s { chatInputEditor = ed })
    (copyAllFromEditor (chatInputEditor st))
    restoreChatInput
    submitChatInput
    cancelChatInput

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
          let newMsg = Message roleText (ContentText "") Nothing Nothing Nothing Nothing Nothing
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
handleVerboseContentEvent ev = do
  st <- get
  handleSimpleEditorEvent
    ev
    (verboseContentEditor st)
    (\ed s -> s { verboseContentEditor = ed })
    (copyAllFromEditor (verboseContentEditor st))
    restoreVerboseContent
    saveVerboseContent
    cancelVerboseContent

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

applySelection :: EventM Name AppState ()
applySelection = do
  st <- get
  case activeList st of
    ProviderList ->
      case L.listSelectedElement (providerList st) of
        Nothing -> pure ()
        Just (_, providerName) -> do
          let customModels' = []
          let models = buildModelItems (config st) providerName customModels'
          let modelList' = L.list ModelList (V.fromList models) 1
          modify (\s -> s
            { selectedProvider = Just providerName
            , selectedModel = listToMaybe models
            , modelList = modelList'
            , customModels = customModels'
            , activeList = ModelList
            , statusText = "Selected provider: " <> providerName
            })
    ModelList ->
      case L.listSelectedElement (modelList st) of
        Nothing -> pure ()
        Just (_, modelName) ->
          if modelName == customModelItem
            then openModelInputDialog
            else modify (\s -> s
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
