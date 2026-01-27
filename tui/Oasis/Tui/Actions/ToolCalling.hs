module Oasis.Tui.Actions.ToolCalling
  ( runToolCallingAction
  ) where

import Relude
import Brick.BChan (BChan, writeBChan)
import Brick.Types (EventM)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson (Value, decode, eitherDecode)
import Data.Time (getZonedTime, formatTime, defaultTimeLocale)
import Oasis.Client.OpenAI
  ( buildChatUrl
  , encodeRequestJsonWithFlatExtra
  , renderClientError
  )
import Oasis.Client.OpenAI.Types
  ( ChatCompletionRequest(..)
  , ChatCompletionResponse(..)
  , ChatChoice(..)
  )
import Oasis.Client.OpenAI.Hooks (RequestHook(..), emptyRequestHook, emptyClientHooks)
import Oasis.Demo.ToolCalling
  ( toolCallingMessages
  , toolCallingTools
  )
import Oasis.Model (resolveModelId)
import Oasis.Service.Amap (getWeatherText)
import Oasis.Tui.Actions.Common
  ( resolveSelectedProvider
  , startRunner
  , runInBackground
  , encodeJsonText
  , buildDebugInfo
  , selectBaseUrl
  , withMessageListHooks
  , extractAssistantContent
  , jsonRequestHeaders
  , decodeJsonText
  )
import Oasis.Tui.Render.Output (mdJsonSection, mdTextSection, mdConcat)
import Oasis.Tui.State (AppState(..), Name(..), TuiEvent(..), DebugRequestInfo(..))
import Oasis.Types
  ( Provider
  , Message(..)
  , MessageContent(..)
  , ToolCall(..)
  , ToolCallFunction(..)
  )
import Oasis.Runner.ToolCalling
  ( ToolCallingInput(..)
  , ToolCallingResult(..)
  , buildToolCallingRequest
  , runToolCallingWithRequestWithHooks
  , runToolCallingWithRequestWithHook
  )

runToolCallingAction :: EventM Name AppState ()
runToolCallingAction =
  do
    st <- get
    mResolved <- resolveSelectedProvider
    forM_ mResolved $ \(provider, apiKey) -> do
      startRunner "Running tool-calling runner..."
      let modelOverride = selectedModel st
          params = chatParams st
          useBeta = betaUrlSetting st
          chan = eventChan st
          providerName = fromMaybe "-" (selectedProvider st)
          modelId = resolveModelId provider modelOverride
          tools = toolCallingTools
          messages0 = toolCallingMessages
          reqBody0 = buildToolCallingRequest modelId params messages0 tools (Just True)
          reqJsonPreview0 = encodeRequestJsonWithFlatExtra reqBody0
          reqJsonDebug0 = encodeJsonText reqBody0
          endpoint = buildChatUrl (selectBaseUrl provider useBeta)
          info0 = buildDebugInfo providerName modelId endpoint (jsonRequestHeaders apiKey)
      progressRef <- newIORef emptyToolCallingProgress
      reqIndexRef <- newIORef (0 :: Int)
      let handler0 bodyText = do
            reqBody0' <- (decodeJsonText bodyText :: Either Text ChatCompletionRequest)
            if stream reqBody0'
              then Left "tool-calling runner requires stream=false"
              else Right $ do
                let reqMessages0 = messages reqBody0'
                    hooks0 = withMessageListHooks chan reqMessages0 emptyClientHooks
                    reqHook = buildToolCallingRequestHook st info0 progressRef reqIndexRef
                result <- runToolCallingWithRequestWithHook reqHook hooks0 provider apiKey reqBody0' (executeToolCallWithProgress progressRef chan) useBeta
                pure (renderToolCallingResult result)
      runInBackground st $ do
        case handler0 reqJsonDebug0 of
          Left err -> pure (ToolCallingCompleted "Tool-calling runner failed." (mdTextSection "Error" err))
          Right action -> action

executeToolCall :: ToolCall -> IO (Either Text Text)
executeToolCall ToolCall{function = ToolCallFunction{name, arguments}} =
  case name of
    "get_current_time" -> do
      now <- getZonedTime
      let formatted = T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now)
      pure (Right ("当前时间：" <> formatted <> "。"))
    "get_current_weather" -> do
      let argsValue = decode (BL.fromStrict (TE.encodeUtf8 arguments)) :: Maybe Value
      case argsValue >>= getLocation of
        Nothing -> pure (Left "无法获取城市参数。")
        Just loc -> do
          result <- getWeatherText loc
          case result of
            Left err -> pure (Left (loc <> "天气查询失败：" <> err))
            Right weather -> pure (Right (loc <> "今天天气是" <> weather <> "。"))
    _ -> pure (Left ("Unknown tool: " <> name))

executeToolCallWithProgress :: IORef ToolCallingProgress -> BChan TuiEvent -> ToolCall -> IO (Either Text Text)
executeToolCallWithProgress progressRef chan toolCall = do
  result <- executeToolCall toolCall
  let toolSection = case result of
        Left terr -> mdTextSection "Tool Result" ("Error: " <> terr)
        Right tmsg -> mdTextSection "Tool Result" tmsg
  modifyIORef' progressRef (\p -> p { toolSection = Just toolSection })
  output <- renderToolCallingProgress progressRef
  writeBChan chan (ToolCallingOutput output)
  pure result

getLocation :: Value -> Maybe Text
getLocation (Aeson.Object obj) =
  case KM.lookup "location" obj of
    Just (Aeson.String t) -> Just t
    _ -> Nothing
getLocation _ = Nothing

decodeChatResponse :: BL.ByteString -> Either Text ChatCompletionResponse
decodeChatResponse body =
  case eitherDecode body of
    Left err -> Left (toText err)
    Right resp -> Right resp

extractToolCallFromResponse :: ChatCompletionResponse -> Maybe (Message, ToolCall)
extractToolCallFromResponse ChatCompletionResponse{choices} =
  case choices of
    (ChatChoice{message = Just msg@Message{tool_calls = Just (tc:_)}}:_) -> Just (msg, tc)
    _ -> Nothing

renderToolCallingResult :: Either Text ToolCallingResult -> TuiEvent
renderToolCallingResult = \case
  Left err -> ToolCallingCompleted "Tool-calling runner failed." (mdTextSection "Error" err)
  Right result ->
    let output = case result of
          ToolCallingNoToolCall content -> mdConcat
            [ mdTextSection "First Response" content
            , mdTextSection "Tool Result" "No tool call returned."
            , mdTextSection "Final Assistant" "Skipped because no tool call was returned."
            ]
          ToolCallingToolError toolCallJson terr -> mdConcat
            [ mdJsonSection "First Response (Tool Call)" toolCallJson
            , mdTextSection "Tool Result" ("Error: " <> terr)
            , mdTextSection "Final Assistant" "Skipped due to tool error."
            ]
          ToolCallingSecondError toolCallJson toolMsgText err -> mdConcat
            [ mdJsonSection "First Response (Tool Call)" toolCallJson
            , mdTextSection "Tool Result" toolMsgText
            , mdTextSection "Final Assistant" ("Error: " <> err)
            ]
          ToolCallingSuccess toolCallJson toolMsgText content -> mdConcat
            [ mdJsonSection "First Response (Tool Call)" toolCallJson
            , mdTextSection "Tool Result" toolMsgText
            , mdTextSection "Final Assistant" content
            ]
    in ToolCallingCompleted "Tool-calling runner completed." output

buildToolCallingRequestHook :: AppState -> DebugRequestInfo -> IORef ToolCallingProgress -> IORef Int -> RequestHook
buildToolCallingRequestHook st info progressRef reqIndexRef = RequestHook
  { beforeRequest = \req -> do
      let finalize reqFinal = do
            writeBChan (eventChan st) (MessageListSynced (messages reqFinal))
            modifyIORef' reqIndexRef (+1)
            pure (Right reqFinal)
      if not (debugEnabled st)
        then finalize req
        else do
          responseVar <- newEmptyMVar
          let handler payload =
                if T.null (T.strip payload)
                  then Right $ do
                    putMVar responseVar (Left "Debug request cancelled.")
                    pure DebugNoOp
                  else case decodeJsonText payload :: Either Text ChatCompletionRequest of
                    Left err -> Left err
                    Right req' -> Right $ do
                      putMVar responseVar (Right req')
                      pure DebugNoOp
          writeBChan (eventChan st) (DebugRequestOpen info (encodeJsonText req) handler (activeList st))
          takeMVar responseVar >>= \case
            Left err -> pure (Left err)
            Right reqFinal -> finalize reqFinal
  , afterResponse = \req resp -> do
      let hasTool = any (\m -> role m == "tool") (messages req)
      case resp of
        Left err -> do
          let errSection = mdTextSection "Error" (renderClientError err)
          if hasTool
            then modifyIORef' progressRef (\p -> p { finalSection = Just errSection })
            else modifyIORef' progressRef (\p -> p { firstSection = Just errSection })
        Right body ->
          case decodeChatResponse body of
            Left perr -> do
              let errSection = mdTextSection "Error" perr
              if hasTool
                then modifyIORef' progressRef (\p -> p { finalSection = Just errSection })
                else modifyIORef' progressRef (\p -> p { firstSection = Just errSection })
            Right response ->
              if hasTool
                then do
                  let finalText = fromMaybe "No assistant message returned." (extractAssistantContent response)
                      finalSection = mdTextSection "Final Assistant" finalText
                  modifyIORef' progressRef (\p -> p { finalSection = Just finalSection })
                  let reqMessages = messages req
                      updated = reqMessages <> [Message "assistant" (ContentText finalText) Nothing Nothing Nothing Nothing Nothing]
                  writeBChan (eventChan st) (MessageListSynced updated)
                else do
                  case extractToolCallFromResponse response of
                    Nothing -> do
                      let firstText = fromMaybe "No assistant message returned." (extractAssistantContent response)
                          firstSection = mdTextSection "First Response" firstText
                      modifyIORef' progressRef (\p -> p { firstSection = Just firstSection })
                    Just (_, toolCall) -> do
                      let firstSection = mdJsonSection "First Response (Tool Call)" (encodeJsonText toolCall)
                      modifyIORef' progressRef (\p -> p { firstSection = Just firstSection })
      output <- renderToolCallingProgress progressRef
      writeBChan (eventChan st) (ToolCallingOutput output)
  }

data ToolCallingProgress = ToolCallingProgress
  { firstSection :: Maybe Text
  , toolSection :: Maybe Text
  , finalSection :: Maybe Text
  }

emptyToolCallingProgress :: ToolCallingProgress
emptyToolCallingProgress = ToolCallingProgress Nothing Nothing Nothing

renderToolCallingProgress :: IORef ToolCallingProgress -> IO Text
renderToolCallingProgress ref = do
  ToolCallingProgress{firstSection, toolSection, finalSection} <- readIORef ref
  let pieces = catMaybes [firstSection, toolSection, finalSection]
  pure (mdConcat pieces)
