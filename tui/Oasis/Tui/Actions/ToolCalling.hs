module Oasis.Tui.Actions.ToolCalling
  ( runToolCallingAction
  ) where

import Relude
import Brick.Types (EventM)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson (Value, decode)
import Data.Time (getZonedTime, formatTime, defaultTimeLocale)
import Oasis.Client.OpenAI
  ( ChatCompletionRequest(..)
  , buildChatUrl
  , emptyClientHooks
  )
import Oasis.Demo.ToolCalling
  ( toolCallingMessages
  , toolCallingTools
  )
import Oasis.Model (resolveModelId)
import Oasis.Service.Amap (getWeatherText)
import Oasis.Tui.Actions.Common
  ( runProviderAction
  , encodeJsonText
  , buildDebugInfo
  , selectBaseUrl
  , withMessageListHooks
  , extractAssistantContent
  , jsonRequestHeaders
  , decodeJsonText
  )
import Oasis.Tui.Render.Output (mdJsonSection, mdTextSection, mdConcat)
import Oasis.Tui.State (AppState(..), Name(..), TuiEvent(..))
import Oasis.Types
  ( Provider
  , ToolCall(..)
  , ToolCallFunction(..)
  )
import Oasis.Runner.ToolCalling
  ( ToolCallingInput(..)
  , ToolCallingResult(..)
  , buildToolCallingRequest
  , runToolCallingWithRequestWithHooks
  )

runToolCallingAction :: EventM Name AppState ()
runToolCallingAction =
  runProviderAction "Running tool-calling runner..." $ \st provider apiKey -> do
    let modelOverride = selectedModel st
        params = chatParams st
        useBeta = betaUrlSetting st
        chan = eventChan st
        providerName = fromMaybe "-" (selectedProvider st)
        modelId = resolveModelId provider modelOverride
        tools = toolCallingTools
        messages0 = toolCallingMessages
        reqBody0 = buildToolCallingRequest modelId params messages0 tools (Just True)
        reqJson0 = encodeJsonText reqBody0
        endpoint = buildChatUrl (selectBaseUrl provider useBeta)
        info0 = buildDebugInfo providerName modelId endpoint (jsonRequestHeaders apiKey)
        handler0 bodyText = do
          reqBody0' <- (decodeJsonText bodyText :: Either Text ChatCompletionRequest)
          if stream reqBody0'
            then Left "tool-calling runner requires stream=false"
            else Right $ do
              let reqMessages0 = messages reqBody0'
                  hooks0 = withMessageListHooks chan reqMessages0 emptyClientHooks
              result <- runToolCallingWithRequestWithHooks hooks0 provider apiKey reqBody0' executeToolCall useBeta
              pure (renderToolCallingResult result)
    pure (info0, reqJson0, handler0)


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

getLocation :: Value -> Maybe Text
getLocation (Aeson.Object obj) =
  case KM.lookup "location" obj of
    Just (Aeson.String t) -> Just t
    _ -> Nothing
getLocation _ = Nothing

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
