module Oasis.Tui.Actions.ToolCalling
  ( runToolCallingAction
  ) where

import Relude
import Brick.Types (EventM)
import Control.Monad.State.Class (get)
import qualified Data.Aeson as Aeson
import Data.Aeson (Value, eitherDecode, decode, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getZonedTime, formatTime, defaultTimeLocale)
import Oasis.Chat.Message (userMessage, systemMessage, toolMessage)
import Oasis.Client.OpenAI
  ( ChatCompletionResponse(..)
  , ChatChoice(..)
  , ChatCompletionRequest(..)
  , defaultChatRequest
  , buildChatUrl
  , sendChatCompletionRawWithHooks
  , renderClientError
  , emptyClientHooks
  )
import Oasis.Client.OpenAI.Param (applyChatParams)
import Oasis.Model (resolveModelId)
import Oasis.Service.Amap (getWeatherText)
import Oasis.Tui.Actions.Common
  ( resolveSelectedProvider
  , startRunner
  , encodeJsonText
  , runWithDebug
  , buildDebugInfo
  , selectBaseUrl
  , withMessageListHooks
  , extractAssistantContent
  , jsonRequestHeaders
  , decodeJsonText
  )
import Oasis.Tui.Render.Output (mdJsonSection, mdTextSection, mdConcat)
import Oasis.Tui.State (AppState(..), Name(..), TuiEvent(..))
import qualified Oasis.Types as Types
import Oasis.Types
  ( Message(..)
  , messageContentText
  , Tool(..)
  , ToolFunctionSpec(..)
  , ToolCall(..)
  , ToolCallFunction(..)
  )

runToolCallingAction :: EventM Name AppState ()
runToolCallingAction = do
  st <- get
  mResolved <- resolveSelectedProvider
  forM_ mResolved $ \(provider, apiKey) -> do
    let modelOverride = selectedModel st
        params = chatParams st
        useBeta = betaUrlSetting st
        chan = eventChan st
        providerName = fromMaybe "-" (selectedProvider st)
    startRunner "Running tool-calling runner..."
    let modelId = resolveModelId provider modelOverride
        tools = buildTools
        systemMsg = T.unlines
          [ "你是一个很有帮助的助手。"
          , "如果用户提问关于天气的问题，请调用 ‘get_current_weather’ 函数；"
          , "如果用户提问关于时间的问题，请调用 ‘get_current_time’ 函数。"
          , "请以友好的语气回答问题。"
          ]
        messages0 =
          [ systemMessage systemMsg
          , userMessage "上海天气"
          ]
        reqBase0 = (defaultChatRequest modelId messages0)
          { tools = Just tools
          , parallel_tool_calls = Just True
          }
        reqBody0 = applyChatParams params reqBase0
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
              firstResp <- sendChatCompletionRawWithHooks hooks0 provider apiKey reqBody0' useBeta
              case firstResp of
                Left err ->
                  let output = mdConcat
                        [ mdTextSection "First Response" ("Error: " <> renderClientError err)
                        , mdTextSection "Tool Result" "Skipped due to request error."
                        , mdTextSection "Final Assistant" "Skipped due to request error."
                        ]
                  in pure (ToolCallingCompleted "Tool-calling runner failed." output)
                Right body ->
                  case decodeChatResponse body of
                    Left err ->
                      let output = mdConcat
                            [ mdTextSection "First Response" ("Error: " <> err)
                            , mdTextSection "Tool Result" "Skipped due to decode error."
                            , mdTextSection "Final Assistant" "Skipped due to decode error."
                            ]
                      in pure (ToolCallingCompleted "Tool-calling runner failed." output)
                    Right response0 ->
                      case extractToolCallLocal response0 of
                        Nothing ->
                          let assistantText = fromMaybe "No assistant message returned." (extractAssistantContent response0)
                              output = mdConcat
                                [ mdTextSection "First Response" assistantText
                                , mdTextSection "Tool Result" "No tool call returned."
                                , mdTextSection "Final Assistant" "Skipped because no tool call was returned."
                                ]
                          in pure (ToolCallingCompleted "Tool-calling runner completed." output)
                        Just (assistantMsg, toolCall) -> do
                          toolResult <- executeToolCall toolCall
                          let firstSection = mdJsonSection "First Response (Tool Call)" (encodeJsonText toolCall)
                              toolSection = case toolResult of
                                Left terr -> mdTextSection "Tool Result" ("Error: " <> terr)
                                Right tmsg -> mdTextSection "Tool Result" tmsg
                          case toolResult of
                            Left _ ->
                              let output = mdConcat
                                    [ firstSection
                                    , toolSection
                                    , mdTextSection "Final Assistant" "Skipped due to tool error."
                                    ]
                              in pure (ToolCallingCompleted "Tool-calling runner failed." output)
                            Right toolMsgText -> do
                              let toolMsg = toolMessage (Types.id toolCall) toolMsgText
                                  messages1 = messages0 <> [assistantMsg, toolMsg]
                                  reqBase1 = (defaultChatRequest modelId messages1)
                                    { tools = Just tools
                                    }
                                  reqBody1 = applyChatParams params reqBase1
                                  reqJson1 = encodeJsonText reqBody1
                                  info1 = buildDebugInfo providerName modelId endpoint (jsonRequestHeaders apiKey)
                                  handler1 bodyText1 = do
                                    reqBody1' <- (decodeJsonText bodyText1 :: Either Text ChatCompletionRequest)
                                    if stream reqBody1'
                                      then Left "tool-calling runner requires stream=false"
                                      else Right $ do
                                        let reqMessages1 = messages reqBody1'
                                            hooks1 = withMessageListHooks chan reqMessages1 emptyClientHooks
                                            toolSection' =
                                              case lastToolContent reqMessages1 of
                                                Nothing -> toolSection
                                                Just tmsg -> mdTextSection "Tool Result" tmsg
                                        secondResp <- sendChatCompletionRawWithHooks hooks1 provider apiKey reqBody1' useBeta
                                        case secondResp of
                                          Left err ->
                                            let output = mdConcat
                                                  [ firstSection
                                                  , toolSection'
                                                  , mdTextSection "Final Assistant" ("Error: " <> renderClientError err)
                                                  ]
                                            in pure (ToolCallingCompleted "Tool-calling runner failed." output)
                                          Right body2 ->
                                            case decodeChatResponse body2 of
                                              Left err ->
                                                let output = mdConcat
                                                      [ firstSection
                                                      , toolSection'
                                                      , mdTextSection "Final Assistant" ("Error: " <> err)
                                                      ]
                                                in pure (ToolCallingCompleted "Tool-calling runner failed." output)
                                              Right response2 ->
                                                let finalText = fromMaybe "No assistant message returned." (extractAssistantContent response2)
                                                    output = mdConcat
                                                      [ firstSection
                                                      , toolSection'
                                                      , mdTextSection "Final Assistant" finalText
                                                      ]
                                                in pure (ToolCallingCompleted "Tool-calling runner completed." output)
                              if debugEnabled st
                                then pure (DebugRequestOpen info1 reqJson1 handler1 (activeList st))
                                else case handler1 reqJson1 of
                                  Left err ->
                                    let output = mdConcat
                                          [ firstSection
                                          , toolSection
                                          , mdTextSection "Final Assistant" ("Error: " <> err)
                                          ]
                                    in pure (ToolCallingCompleted "Tool-calling runner failed." output)
                                  Right action -> action
    runWithDebug info0 reqJson0 handler0

decodeChatResponse :: BL.ByteString -> Either Text ChatCompletionResponse
decodeChatResponse body =
  case eitherDecode body of
    Left err -> Left (toText err)
    Right resp -> Right resp

extractToolCallLocal :: ChatCompletionResponse -> Maybe (Message, ToolCall)
extractToolCallLocal ChatCompletionResponse{choices} =
  case choices of
    (ChatChoice{message = Just msg@Message{tool_calls = Just (tc:_)}}:_) -> Just (msg, tc)
    _ -> Nothing

buildTools :: [Tool]
buildTools =
  [ Tool
      { type_ = "function"
      , function = ToolFunctionSpec
          { name = "get_current_time"
          , description = Just "当你想知道现在的时间时非常有用。"
          , parameters = Aeson.object
              [ "type" .= ("object" :: Text)
              , "properties" .= Aeson.object []
              , "required" .= ([] :: [Text])
              ]
          }
      }
  , Tool
      { type_ = "function"
      , function = ToolFunctionSpec
          { name = "get_current_weather"
          , description = Just "当你想查询指定城市的天气时非常有用。"
          , parameters = Aeson.object
              [ "type" .= ("object" :: Text)
              , "properties" .= Aeson.object
                  [ "location" .= Aeson.object
                      [ "type" .= ("string" :: Text)
                      , "description" .= ("城市或县区，比如北京市、杭州市、余杭区等。" :: Text)
                      ]
                  ]
              , "required" .= (["location"] :: [Text])
              ]
          }
      }
  ]

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

lastToolContent :: [Message] -> Maybe Text
lastToolContent msgs =
  case reverse [messageContentText (content m) | m <- msgs, role m == "tool"] of
    (t:_) | not (T.null (T.strip t)) -> Just t
    _ -> Nothing
