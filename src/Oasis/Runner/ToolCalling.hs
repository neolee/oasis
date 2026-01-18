module Oasis.Runner.ToolCalling
  ( runToolCalling
  ) where

import Relude
import Oasis.Types
import qualified Oasis.Types as OT
import Oasis.Client.OpenAI
import Oasis.Runner.Common (resolveModelId, ChatParams, applyChatParams)
import Oasis.Service.Amap (getWeatherText)
import Data.Aeson (Value, decode, eitherDecode, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.Time (getZonedTime, formatTime, defaultTimeLocale)

runToolCalling :: Provider -> Text -> Maybe Text -> ChatParams -> IO (Either Text ())
runToolCalling provider apiKey modelOverride params = do
  let modelId = resolveModelId provider modelOverride
      tools = buildTools
      systemMessage = T.unlines
        [ "你是一个很有帮助的助手。"
        , "如果用户提问关于天气的问题，请调用 ‘get_current_weather’ 函数；"
        , "如果用户提问关于时间的问题，请调用 ‘get_current_time’ 函数。"
        , "请以友好的语气回答问题。"
        ]
      messages0 =
        [ Message "system" systemMessage Nothing Nothing
        , Message "user" "上海天气" Nothing Nothing
        ]
      reqBase = (defaultChatRequest modelId messages0)
        { tools = Just tools
        , parallel_tool_calls = Just True
        }
      reqBody = applyChatParams params reqBase
  firstResp <- sendChatCompletionRaw provider apiKey reqBody
  case firstResp of
    Left err -> pure (Left (renderClientError err))
    Right body ->
      case eitherDecode body of
        Left err ->
          let raw = TE.decodeUtf8Lenient (BL.toStrict body)
          in pure (Left ("Failed to decode response: " <> toText err <> "\nRaw: " <> raw))
        Right response -> do
          case extractToolCall response of
            Nothing -> do
              case extractAssistantContent response of
                Nothing -> pure (Left "No assistant message returned.")
                Just content -> do
                  putTextLn content
                  pure (Right ())
            Just (assistantMessage, toolCall) -> do
              result <- executeToolCall toolCall
              let toolMsg = Message "tool" result (Just (OT.id toolCall)) Nothing
                  messages1 = messages0 <> [assistantMessage, toolMsg]
                  reqBase2 = (defaultChatRequest modelId messages1)
                    { tools = Just tools
                    }
                  reqBody2 = applyChatParams params reqBase2
              secondResp <- sendChatCompletionRaw provider apiKey reqBody2
              case secondResp of
                Left err -> pure (Left (renderClientError err))
                Right body2 ->
                  case eitherDecode body2 of
                    Left err ->
                      let raw = TE.decodeUtf8Lenient (BL.toStrict body2)
                      in pure (Left ("Failed to decode response: " <> toText err <> "\nRaw: " <> raw))
                    Right response2 ->
                      case extractAssistantContent response2 of
                        Nothing -> pure (Left "No assistant message returned.")
                        Just content -> do
                          putTextLn content
                          pure (Right ())

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

extractToolCall :: ChatCompletionResponse -> Maybe (Message, ToolCall)
extractToolCall ChatCompletionResponse{choices} =
  case choices of
    (ChatChoice{message = Just msg@Message{tool_calls = Just (tc:_)}}:_) -> Just (msg, tc)
    _ -> Nothing

extractAssistantContent :: ChatCompletionResponse -> Maybe Text
extractAssistantContent ChatCompletionResponse{choices} =
  case choices of
    (ChatChoice{message = Just Message{content}}:_) -> Just content
    _ -> Nothing

executeToolCall :: ToolCall -> IO Text
executeToolCall ToolCall{function = ToolCallFunction{name, arguments}} =
  case name of
    "get_current_time" -> getCurrentTime
    "get_current_weather" -> do
      let argsValue = decode (BL.fromStrict (TE.encodeUtf8 arguments)) :: Maybe Value
      case argsValue >>= getLocation of
        Nothing -> pure "无法获取城市参数。"
        Just loc -> getCurrentWeather loc
    _ -> pure ("Unknown tool: " <> name)

getLocation :: Value -> Maybe Text
getLocation (Aeson.Object obj) =
  case KM.lookup "location" obj of
    Just (Aeson.String t) -> Just t
    _ -> Nothing
getLocation _ = Nothing

getCurrentWeather :: Text -> IO Text
getCurrentWeather location = do
  result <- getWeatherText location
  case result of
    Left err -> pure (location <> "天气查询失败：" <> err)
    Right weather -> pure (location <> "今天天气是" <> weather <> "。")

getCurrentTime :: IO Text
getCurrentTime = do
  now <- getZonedTime
  let formatted = T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now)
  pure ("当前时间：" <> formatted <> "。")
