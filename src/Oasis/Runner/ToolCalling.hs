module Oasis.Runner.ToolCalling
  ( runToolCalling
  ) where

import Relude
import Oasis.Types
import qualified Oasis.Types as OT
import Oasis.Client.OpenAI
import qualified Oasis.Chat.Message as Msg
import Oasis.Model (resolveModelId)
import Oasis.Client.OpenAI.Param (ChatParams)
import Oasis.Client.OpenAI.Context (extractAssistantContent, extractToolCall)
import Oasis.Runner.Result (parseRawResponseStrict)
import Oasis.Service.Amap (getWeatherText)
import Data.Aeson (Value, decode, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.Time (getZonedTime, formatTime, defaultTimeLocale)

runToolCalling :: Provider -> Text -> Maybe Text -> ChatParams -> Bool -> IO (Either Text ())
runToolCalling provider apiKey modelOverride params useBeta = do
  let modelId = resolveModelId provider modelOverride
      tools = buildTools
      systemMessage = T.unlines
        [ "你是一个很有帮助的助手。"
        , "如果用户提问关于天气的问题，请调用 ‘get_current_weather’ 函数；"
        , "如果用户提问关于时间的问题，请调用 ‘get_current_time’ 函数。"
        , "请以友好的语气回答问题。"
        ]
      messages0 =
        [ Msg.systemMessage systemMessage
        , Msg.userMessage "上海天气"
        ]
  let reqBase0 = (defaultChatRequest modelId messages0)
        { tools = Just tools
        , parallel_tool_calls = Just True
        }
  firstResp <- requestChat provider apiKey params reqBase0 useBeta
  case parseRawResponseStrict firstResp of
    Left err -> pure (Left err)
    Right (_, response) -> do
      case extractToolCall response of
        Nothing -> do
          case extractAssistantContent response of
            Nothing -> pure (Left "No assistant message returned.")
            Just content -> do
              putTextLn content
              pure (Right ())
        Just (assistantMessage, toolCall) -> do
          result <- executeToolCall toolCall
          let toolMsg = Msg.toolMessage (OT.id toolCall) result
              messages1 = messages0 <> [assistantMessage, toolMsg]
              reqBase2 = (defaultChatRequest modelId messages1)
                { tools = Just tools
                }
          secondResp <- requestChat provider apiKey params reqBase2 useBeta
          case parseRawResponseStrict secondResp of
            Left err -> pure (Left err)
            Right (_, response2) ->
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
