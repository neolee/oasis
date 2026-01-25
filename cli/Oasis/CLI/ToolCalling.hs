module Oasis.CLI.ToolCalling
  ( executeDemoToolCall
  ) where

import Relude
import Oasis.Service.Amap (getWeatherText)
import Oasis.Types (ToolCall(..), ToolCallFunction(..))
import Data.Aeson (Value, decode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.Time (getZonedTime, formatTime, defaultTimeLocale)

executeDemoToolCall :: ToolCall -> IO (Either Text Text)
executeDemoToolCall ToolCall{function = ToolCallFunction{name, arguments}} =
  case name of
    "get_current_time" -> Right <$> getCurrentTime
    "get_current_weather" -> do
      let argsValue = decode (BL.fromStrict (TE.encodeUtf8 arguments)) :: Maybe Value
      case argsValue >>= getLocation of
        Nothing -> pure (Left "无法获取城市参数。")
        Just loc -> Right <$> getCurrentWeather loc
    _ -> pure (Left ("Unknown tool: " <> name))

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
