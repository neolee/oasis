module Oasis.Service.Amap
  ( getWeatherText
  ) where

import Relude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8
import Data.Aeson (Value(..), eitherDecode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

baseUrl = "https://restapi.amap.com/v3/weather/weatherInfo"

getWeatherText :: Text -> IO (Either Text Text)
getWeatherText city = do
  apiKeyText <- toText . fromMaybe "" <$> lookupEnv "AMAP_API_KEY"
  if T.null apiKeyText
    then pure (Left "missing environment var AMAP_API_KEY")
    else do
      manager <- newManager tlsManagerSettings
      initReq <- parseRequest baseUrl
      let query =
            [ ("key", Just (TE.encodeUtf8 apiKeyText))
            , ("city", Just (TE.encodeUtf8 city))
            ]
          req = setQueryString query initReq
      resp <- httpLbs req manager
      if statusCode (responseStatus resp) /= 200
        then pure (Left ("AMAP request failed, status: " <> show (statusCode (responseStatus resp))))
        else do
          let body = responseBody resp
          case eitherDecode body of
            Left err -> pure (Left ("Failed to decode AMAP response: " <> toText err))
            Right val ->
              case extractWeather val of
                Nothing ->
                  let raw = TE.decodeUtf8Lenient (BL.toStrict body)
                  in pure (Left ("Failed to parse weather info. Raw: " <> raw))
                Just w -> pure (Right w)

extractWeather :: Value -> Maybe Text
extractWeather (Object obj) = do
  statusVal <- KM.lookup "status" obj
  case statusVal of
    String "1" -> pure ()
    _ -> Nothing
  livesVal <- KM.lookup "lives" obj
  case livesVal of
    Array arr | not (V.null arr) ->
      case V.head arr of
        Object liveObj ->
          case KM.lookup "weather" liveObj of
            Just (String w) -> Just w
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing
extractWeather _ = Nothing
