{-# LANGUAGE StrictData #-}

module Oasis.Client.OpenAI.Http
  ( buildChatUrl
  , buildModelsUrl
  , authHeader
  , newTlsManager
  , buildRequest
  , jsonHeaders
  , sseHeaders
  , modelsHeaders
  ) where

import Relude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS8
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (HeaderName, hAccept, hAuthorization, hContentType)
import Network.HTTP.Types.Method (Method)

buildChatUrl :: Text -> Text
buildChatUrl baseUrl =
  let trimmed = T.dropWhileEnd (== '/') baseUrl
      lower   = T.toLower trimmed
      versionSuffixes = ["/v1", "/v2", "/v3", "/v4", "/v5"]
      hasVersionSuffix = any (`T.isSuffixOf` lower) versionSuffixes
      pathSuffix = if hasVersionSuffix
                     then "/chat/completions"
                     else "/v1/chat/completions"
  in trimmed <> pathSuffix

buildModelsUrl :: Text -> Text
buildModelsUrl baseUrl =
  let trimmed = T.dropWhileEnd (== '/') baseUrl
      lower   = T.toLower trimmed
      versionSuffixes = ["/v1", "/v2", "/v3", "/v4", "/v5"]
      hasVersionSuffix = any (`T.isSuffixOf` lower) versionSuffixes
      pathSuffix = if hasVersionSuffix
                     then "/models"
                     else "/v1/models"
  in trimmed <> pathSuffix

authHeader :: Text -> [(HeaderName, BS8.ByteString)]
authHeader apiKey
  | T.null apiKey = []
  | otherwise    = [(hAuthorization, "Bearer " <> TE.encodeUtf8 apiKey)]

newTlsManager :: IO Manager
newTlsManager = newManager tlsManagerSettings

buildRequest :: Text -> Method -> RequestBody -> [(HeaderName, BS8.ByteString)] -> IO Request
buildRequest url method reqBody headers = do
  initReq <- parseRequest (toString url)
  pure initReq
    { method = method
    , requestBody = reqBody
    , requestHeaders = headers
    }

jsonHeaders :: Text -> [(HeaderName, BS8.ByteString)]
jsonHeaders apiKey =
  [ (hContentType, "application/json")
  , (hAccept, "application/json")
  ] <> authHeader apiKey

sseHeaders :: Text -> [(HeaderName, BS8.ByteString)]
sseHeaders apiKey =
  [ (hContentType, "application/json")
  , (hAccept, "text/event-stream")
  ] <> authHeader apiKey

modelsHeaders :: Text -> [(HeaderName, BS8.ByteString)]
modelsHeaders apiKey =
  [ (hAccept, "application/json")
  ] <> authHeader apiKey
