{-# LANGUAGE StrictData #-}

module Oasis.Client.OpenAI.Http
  ( buildChatUrl
  , buildModelsUrl
  , authHeader
  , newTlsManager
  , buildRequest
  , executeRequest
  , clientErrorFromResponse
  , jsonHeaders
  , sseHeaders
  , modelsHeaders
  ) where

import Relude
import Oasis.Client.OpenAI.Types
import Data.Aeson (decode)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (HeaderName, ResponseHeaders, hAccept, hAuthorization, hContentType)
import Network.HTTP.Types.Method (Method)
import Network.HTTP.Types.Status (Status, statusCode, statusMessage)

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

executeRequest :: Request -> Manager -> IO (Either ClientError BL.ByteString)
executeRequest req manager = do
  resp <- httpLbs req manager
  let status = responseStatus resp
  if isSuccessStatus status
    then pure (Right (responseBody resp))
    else pure (Left (clientErrorFromResponse status (responseHeaders resp) (responseBody resp)))

clientErrorFromResponse :: Status -> ResponseHeaders -> BL.ByteString -> ClientError
clientErrorFromResponse status headers body =
  let rawText = TE.decodeUtf8Lenient (BL.toStrict body)
      reqId = L.lookup "x-request-id" headers <|> L.lookup "request-id" headers
      errResp = decode body
  in ClientError
      { status = statusCode status
      , statusText = TE.decodeUtf8Lenient (statusMessage status)
      , requestId = TE.decodeUtf8Lenient <$> reqId
      , errorResponse = errResp
      , rawBody = rawText
      }

isSuccessStatus :: Status -> Bool
isSuccessStatus st =
  let code = statusCode st
  in code >= 200 && code < 300

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
