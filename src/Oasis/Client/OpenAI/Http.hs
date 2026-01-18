{-# LANGUAGE StrictData #-}

module Oasis.Client.OpenAI.Http
  ( buildChatUrl
  , buildModelsUrl
  , buildEmbeddingsUrl
  , buildResponsesUrl
  , authHeader
  , newTlsManager
  , buildRequest
  , executeRequest
  , executeRequestWithHooks
  , clientErrorFromResponse
  , ClientHooks(..)
  , emptyClientHooks
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

data ClientHooks = ClientHooks
  { onRequest  :: Maybe (Request -> IO ())
  , onResponse :: Maybe (Status -> ResponseHeaders -> BL.ByteString -> IO ())
  , onError    :: Maybe (ClientError -> IO ())
  }

emptyClientHooks :: ClientHooks
emptyClientHooks = ClientHooks Nothing Nothing Nothing

buildChatUrl :: Text -> Text
buildChatUrl baseUrl = buildEndpointUrl baseUrl "/chat/completions"

buildModelsUrl :: Text -> Text
buildModelsUrl baseUrl = buildEndpointUrl baseUrl "/models"

buildEmbeddingsUrl :: Text -> Text
buildEmbeddingsUrl baseUrl = buildEndpointUrl baseUrl "/embeddings"

buildResponsesUrl :: Text -> Text
buildResponsesUrl baseUrl = buildEndpointUrl baseUrl "/responses"

buildEndpointUrl :: Text -> Text -> Text
buildEndpointUrl baseUrl pathSuffix =
  let trimmed = T.dropWhileEnd (== '/') baseUrl
      lower   = T.toLower trimmed
      versionSuffixes = ["/v1", "/v2", "/v3", "/v4", "/v5"]
      hasVersionSuffix = any (`T.isSuffixOf` lower) versionSuffixes
      finalSuffix = if hasVersionSuffix
                      then pathSuffix
                      else "/v1" <> pathSuffix
  in trimmed <> finalSuffix

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
  executeRequestWithHooks emptyClientHooks req manager

executeRequestWithHooks :: ClientHooks -> Request -> Manager -> IO (Either ClientError BL.ByteString)
executeRequestWithHooks hooks req manager = do
  forM_ (onRequest hooks) ($ req)
  resp <- httpLbs req manager
  let status = responseStatus resp
      headers = responseHeaders resp
      body = responseBody resp
  if isSuccessStatus status
    then do
      forM_ (onResponse hooks) (\f -> f status headers body)
      pure (Right body)
    else do
      let err = clientErrorFromResponse status headers body
      forM_ (onError hooks) ($ err)
      pure (Left err)

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
