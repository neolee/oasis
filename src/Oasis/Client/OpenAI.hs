{-# LANGUAGE StrictData #-}

module Oasis.Client.OpenAI
  ( ChatCompletionRequest(..)
  , ChatCompletionResponse(..)
  , ChatChoice(..)
  , Usage(..)
  , sendChatCompletion
  , buildChatUrl
  ) where

import Relude
import Oasis.Types
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (HeaderName, hAccept, hAuthorization, hContentType)

data ChatCompletionRequest = ChatCompletionRequest
  { model       :: Text
  , messages    :: [Message]
  , temperature :: Maybe Double
  , stream      :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON ChatCompletionRequest where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

instance FromJSON ChatCompletionRequest where
  parseJSON = genericParseJSON defaultOptions

data ChatCompletionResponse = ChatCompletionResponse
  { id      :: Maybe Text
  , object  :: Maybe Text
  , created :: Maybe Int
  , model   :: Maybe Text
  , choices :: [ChatChoice]
  , usage   :: Maybe Usage
  } deriving (Show, Eq, Generic)

instance FromJSON ChatCompletionResponse where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ChatCompletionResponse where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

data ChatChoice = ChatChoice
  { index         :: Maybe Int
  , message       :: Maybe Message
  , finish_reason :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON ChatChoice where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ChatChoice where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

data Usage = Usage
  { prompt_tokens     :: Maybe Int
  , completion_tokens :: Maybe Int
  , total_tokens      :: Maybe Int
  } deriving (Show, Eq, Generic)

instance FromJSON Usage where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Usage where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

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

sendChatCompletion :: Provider -> Text -> Text -> [Message] -> IO (Either Text ChatCompletionResponse)
sendChatCompletion provider apiKey modelId msgs = do
  manager <- newManager tlsManagerSettings
  let url = buildChatUrl (base_url provider)
      reqBody = ChatCompletionRequest
        { model = modelId
        , messages = msgs
        , temperature = Nothing
        , stream = False
        }
  initReq <- parseRequest (toString url)
  let headers =
        [ (hContentType, "application/json")
        , (hAccept, "application/json")
        ] <> authHeader apiKey
      req = initReq
        { method = "POST"
        , requestBody = RequestBodyLBS (encode reqBody)
        , requestHeaders = headers
        }
  resp <- httpLbs req manager
  let body = responseBody resp
  case eitherDecode body of
    Left err ->
      let raw = TE.decodeUtf8Lenient (BL.toStrict body)
      in pure $ Left ("Failed to decode response: " <> toText err <> "\nRaw: " <> raw)
    Right val -> pure $ Right val

authHeader :: Text -> [(HeaderName, BS8.ByteString)]
authHeader apiKey
  | T.null apiKey = []
  | otherwise    = [(hAuthorization, "Bearer " <> TE.encodeUtf8 apiKey)]
