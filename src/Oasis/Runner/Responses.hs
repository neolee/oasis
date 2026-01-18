module Oasis.Runner.Responses
  ( ResponsesResult(..)
  , ResponsesParams(..)
  , emptyResponsesParams
  , parseResponsesParams
  , runResponses
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI
import Oasis.Runner.Common (resolveModelId)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:?), (.=), withObject, encode, decode)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as Aeson

data ResponsesResult = ResponsesResult
  { requestJson  :: Text
  , responseJson :: Text
  , response     :: Maybe ResponsesResponse
  } deriving (Show, Eq)

data ResponsesParams = ResponsesParams
  { paramTemperature    :: Maybe Double
  , paramTopP           :: Maybe Double
  , paramMaxOutputTokens :: Maybe Int
  , paramUser           :: Maybe Text
  , paramResponseFormat :: Maybe Aeson.Value
  } deriving (Show, Eq, Generic)

instance FromJSON ResponsesParams where
  parseJSON = withObject "ResponsesParams" $ \o -> do
    let get name alt = o .:? name <|> o .:? alt
    paramTemperature <- get "temperature" "temperature"
    paramTopP <- get "top_p" "topP"
    paramMaxOutputTokens <- get "max_output_tokens" "maxOutputTokens"
    paramUser <- get "user" "user"
    paramResponseFormat <- get "response_format" "responseFormat"
    pure ResponsesParams{..}

instance ToJSON ResponsesParams where
  toJSON ResponsesParams{..} = Aeson.object $ catMaybes
    [ ("temperature" .=) <$> paramTemperature
    , ("top_p" .=) <$> paramTopP
    , ("max_output_tokens" .=) <$> paramMaxOutputTokens
    , ("user" .=) <$> paramUser
    , ("response_format" .=) <$> paramResponseFormat
    ]

emptyResponsesParams :: ResponsesParams
emptyResponsesParams = ResponsesParams Nothing Nothing Nothing Nothing Nothing

parseResponsesParams :: Maybe Text -> Either Text ResponsesParams
parseResponsesParams = \case
  Nothing -> Right emptyResponsesParams
  Just raw
    | T.null (T.strip raw) -> Right emptyResponsesParams
    | otherwise ->
        case Aeson.eitherDecode (BL.fromStrict (TE.encodeUtf8 raw)) of
          Left err -> Left ("Invalid --extra-args JSON: " <> toText err)
          Right params -> Right params

runResponses :: Provider -> Text -> Maybe Text -> ResponsesParams -> Text -> IO (Either Text ResponsesResult)
runResponses provider apiKey modelOverride params inputText = do
  let modelId = resolveModelId provider modelOverride
      reqBody = ResponsesRequest
        { model = modelId
        , input = Aeson.String inputText
        , stream = Nothing
        , max_output_tokens = paramMaxOutputTokens params
        , temperature = paramTemperature params
        , top_p = paramTopP params
        , user = paramUser params
        , response_format = paramResponseFormat params
        }
      reqJsonText = TE.decodeUtf8Lenient (BL.toStrict (encode reqBody))
  resp <- sendResponsesRaw provider apiKey reqBody
  case resp of
    Left err -> pure (Left (renderClientError err))
    Right body ->
      let respText = TE.decodeUtf8Lenient (BL.toStrict body)
          decoded = decode body
      in pure $ Right (ResponsesResult reqJsonText respText decoded)
