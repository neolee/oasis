module Oasis.Runner.Responses
  ( ResponsesResult
  , ResponsesParams(..)
  , emptyResponsesParams
  , parseResponsesParams
  , runResponses
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI
import Oasis.Runner.Common (resolveModelId, parseExtraArgs)
import Oasis.Runner.Result (RunnerResult(..), encodeRequestJson, buildRunnerResult)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:?), (.=), withObject)
import qualified Data.Text as T
import qualified Data.Aeson as Aeson

type ResponsesResult = RunnerResult ResponsesResponse

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
parseResponsesParams = parseExtraArgs "Responses" emptyResponsesParams

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
      reqJsonText = encodeRequestJson reqBody
  resp <- sendResponsesRaw provider apiKey reqBody
  pure (buildRunnerResult reqJsonText resp)
