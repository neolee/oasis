module Oasis.Runner.Embeddings
  ( EmbeddingResult
  , EmbeddingParams(..)
  , emptyEmbeddingParams
  , parseEmbeddingParams
  , runEmbeddings
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI
import Oasis.Runner.Common (resolveModelId, parseExtraArgs)
import Oasis.Runner.Result (RunnerResult(..), encodeRequestJson, buildRunnerResult)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:?), (.=), withObject)
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key

type EmbeddingResult = RunnerResult EmbeddingResponse

data EmbeddingParams = EmbeddingParams
  { paramEncodingFormat :: Maybe Text
  , paramDimensions     :: Maybe Int
  , paramUser           :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON EmbeddingParams where
  parseJSON = withObject "EmbeddingParams" $ \o -> do
    let get :: FromJSON a => Text -> Text -> Parser (Maybe a)
        get name alt = o .:? Key.fromText name <|> o .:? Key.fromText alt
    paramEncodingFormat <- get "encoding_format" "encodingFormat"
    paramDimensions <- get "dimensions" "dimensions"
    paramUser <- get "user" "user"
    pure EmbeddingParams{..}

instance ToJSON EmbeddingParams where
  toJSON EmbeddingParams{..} = Aeson.object $ catMaybes
    [ ("encoding_format" .=) <$> paramEncodingFormat
    , ("dimensions" .=) <$> paramDimensions
    , ("user" .=) <$> paramUser
    ]

emptyEmbeddingParams :: EmbeddingParams
emptyEmbeddingParams = EmbeddingParams Nothing Nothing Nothing

parseEmbeddingParams :: Maybe Text -> Either Text EmbeddingParams
parseEmbeddingParams = parseExtraArgs "Embeddings" emptyEmbeddingParams

runEmbeddings :: Provider -> Text -> Maybe Text -> EmbeddingParams -> Text -> IO (Either Text EmbeddingResult)
runEmbeddings provider apiKey modelOverride params inputText = do
  let modelId = resolveModelId provider modelOverride
      reqBody = EmbeddingRequest
        { model = modelId
        , input = Aeson.String inputText
        , encoding_format = paramEncodingFormat params
        , dimensions = paramDimensions params
        , user = paramUser params
        }
      reqJsonText = encodeRequestJson reqBody
  resp <- sendEmbeddingsRaw provider apiKey reqBody
  pure (buildRunnerResult reqJsonText resp)
