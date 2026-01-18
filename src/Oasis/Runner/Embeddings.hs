module Oasis.Runner.Embeddings
  ( EmbeddingResult(..)
  , EmbeddingParams(..)
  , emptyEmbeddingParams
  , parseEmbeddingParams
  , runEmbeddings
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

data EmbeddingResult = EmbeddingResult
  { requestJson  :: Text
  , responseJson :: Text
  , response     :: Maybe EmbeddingResponse
  } deriving (Show, Eq)

data EmbeddingParams = EmbeddingParams
  { paramEncodingFormat :: Maybe Text
  , paramDimensions     :: Maybe Int
  , paramUser           :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON EmbeddingParams where
  parseJSON = withObject "EmbeddingParams" $ \o -> do
    let get name alt = o .:? name <|> o .:? alt
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
parseEmbeddingParams = \case
  Nothing -> Right emptyEmbeddingParams
  Just raw
    | T.null (T.strip raw) -> Right emptyEmbeddingParams
    | otherwise ->
        case Aeson.eitherDecode (BL.fromStrict (TE.encodeUtf8 raw)) of
          Left err -> Left ("Invalid --extra-args JSON: " <> toText err)
          Right params -> Right params

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
      reqJsonText = TE.decodeUtf8Lenient (BL.toStrict (encode reqBody))
  resp <- sendEmbeddingsRaw provider apiKey reqBody
  case resp of
    Left err -> pure (Left (renderClientError err))
    Right body ->
      let respText = TE.decodeUtf8Lenient (BL.toStrict body)
          decoded = decode body
      in pure $ Right (EmbeddingResult reqJsonText respText decoded)
