module Oasis.Runner.GetModels
  ( GetModelsResult(..)
  , runGetModels
  ) where

import Relude
import Data.Aeson (Value, decode)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Oasis.Client.OpenAI
import Oasis.Types

data GetModelsResult = GetModelsResult
  { responseJson :: Text
  , response     :: Maybe Value
  } deriving (Show, Eq)

runGetModels :: Provider -> Text -> IO (Either Text GetModelsResult)
runGetModels provider apiKey = do
  resp <- sendModelsRaw provider apiKey
  case resp of
    Left err -> pure (Left err)
    Right body ->
      let respText = TE.decodeUtf8Lenient (BL.toStrict body)
          decoded = decode body
      in pure $ Right (GetModelsResult respText decoded)
