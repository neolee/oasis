module Oasis.Runner.Basic
  ( BasicResult(..)
  , runBasic
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI
import Data.Aeson (encode, decode)
import Oasis.Runner.Common (resolveModelId, buildUserMessages)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL

data BasicResult = BasicResult
  { requestJson  :: Text
  , responseJson :: Text
  , response     :: Maybe ChatCompletionResponse
  } deriving (Show, Eq)

runBasic :: Provider -> Text -> Maybe Text -> Text -> IO (Either Text BasicResult)
runBasic provider apiKey modelOverride prompt = do
  let modelId = resolveModelId provider modelOverride
      messages = buildUserMessages prompt
      reqBody = ChatCompletionRequest
        { model = modelId
        , messages = messages
        , temperature = Nothing
        , stream = False
        }
      reqJsonText = TE.decodeUtf8Lenient (BL.toStrict (encode reqBody))
  resp <- sendChatCompletionRaw provider apiKey reqBody
  case resp of
    Left err -> pure (Left err)
    Right body ->
      let respText = TE.decodeUtf8Lenient (BL.toStrict body)
          decoded = decode body
      in pure $ Right (BasicResult reqJsonText respText decoded)
