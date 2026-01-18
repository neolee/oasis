module Oasis.Runner.Basic
  ( BasicResult(..)
  , runBasic
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI
import Data.Aeson (encode, decode)
import Oasis.Runner.Common (resolveModelId, buildUserMessages, ChatParams, applyChatParams)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL

data BasicResult = BasicResult
  { requestJson  :: Text
  , responseJson :: Text
  , response     :: Maybe ChatCompletionResponse
  } deriving (Show, Eq)

runBasic :: Provider -> Text -> Maybe Text -> ChatParams -> Text -> IO (Either Text BasicResult)
runBasic provider apiKey modelOverride params prompt = do
  let modelId = resolveModelId provider modelOverride
      messages = buildUserMessages prompt
      reqBase = ChatCompletionRequest
        { model = modelId
        , messages = messages
        , temperature = Nothing
        , top_p = Nothing
        , max_completion_tokens = Nothing
        , stop = Nothing
        , presence_penalty = Nothing
        , frequency_penalty = Nothing
        , seed = Nothing
        , logit_bias = Nothing
        , user = Nothing
        , service_tier = Nothing
        , reasoning_effort = Nothing
        , stream_options = Nothing
        , stream = False
        , response_format = Nothing
        , tools = Nothing
        , tool_choice = Nothing
        , parallel_tool_calls = Nothing
        }
      reqBody = applyChatParams params reqBase
      reqJsonText = TE.decodeUtf8Lenient (BL.toStrict (encode reqBody))
  resp <- sendChatCompletionRaw provider apiKey reqBody
  case resp of
    Left err -> pure (Left (renderClientError err))
    Right body ->
      let respText = TE.decodeUtf8Lenient (BL.toStrict body)
          decoded = decode body
      in pure $ Right (BasicResult reqJsonText respText decoded)
