module Oasis.Runner.Common
  ( selectModelId
  , resolveModelId
  , buildUserMessages
  , ChatParams(..)
  , emptyChatParams
  , parseChatParams
  , applyChatParams
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI (ChatCompletionRequest(..))
import Data.Aeson (FromJSON(..), ToJSON(..), (.:?), (.=), object, withObject)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as Aeson

selectModelId :: Provider -> Text
selectModelId Provider{..} =
  case fmap T.toLower default_model_type of
    Just "reasoner" | not (T.null reasoner_model_id) -> reasoner_model_id
    Just "coder"    | not (T.null coder_model_id)    -> coder_model_id
    _                                             -> chat_model_id

resolveModelId :: Provider -> Maybe Text -> Text
resolveModelId provider = \case
  Just m
    | let trimmed = T.strip m
    , not (T.null trimmed)
    , T.toLower trimmed /= "default"
    , trimmed /= "-" -> trimmed
  _ -> selectModelId provider

buildUserMessages :: Text -> [Message]
buildUserMessages prompt = [Message "user" prompt Nothing Nothing]

data ChatParams = ChatParams
  { paramTemperature :: Maybe Double
  , paramTopP :: Maybe Double
  , paramMaxCompletionTokens :: Maybe Int
  , paramStop :: Maybe StopParam
  , paramPresencePenalty :: Maybe Double
  , paramFrequencyPenalty :: Maybe Double
  , paramSeed :: Maybe Int
  , paramLogitBias :: Maybe Aeson.Value
  , paramUser :: Maybe Text
  , paramServiceTier :: Maybe Text
  , paramReasoningEffort :: Maybe Text
  , paramStreamOptions :: Maybe Aeson.Value
  } deriving (Show, Eq, Generic)

instance FromJSON ChatParams where
  parseJSON = withObject "ChatParams" $ \o -> do
    let get name alt = o .:? name <|> o .:? alt
    paramTemperature <- get "temperature" "temperature"
    paramTopP <- get "top_p" "topP"
    paramMaxCompletionTokens <- get "max_completion_tokens" "maxCompletionTokens"
    paramStop <- get "stop" "stop"
    paramPresencePenalty <- get "presence_penalty" "presencePenalty"
    paramFrequencyPenalty <- get "frequency_penalty" "frequencyPenalty"
    paramSeed <- get "seed" "seed"
    paramLogitBias <- get "logit_bias" "logitBias"
    paramUser <- get "user" "user"
    paramServiceTier <- get "service_tier" "serviceTier"
    paramReasoningEffort <- get "reasoning_effort" "reasoningEffort"
    paramStreamOptions <- get "stream_options" "streamOptions"
    pure ChatParams{..}

instance ToJSON ChatParams where
  toJSON ChatParams{..} = object $ catMaybes
    [ ("temperature" .=) <$> paramTemperature
    , ("top_p" .=) <$> paramTopP
    , ("max_completion_tokens" .=) <$> paramMaxCompletionTokens
    , ("stop" .=) <$> paramStop
    , ("presence_penalty" .=) <$> paramPresencePenalty
    , ("frequency_penalty" .=) <$> paramFrequencyPenalty
    , ("seed" .=) <$> paramSeed
    , ("logit_bias" .=) <$> paramLogitBias
    , ("user" .=) <$> paramUser
    , ("service_tier" .=) <$> paramServiceTier
    , ("reasoning_effort" .=) <$> paramReasoningEffort
    , ("stream_options" .=) <$> paramStreamOptions
    ]

emptyChatParams :: ChatParams
emptyChatParams = ChatParams Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

parseChatParams :: Maybe Text -> Either Text ChatParams
parseChatParams = \case
  Nothing -> Right emptyChatParams
  Just raw
    | T.null (T.strip raw) -> Right emptyChatParams
    | otherwise ->
        case Aeson.eitherDecode (BL.fromStrict (TE.encodeUtf8 raw)) of
          Left err -> Left ("Invalid --extra-args JSON: " <> toText err)
          Right params -> Right params

applyChatParams :: ChatParams -> ChatCompletionRequest -> ChatCompletionRequest
applyChatParams ChatParams{..} req =
  req
    { temperature = paramTemperature <|> temperature req
    , top_p = paramTopP <|> top_p req
    , max_completion_tokens = paramMaxCompletionTokens <|> max_completion_tokens req
    , stop = paramStop <|> stop req
    , presence_penalty = paramPresencePenalty <|> presence_penalty req
    , frequency_penalty = paramFrequencyPenalty <|> frequency_penalty req
    , seed = paramSeed <|> seed req
    , logit_bias = paramLogitBias <|> logit_bias req
    , user = paramUser <|> user req
    , service_tier = paramServiceTier <|> service_tier req
    , reasoning_effort = paramReasoningEffort <|> reasoning_effort req
    , stream_options = paramStreamOptions <|> stream_options req
    }