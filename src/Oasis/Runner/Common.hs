module Oasis.Runner.Common
  ( selectModelId
  , resolveModelId
  , buildUserMessages
  , extractAssistantContent
  , extractToolCall
  , ChatParams(..)
  , emptyChatParams
  , parseChatParams
  , parseExtraArgs
  , applyChatParams
  , requestChat
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI (ChatCompletionRequest(..), ChatCompletionResponse(..), ChatChoice(..), ClientError, sendChatCompletionRaw)
import Oasis.Chat.Message (userMessage)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:?), (.=), withObject)
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key

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
buildUserMessages prompt = [userMessage prompt]

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
    let get :: FromJSON a => Text -> Text -> Parser (Maybe a)
        get name alt = o .:? Key.fromText name <|> o .:? Key.fromText alt
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
  toJSON ChatParams{..} = Aeson.object $ catMaybes
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
parseChatParams = parseExtraArgs "Chat" emptyChatParams

parseExtraArgs :: FromJSON a => Text -> a -> Maybe Text -> Either Text a
parseExtraArgs label emptyValue = \case
  Nothing -> Right emptyValue
  Just raw
    | T.null (T.strip raw) -> Right emptyValue
    | otherwise ->
        case Aeson.eitherDecode (BL.fromStrict (TE.encodeUtf8 raw)) of
          Left err -> Left (label <> ": Invalid --extra-args JSON: " <> toText err)
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

requestChat :: Provider -> Text -> ChatParams -> ChatCompletionRequest -> IO (Either ClientError BL.ByteString)
requestChat provider apiKey params reqBase = do
  let reqBody = applyChatParams params reqBase
  sendChatCompletionRaw provider apiKey reqBody

extractAssistantContent :: ChatCompletionResponse -> Maybe Text
extractAssistantContent ChatCompletionResponse{choices} =
  case choices of
    (ChatChoice{message = Just Message{content}}:_) -> Just (messageContentText content)
    _ -> Nothing

extractToolCall :: ChatCompletionResponse -> Maybe (Message, ToolCall)
extractToolCall ChatCompletionResponse{choices} =
  case choices of
    (ChatChoice{message = Just msg@Message{tool_calls = Just (tc:_)}}:_) -> Just (msg, tc)
    _ -> Nothing