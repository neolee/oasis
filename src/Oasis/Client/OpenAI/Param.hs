module Oasis.Client.OpenAI.Param
  ( ChatParams(..)
  , emptyChatParams
  , parseChatParams
  , parseExtraArgs
  , parseExtraBodyText
  , decodeExtraBodyValue
  , extraBodyFromEnableThinking
  , mergeExtraBodyList
  , lookupEnableThinking
  , applyExtraBodyToChatParams
  , applyChatParams
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI.Types (ChatCompletionRequest(..))
import Data.Aeson (FromJSON(..), ToJSON(..), (.:?), (.=), withObject)
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap

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
  , paramExtraBody :: Maybe Aeson.Value
  , paramEnableThinking :: Bool
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
    let paramExtraBody = Nothing
        paramEnableThinking = False
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
emptyChatParams = ChatParams
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  False

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
  let enableThinkingBody = extraBodyFromEnableThinking paramEnableThinking
      mergedExtraBody = mergeExtraBodyList (catMaybes [extra_body req, enableThinkingBody, paramExtraBody])
  in
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
    , extra_body = mergedExtraBody
    }

parseExtraBodyText :: Text -> Either Text (Maybe Aeson.Value)
parseExtraBodyText raw
  | T.null (T.strip raw) = Right Nothing
  | otherwise = Just <$> decodeExtraBodyValue raw

decodeExtraBodyValue :: Text -> Either Text Aeson.Value
decodeExtraBodyValue raw =
  case Aeson.eitherDecode (BL.fromStrict (TE.encodeUtf8 raw)) of
    Left err -> Left ("extra_body: invalid JSON: " <> toText err)
    Right val ->
      case val of
        Aeson.Object _ -> Right val
        _ -> Left "extra_body: expected a JSON object"

extraBodyFromEnableThinking :: Bool -> Maybe Aeson.Value
extraBodyFromEnableThinking enabled
  | enabled =
      let obj = KeyMap.singleton (Key.fromText "enable_thinking") (Aeson.Bool True)
      in Just (Aeson.Object obj)
  | otherwise = Nothing

mergeExtraBodyList :: [Aeson.Value] -> Maybe Aeson.Value
mergeExtraBodyList = \case
  [] -> Nothing
  (v:vs) -> Just (foldl' mergeExtraBodyValues v vs)

mergeExtraBodyValues :: Aeson.Value -> Aeson.Value -> Aeson.Value
mergeExtraBodyValues left right =
  case (left, right) of
    (Aeson.Object l, Aeson.Object r) -> Aeson.Object (mergeObjects l r)
    _ -> right
  where
    mergeObjects = KeyMap.unionWith mergeExtraBodyValues

lookupEnableThinking :: Aeson.Value -> Maybe Bool
lookupEnableThinking (Aeson.Object obj) =
  case KeyMap.lookup (Key.fromText "enable_thinking") obj of
    Just (Aeson.Bool b) -> Just b
    _ -> Nothing
lookupEnableThinking _ = Nothing

applyExtraBodyToChatParams :: Maybe Aeson.Value -> ChatParams -> ChatParams
applyExtraBodyToChatParams extra params =
  let merged = mergeExtraBodyList (catMaybes [paramExtraBody params, extra])
  in params { paramExtraBody = merged }
