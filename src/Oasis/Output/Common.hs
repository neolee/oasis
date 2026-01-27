module Oasis.Output.Common
  ( encodeJsonText
  , decodeJsonText
  , parseRawResponseStrict
  , buildRequestContext
  , extractAssistantContent
  , extractResponsesAssistantContent
  ) where

import Relude
import Data.Aeson (FromJSON, ToJSON, Value(..), encode, eitherDecode, eitherDecodeStrict)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Oasis.Output.Types (RequestContext(..))
import Oasis.Types (Message(..), messageContentText)
import Oasis.Client.OpenAI.Types (ChatCompletionResponse(..), ChatChoice(..), ResponsesResponse(..), ClientError)
import Oasis.Client.OpenAI (renderClientError)

encodeJsonText :: ToJSON a => a -> Text
encodeJsonText = TE.decodeUtf8Lenient . BL.toStrict . encode

decodeJsonText :: FromJSON a => Text -> Either Text a
decodeJsonText raw =
  case eitherDecodeStrict (TE.encodeUtf8 raw) of
    Left err -> Left (toText err)
    Right req -> Right req

parseRawResponseStrict :: FromJSON a => Either ClientError BL.ByteString -> Either Text (Text, a)
parseRawResponseStrict = \case
  Left err -> Left (renderClientError err)
  Right body ->
    case eitherDecode body of
      Left err ->
        let raw = TE.decodeUtf8Lenient (BL.toStrict body)
        in Left ("Failed to decode response: " <> toText err <> "\nRaw: " <> raw)
      Right val -> Right (TE.decodeUtf8Lenient (BL.toStrict body), val)

buildRequestContext :: ToJSON a => Text -> a -> RequestContext
buildRequestContext url reqBody =
  RequestContext
    { requestUrl = url
    , requestJson = encodeJsonText reqBody
    }

extractAssistantContent :: ChatCompletionResponse -> Maybe Text
extractAssistantContent ChatCompletionResponse{choices} =
  case choices of
    (ChatChoice{message = Just Message{content}}:_) -> Just (messageContentText content)
    _ -> Nothing

extractResponsesAssistantContent :: ResponsesResponse -> Maybe Text
extractResponsesAssistantContent ResponsesResponse{output_text, output} =
  output_text <|> (output >>= extractFromOutput)
  where
    extractFromOutput :: Value -> Maybe Text
    extractFromOutput = \case
      Array arr -> asum (map extractFromOutput (toList arr))
      Object obj ->
        let mType = lookupText "type" obj
            mRole = lookupText "role" obj
            contentVal = KM.lookup (Key.fromText "content") obj
            textVal = lookupText "text" obj
        in case mType of
            Just "message" ->
              contentVal >>= extractFromOutput
            Just "output_text" -> textVal
            _ ->
              case contentVal of
                Just v -> extractFromOutput v <|> textVal
                Nothing -> textVal
      _ -> Nothing

    lookupText :: Text -> Aeson.Object -> Maybe Text
    lookupText key obj =
      case KM.lookup (Key.fromText key) obj of
        Just (String t) -> Just t
        _ -> Nothing
