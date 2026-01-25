module Oasis.Output.Common
  ( encodeJsonText
  , decodeJsonText
  , parseRawResponseStrict
  , buildRequestContext
  , selectBaseUrl
  , extractAssistantContent
  ) where

import Relude
import Data.Aeson (FromJSON, ToJSON, encode, eitherDecode, eitherDecodeStrict)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Oasis.Output.Types (RequestContext(..))
import Oasis.Types (Provider(..), Message(..), messageContentText)
import Oasis.Client.OpenAI (ChatCompletionResponse(..), ChatChoice(..), ClientError, renderClientError)

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

selectBaseUrl :: Provider -> Bool -> Text
selectBaseUrl Provider{base_url, beta_base_url} useBeta =
  let beta = beta_base_url >>= nonEmpty
  in if useBeta then fromMaybe base_url beta else base_url
  where
    nonEmpty t =
      let trimmed = T.strip t
      in if T.null trimmed then Nothing else Just trimmed

extractAssistantContent :: ChatCompletionResponse -> Maybe Text
extractAssistantContent ChatCompletionResponse{choices} =
  case choices of
    (ChatChoice{message = Just Message{content}}:_) -> Just (messageContentText content)
    _ -> Nothing
