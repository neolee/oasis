module Oasis.Runner.Result
  ( encodeRequestJson
  , decodeResponseJson
  , decodeResponseJsonStrict
  , parseRawResponse
  , parseRawResponseStrict
  , buildRequestResponse
  ) where

import Relude
import Data.Aeson (ToJSON(..), FromJSON(..), encode, decode, eitherDecode)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Oasis.Client.OpenAI.Types (ClientError)
import Oasis.Client.OpenAI (renderClientError)
import Oasis.Types (RequestResponse(..))

encodeRequestJson :: ToJSON a => a -> Text
encodeRequestJson req = TE.decodeUtf8Lenient (BL.toStrict (encode req))

decodeResponseJson :: FromJSON a => BL.ByteString -> (Text, Maybe a)
decodeResponseJson body =
  let respText = TE.decodeUtf8Lenient (BL.toStrict body)
      decoded = decode body
  in (respText, decoded)

decodeResponseJsonStrict :: FromJSON a => BL.ByteString -> Either Text (Text, a)
decodeResponseJsonStrict body =
  case eitherDecode body of
    Left err ->
      let raw = TE.decodeUtf8Lenient (BL.toStrict body)
      in Left ("Failed to decode response: " <> toText err <> "\nRaw: " <> raw)
    Right val -> Right (TE.decodeUtf8Lenient (BL.toStrict body), val)

parseRawResponse :: FromJSON a => Either ClientError BL.ByteString -> Either Text (Text, Maybe a)
parseRawResponse = \case
  Left err -> Left (renderClientError err)
  Right body -> Right (decodeResponseJson body)

parseRawResponseStrict :: FromJSON a => Either ClientError BL.ByteString -> Either Text (Text, a)
parseRawResponseStrict = \case
  Left err -> Left (renderClientError err)
  Right body -> decodeResponseJsonStrict body

buildRequestResponse :: FromJSON a => Text -> Either ClientError BL.ByteString -> Either Text (RequestResponse a)
buildRequestResponse reqJson resp =
  case parseRawResponse resp of
    Left err -> Left err
    Right (respText, decoded) -> Right (RequestResponse reqJson respText decoded)
