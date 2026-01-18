module Oasis.Runner.Result
  ( RunnerResult(..)
  , encodeRequestJson
  , decodeResponseJson
  , decodeResponseJsonStrict
  , parseRawResponse
  , parseRawResponseStrict
  , buildRunnerResult
  ) where

import Relude
import Data.Aeson (ToJSON(..), FromJSON(..), encode, decode, eitherDecode)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Oasis.Client.OpenAI (ClientError, renderClientError)

data RunnerResult a = RunnerResult
  { requestJson  :: Text
  , responseJson :: Text
  , response     :: Maybe a
  } deriving (Show, Eq)

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

buildRunnerResult :: FromJSON a => Text -> Either ClientError BL.ByteString -> Either Text (RunnerResult a)
buildRunnerResult reqJson resp =
  case parseRawResponse resp of
    Left err -> Left err
    Right (respText, decoded) -> Right (RunnerResult reqJson respText decoded)
