module Oasis.Output.Render
  ( renderSectionsText
  , prettyJsonText
  ) where

import Relude
import Data.Aeson (Value, eitherDecodeStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Oasis.Output.Types (OutputSection(..), OutputPayload(..))

prettyJsonText :: Text -> Text
prettyJsonText input =
  case eitherDecodeStrict (encodeUtf8 input) :: Either String Value of
    Left _ -> input
    Right val -> decodeUtf8 (LBS.toStrict (encodePretty val))

renderSectionsText :: [OutputSection] -> Text
renderSectionsText sections =
  T.intercalate "\n\n" (map renderSection sections)

renderSection :: OutputSection -> Text
renderSection OutputSection{title, payload} =
  "## " <> title <> "\n" <> renderPayload payload

renderPayload :: OutputPayload -> Text
renderPayload = \case
  OutputText t -> t
  OutputJson t -> prettyJsonText t
  OutputJsonValue v -> decodeUtf8 (LBS.toStrict (encodePretty v))
