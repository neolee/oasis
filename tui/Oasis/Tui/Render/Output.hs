module Oasis.Tui.Render.Output
  ( prettyJson
  , codeBlock
  , mdCodeSection
  , mdTextSection
  , mdConcat
  ) where

import Relude
import Data.Aeson (Value, eitherDecodeStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

prettyJson :: Text -> Text
prettyJson input =
  case eitherDecodeStrict (encodeUtf8 input) :: Either String Value of
    Left _ -> input
    Right val -> decodeUtf8 (LBS.toStrict (encodePretty val))

codeBlock :: Text -> Text -> Text
codeBlock lang content =
  "```" <> lang <> "\n" <> content <> "\n```"

mdCodeSection :: Text -> Text -> Text -> Text
mdCodeSection title lang content =
  "## " <> title <> "\n" <> codeBlock lang content

mdTextSection :: Text -> Text -> Text
mdTextSection title content =
  "## " <> title <> "\n" <> content

mdConcat :: [Text] -> Text
mdConcat = T.intercalate "\n\n"
