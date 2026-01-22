module Oasis.Tui.Render.Output
  ( RequestContext(..)
  , prettyJson
  , codeBlock
  , mdCodeSection
  , mdTextSection
  , mdConcat
  , requestSections
  , renderErrorOutput
  ) where

import Relude
import Data.Aeson (Value, eitherDecodeStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

data RequestContext = RequestContext
  { requestUrl  :: Text
  , requestJson :: Text
  } deriving (Show, Eq)

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

requestSections :: RequestContext -> [Text]
requestSections RequestContext{requestUrl, requestJson} =
  let prettyRequest = prettyJson requestJson
  in catMaybes
      [ Just (mdTextSection "Request URL" requestUrl)
      , if T.null (T.strip requestJson)
          then Nothing
          else Just (mdCodeSection "Request" "json" prettyRequest)
      ]

renderErrorOutput :: RequestContext -> Text -> Text
renderErrorOutput ctx err =
  mdConcat (requestSections ctx <> [mdTextSection "Error" err])
