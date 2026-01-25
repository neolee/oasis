module Oasis.Tui.Render.Output
  ( RequestContext(..)
  , prettyJson
  , codeBlock
  , mdCodeSection
  , mdJsonSection
  , mdTextSection
  , mdConcat
  , requestSections
  , renderErrorOutput
  ) where

import Relude
import qualified Data.Text as T
import Oasis.Output.Types (RequestContext(..))
import Oasis.Output.Render (prettyJsonText)

prettyJson :: Text -> Text
prettyJson = prettyJsonText

codeBlock :: Text -> Text -> Text
codeBlock lang content =
  "```" <> lang <> "\n" <> content <> "\n```"

mdCodeSection :: Text -> Text -> Text -> Text
mdCodeSection title lang content =
  "## " <> title <> "\n" <> codeBlock lang content

mdJsonSection :: Text -> Text -> Text
mdJsonSection title content =
  mdCodeSection title "json" (prettyJson content)

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
