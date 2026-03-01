module Oasis.Tui.Render.Output
  ( OutputBlock(..)
  , Output
  , RequestContext(..)
  , mdCodeSection
  , mdJsonSection
  , mdTextSection
  , plainTextSection
  , outputConcat
  , requestSections
  , renderErrorOutput
  ) where

import Relude
import qualified Data.Text as T
import Oasis.Output.Types (RequestContext(..))
import Oasis.Output.Render (prettyJsonText)

-- | A block of output content.
-- 'MdBlock' is rendered through the Markdown renderer (headings, code fences, etc.).
-- 'PlainBlock' is rendered verbatim, line by line, with no Markdown interpretation.
data OutputBlock
  = MdBlock Text
  | PlainBlock Text

-- | An ordered sequence of output blocks.
type Output = [OutputBlock]

prettyJson :: Text -> Text
prettyJson = prettyJsonText

codeBlock :: Text -> Text -> Text
codeBlock lang content =
  "```" <> lang <> "\n" <> content <> "\n```"

mdCodeSection :: Text -> Text -> Text -> Output
mdCodeSection title lang content =
  [MdBlock ("## " <> title <> "\n" <> codeBlock lang content)]

mdJsonSection :: Text -> Text -> Output
mdJsonSection title content =
  mdCodeSection title "json" (prettyJson content)

mdTextSection :: Text -> Text -> Output
mdTextSection title content =
  [MdBlock ("## " <> title <> "\n" <> content)]

-- | A section whose body is plain text: the title is a Markdown heading,
-- but the content is rendered verbatim with no Markdown interpretation.
plainTextSection :: Text -> Text -> Output
plainTextSection title content =
  [MdBlock ("## " <> title), PlainBlock content]

-- | Concatenate multiple outputs, inserting a blank line between each pair.
outputConcat :: [Output] -> Output
outputConcat []     = []
outputConcat [x]    = x
outputConcat (x:xs) = x <> [PlainBlock ""] <> outputConcat xs

-- | Returns a list of output sections so callers can append further sections
-- before passing the whole list to 'outputConcat'.
requestSections :: RequestContext -> [Output]
requestSections RequestContext{requestUrl, requestJson} =
  let prettyRequest = prettyJson requestJson
  in catMaybes
      [ Just (mdTextSection "Request URL" requestUrl)
      , if T.null (T.strip requestJson)
          then Nothing
          else Just (mdCodeSection "Request" "json" prettyRequest)
      ]

renderErrorOutput :: RequestContext -> Text -> Output
renderErrorOutput ctx err =
  outputConcat (requestSections ctx <> [mdTextSection "Error" err])
