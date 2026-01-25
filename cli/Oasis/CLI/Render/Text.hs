module Oasis.CLI.Render.Text
  ( renderSectionsText
  , requestSections
  , responseSections
  , textSection
  , jsonSection
  , warningSection
  ) where

import Relude
import Oasis.Types (RequestResponse(..))
import Oasis.Output.Types (OutputSection(..), OutputSectionKind(..), OutputPayload(..), RequestContext(..))
import Oasis.Output.Render (renderSectionsText)
import qualified Data.Text as T

requestSections :: RequestContext -> [OutputSection]
requestSections RequestContext{requestUrl, requestJson} =
  catMaybes
    [ Just (textSection SectionRequest "Request URL" requestUrl)
    , if T.null (T.strip requestJson)
        then Nothing
        else Just (jsonSection SectionRequest "Request" requestJson)
    ]

responseSections :: RequestResponse a -> [OutputSection]
responseSections RequestResponse{responseJson, response} =
  [ jsonSection SectionResponse "Response" responseJson ]
  <> ([warningSection "Warning" "Response JSON could not be decoded." |
    isNothing response])

textSection :: OutputSectionKind -> Text -> Text -> OutputSection
textSection kind title payload =
  OutputSection
    { kind
    , title
    , payload = OutputText payload
    }

jsonSection :: OutputSectionKind -> Text -> Text -> OutputSection
jsonSection kind title payload =
  OutputSection
    { kind
    , title
    , payload = OutputJson payload
    }

warningSection :: Text -> Text -> OutputSection
warningSection title payload =
  OutputSection
    { kind = SectionError
    , title
    , payload = OutputText payload
    }
