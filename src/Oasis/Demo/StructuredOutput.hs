module Oasis.Demo.StructuredOutput
  ( structuredSystemMessage
  , structuredQuestionText
  , structuredMessages
  , jsonObjectFormat
  , jsonSchemaFormat
  ) where

import Relude
import Data.Aeson (Value, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import Oasis.Chat.Message (systemMessage, userMessage)
import Oasis.Types (Message)

structuredSystemMessage :: Text
structuredSystemMessage = T.unlines
  [ "The user will provide some exam text. Please parse the \"question\" and \"answer\" and output them in JSON format."
  , ""
  , "EXAMPLE INPUT:"
  , "Which is the highest mountain in the world? Mount Everest."
  , ""
  , "EXAMPLE JSON OUTPUT:"
  , "{"
  , "  \"question\": \"Which is the highest mountain in the world?\","
  , "  \"answer\": \"Mount Everest\""
  , "}"
  ]

structuredQuestionText :: Text
structuredQuestionText = "Which is the longest river in the world? The Nile River."

structuredMessages :: [Message]
structuredMessages =
  [ systemMessage structuredSystemMessage
  , userMessage structuredQuestionText
  ]

jsonObjectFormat :: Value
jsonObjectFormat =
  Aeson.object
    [ "type" .= ("json_object" :: Text)
    ]

jsonSchemaFormat :: Value
jsonSchemaFormat =
  Aeson.object
  [ "type" .= ("json_schema" :: Text)
  , "json_schema" .= Aeson.object
    [ "name" .= ("session" :: Text)
    , "schema" .= Aeson.object
      [ "type" .= ("object" :: Text)
      , "properties" .= Aeson.object
        [ "question" .= Aeson.object ["type" .= ("string" :: Text)]
        , "answer" .= Aeson.object ["type" .= ("string" :: Text)]
        ]
      , "required" .= (["question", "answer"] :: [Text])
      ]
    , "required" .= (["session"] :: [Text])
    ]
  ]
