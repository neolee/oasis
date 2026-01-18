module Oasis.Runner.StructuredOutput
  ( StructuredMode(..)
  , runStructuredOutput
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI
import Oasis.Client.OpenAI.Types (setChatStream, setChatResponseFormat)
import qualified Oasis.Chat.Message as Msg
import Oasis.Runner.Common (resolveModelId, ChatParams, applyChatParams)
import Oasis.Runner.Stream (forEachDeltaContent)
import Data.Aeson (Value, decode, encode, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL

systemMessage :: Text
systemMessage = T.unlines
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

questionText :: Text
questionText = "Which is the longest river in the world? The Nile River."

jsonObjectFormat :: Value
jsonObjectFormat = Aeson.object
  [ "type" .= ("json_object" :: Text)
  ]

jsonSchemaFormat :: Value
jsonSchemaFormat = Aeson.object
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

data StructuredMode
  = JSONObject
  | JSONSchema
  deriving (Show, Eq)

runStructuredOutput :: Provider -> Text -> Maybe Text -> ChatParams -> StructuredMode -> IO (Either Text ())
runStructuredOutput provider apiKey modelOverride params mode = do
  let modelId = resolveModelId provider modelOverride
      messages =
        [ Msg.systemMessage systemMessage
        , Msg.userMessage questionText
        ]
      responseFormat = case mode of
        JSONObject -> jsonObjectFormat
        JSONSchema -> jsonSchemaFormat
      reqBase = defaultChatRequest modelId messages
      reqBaseStream = setChatStream True reqBase
      reqBaseFormat = setChatResponseFormat (Just responseFormat) reqBaseStream
      reqBody = applyChatParams params reqBaseFormat
  accumRef <- newIORef ""
  result <- streamChatCompletionWithRequest provider apiKey reqBody (handleChunk accumRef)
  case result of
    Left err -> pure (Left (renderClientError err))
    Right _ -> do
      putTextLn ""
      output <- readIORef accumRef
      case (decode (BL.fromStrict (TE.encodeUtf8 output)) :: Maybe Value) of
        Nothing -> putTextLn "Warning: response is not valid JSON."
        Just val -> do
          putTextLn "--- Parsed JSON ---"
          putTextLn (TE.decodeUtf8Lenient (BL.toStrict (encode val)))
      pure (Right ())

handleChunk :: IORef Text -> ChatCompletionStreamChunk -> IO ()
handleChunk accumRef chunk =
  forEachDeltaContent chunk $ \t -> do
    putText t
    modifyIORef' accumRef (<> t)
