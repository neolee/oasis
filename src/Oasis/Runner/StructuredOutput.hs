module Oasis.Runner.StructuredOutput
  ( StructuredMode(..)
  , StructuredOutputResult(..)
  , runStructuredOutput
  , runStructuredOutputDetailed
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI
import Oasis.Client.OpenAI.Types (setChatStream, setChatResponseFormat)
import qualified Oasis.Chat.Message as Msg
import Oasis.Model (resolveModelId)
import Oasis.Client.OpenAI.Param (ChatParams, applyChatParams)
import Oasis.Runner.Stream (forEachDeltaContent)
import Data.Aeson (Value, decode, encode, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL

data StructuredOutputResult = StructuredOutputResult
  { rawText :: Text
  , parsedJson :: Either Text Text
  } deriving (Show, Eq)

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

runStructuredOutput :: Provider -> Text -> Maybe Text -> ChatParams -> StructuredMode -> Bool -> IO (Either Text ())
runStructuredOutput provider apiKey modelOverride params mode useBeta = do
  result <- runStructuredOutputDetailed provider apiKey modelOverride params mode useBeta
  case result of
    Left err -> pure (Left err)
    Right StructuredOutputResult{rawText, parsedJson} -> do
      unless (T.null rawText) (putText rawText)
      putTextLn ""
      case parsedJson of
        Left _ -> putTextLn "Warning: response is not valid JSON."
        Right pretty -> do
          putTextLn "--- Parsed JSON ---"
          putTextLn pretty
      pure (Right ())

runStructuredOutputDetailed :: Provider -> Text -> Maybe Text -> ChatParams -> StructuredMode -> Bool -> IO (Either Text StructuredOutputResult)
runStructuredOutputDetailed provider apiKey modelOverride params mode useBeta = do
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
  result <- streamChatCompletionWithRequestWithHooks emptyClientHooks provider apiKey reqBody (handleChunk accumRef) useBeta
  case result of
    Left err -> pure (Left (renderClientError err))
    Right _ -> do
      output <- readIORef accumRef
      let parsed =
            case (decode (BL.fromStrict (TE.encodeUtf8 output)) :: Maybe Value) of
              Nothing -> Left "Invalid JSON"
              Just val -> Right (TE.decodeUtf8Lenient (BL.toStrict (encode val)))
      pure (Right StructuredOutputResult
        { rawText = output
        , parsedJson = parsed
        })

handleChunk :: IORef Text -> ChatCompletionStreamChunk -> IO ()
handleChunk accumRef chunk =
  forEachDeltaContent chunk $ \t ->
    modifyIORef' accumRef (<> t)
