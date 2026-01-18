module Oasis.Runner.StructuredOutput
  ( StructuredMode(..)
  , runStructuredOutput
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI
import Oasis.Runner.Common (resolveModelId, ChatParams, applyChatParams)
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
      messages = [Message "system" systemMessage Nothing Nothing, Message "user" questionText Nothing Nothing]
      responseFormat = case mode of
        JSONObject -> jsonObjectFormat
        JSONSchema -> jsonSchemaFormat
      reqBase = ChatCompletionRequest
        { model = modelId
        , messages = messages
        , temperature = Nothing
        , top_p = Nothing
        , max_completion_tokens = Nothing
        , stop = Nothing
        , presence_penalty = Nothing
        , frequency_penalty = Nothing
        , seed = Nothing
        , logit_bias = Nothing
        , user = Nothing
        , service_tier = Nothing
        , reasoning_effort = Nothing
        , stream_options = Nothing
        , stream = True
        , response_format = Just responseFormat
        , tools = Nothing
        , tool_choice = Nothing
        , parallel_tool_calls = Nothing
        }
      reqBody = applyChatParams params reqBase
  accumRef <- newIORef ""
  result <- streamChatCompletionWithRequest provider apiKey reqBody (handleChunk accumRef)
  case result of
    Left err -> pure (Left err)
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
handleChunk accumRef ChatCompletionStreamChunk{choices} =
  forM_ choices $ \StreamChoice{delta} ->
    forM_ delta $ \StreamDelta{content} ->
      forM_ content $ \t -> do
        putText t
        modifyIORef' accumRef (<> t)
