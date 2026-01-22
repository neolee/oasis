module Oasis.Tui.Actions
  ( runBasicAction
  , runResponsesAction
  , runModelsAction
  , runEmbeddingsAction
  , runHooksAction
  , runStructuredJsonAction
  , runStructuredSchemaAction
  , providerModels
  ) where

import Relude
import Brick.BChan (writeBChan)
import Brick.Types (EventM)
import Control.Concurrent (forkIO)
import Control.Monad.State.Class (get, modify)
import qualified Data.List as List
import qualified Data.Map.Strict as M
import Oasis.Config (resolveProvider)
import Oasis.Runner.Basic (runBasic)
import qualified Oasis.Runner.Responses as Responses
import Oasis.Runner.GetModels (runGetModels)
import qualified Oasis.Runner.Embeddings as Embeddings
import Oasis.Tui.Render.Output (RequestContext(..), prettyJson, mdCodeSection, mdTextSection, mdConcat, requestSections, renderErrorOutput)
import Oasis.Tui.State (AppState(..), Name(..), TuiEvent(..))
import Oasis.Types (Config(..), Provider(..), RequestResponse(..), Message(..), messageContentText)
import Oasis.Client.OpenAI (ResponsesResponse(..), ChatCompletionResponse(..), ChatChoice(..), ChatCompletionRequest(..), ResponsesRequest(..), EmbeddingRequest(..), EmbeddingResponse(..), EmbeddingData(..), ChatCompletionStreamChunk(..), StreamChoice(..), StreamDelta(..), defaultChatRequest, buildResponsesUrl, buildChatUrl, buildModelsUrl, buildEmbeddingsUrl, sendChatCompletionRawWithHooks, renderClientError, ClientHooks(..), streamChatCompletionWithRequestWithHooks, emptyClientHooks)
import qualified Oasis.Client.OpenAI as OpenAI
import Oasis.Client.OpenAI.Param (applyChatParams)
import Oasis.Chat.Message (userMessage, systemMessage)
import Oasis.Model (resolveModelId, resolveEmbeddingModelId)
import Data.Aeson (encode, ToJSON, Value, eitherDecodeStrict, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.IORef as IORef
import qualified Data.ByteString as BS
import Data.CaseInsensitive (original)
import Network.HTTP.Client (Request(..))
import Network.HTTP.Types.Header (HeaderName, hAuthorization)
import Network.HTTP.Types.Status (Status, statusCode)

runBasicAction :: Text -> EventM Name AppState ()
runBasicAction prompt = do
  st <- get
  case selectedProvider st of
    Nothing ->
      modify (\s -> s { statusText = "Select a provider first." })
    Just providerName -> do
      resolved <- liftIO (resolveProvider (config st) providerName)
      case resolved of
        Nothing ->
          modify (\s -> s { statusText = "Provider not found: " <> providerName })
        Just (provider, apiKey) -> do
          let modelOverride = selectedModel st
              params = chatParams st
              useBeta = betaUrlSetting st
          modify (\s -> s
            { statusText = "Running basic runner..."
            , outputText = ""
            , runnerStarted = True
            })
          let chan = eventChan st
          void $ liftIO $ forkIO $ do
            let modelId = resolveModelId provider modelOverride
                baseUrl = selectBaseUrl provider useBeta
                reqBody = applyChatParams params (defaultChatRequest modelId [userMessage prompt])
                reqCtx = buildRequestContext (buildChatUrl baseUrl) reqBody
            result <- runBasic provider apiKey modelOverride params prompt useBeta
            let (statusMsg, outputMsg) =
                  case result of
                    Left err ->
                      ("Basic runner failed.", renderErrorOutput reqCtx err)
                    Right rr ->
                      let prettyResponse = prettyJson (responseJson rr)
                          assistantContent = response rr >>= extractAssistantContent
                          output = mdConcat
                            ( requestSections reqCtx
                              <> catMaybes
                                  [ Just (mdCodeSection "Response" "json" prettyResponse)
                                  , fmap (mdTextSection "Assistant") assistantContent
                                  ]
                            )
                      in ("Basic runner completed.", output)
            writeBChan chan (BasicCompleted statusMsg outputMsg)

runResponsesAction :: Text -> EventM Name AppState ()
runResponsesAction inputText = do
  st <- get
  case selectedProvider st of
    Nothing ->
      modify (\s -> s { statusText = "Select a provider first." })
    Just providerName -> do
      resolved <- liftIO (resolveProvider (config st) providerName)
      case resolved of
        Nothing ->
          modify (\s -> s { statusText = "Provider not found: " <> providerName })
        Just (provider, apiKey) -> do
          let modelOverride = selectedModel st
              params = Responses.emptyResponsesParams
          modify (\s -> s
            { statusText = "Running responses runner..."
            , outputText = ""
            , runnerStarted = True
            })
          let chan = eventChan st
          void $ liftIO $ forkIO $ do
            let modelId = resolveModelId provider modelOverride
                reqBody = buildResponsesRequest modelId params inputText
                reqCtx = buildRequestContext (buildResponsesUrl (base_url provider)) reqBody
            result <- Responses.runResponses provider apiKey modelOverride params inputText
            let (statusMsg, outputMsg) =
                  case result of
                    Left err ->
                      ("Responses runner failed.", renderErrorOutput reqCtx err)
                    Right rr ->
                      let prettyResponse = prettyJson (responseJson rr)
                          assistantContent = response rr >>= \r -> output_text r
                          output = mdConcat
                            ( requestSections reqCtx
                              <> catMaybes
                                  [ Just (mdCodeSection "Response" "json" prettyResponse)
                                  , fmap (mdTextSection "Assistant") assistantContent
                                  ]
                            )
                      in ("Responses runner completed.", output)
            writeBChan chan (ResponsesCompleted statusMsg outputMsg)

runModelsAction :: EventM Name AppState ()
runModelsAction = do
  st <- get
  case selectedProvider st of
    Nothing ->
      modify (\s -> s { statusText = "Select a provider first." })
    Just providerName -> do
      resolved <- liftIO (resolveProvider (config st) providerName)
      case resolved of
        Nothing ->
          modify (\s -> s { statusText = "Provider not found: " <> providerName })
        Just (provider, apiKey) -> do
          modify (\s -> s
            { statusText = "Running models runner..."
            , outputText = ""
            , runnerStarted = True
            })
          let chan = eventChan st
          void $ liftIO $ forkIO $ do
            let reqCtx = RequestContext (buildModelsUrl (base_url provider)) ""
            result <- runGetModels provider apiKey
            let (statusMsg, outputMsg) =
                  case result of
                    Left err ->
                      ("Models runner failed.", renderErrorOutput reqCtx err)
                    Right rr ->
                      let prettyResponse = prettyJson (responseJson rr)
                          output = mdConcat
                            ( requestSections reqCtx
                              <> [mdCodeSection "Response" "json" prettyResponse]
                            )
                      in ("Models runner completed.", output)
            writeBChan chan (ModelsCompleted statusMsg outputMsg)

runEmbeddingsAction :: Text -> EventM Name AppState ()
runEmbeddingsAction inputText = do
  st <- get
  case selectedProvider st of
    Nothing ->
      modify (\s -> s { statusText = "Select a provider first." })
    Just providerName -> do
      resolved <- liftIO (resolveProvider (config st) providerName)
      case resolved of
        Nothing ->
          modify (\s -> s { statusText = "Provider not found: " <> providerName })
        Just (provider, apiKey) -> do
          let modelOverride = selectedModel st
              params = Embeddings.emptyEmbeddingParams
          modify (\s -> s
            { statusText = "Running embeddings runner..."
            , outputText = ""
            , runnerStarted = True
            })
          let chan = eventChan st
          void $ liftIO $ forkIO $ do
            let modelId = resolveEmbeddingModelId provider modelOverride
                reqBody = buildEmbeddingsRequest modelId params inputText
                reqCtx = buildRequestContext (buildEmbeddingsUrl (base_url provider)) reqBody
            result <- Embeddings.runEmbeddings provider apiKey modelOverride params inputText
            let (statusMsg, outputMsg) =
                  case result of
                    Left err ->
                      let output = mdConcat
                            ( requestSections reqCtx
                              <> [ mdTextSection "Actual Model" modelId
                                 , mdTextSection "Error" err
                                 ]
                            )
                      in ("Embeddings runner failed.", output)
                    Right rr ->
                      let prettyResponse = prettyJson (responseJson rr)
                          summaryText = embeddingSummary <$> response rr
                          output = mdConcat
                            ( requestSections reqCtx
                              <> [ mdTextSection "Actual Model" modelId ]
                              <> catMaybes
                                  [ Just (mdCodeSection "Response" "json" prettyResponse)
                                  , fmap (mdTextSection "Summary") summaryText
                                  ]
                            )
                      in ("Embeddings runner completed.", output)
            writeBChan chan (EmbeddingsCompleted statusMsg outputMsg)

runHooksAction :: Text -> EventM Name AppState ()
runHooksAction prompt = do
  st <- get
  case selectedProvider st of
    Nothing ->
      modify (\s -> s { statusText = "Select a provider first." })
    Just providerName -> do
      resolved <- liftIO (resolveProvider (config st) providerName)
      case resolved of
        Nothing ->
          modify (\s -> s { statusText = "Provider not found: " <> providerName })
        Just (provider, apiKey) -> do
          let modelOverride = selectedModel st
              params = chatParams st
              useBeta = betaUrlSetting st
          modify (\s -> s
            { statusText = "Running hooks runner..."
            , outputText = ""
            , runnerStarted = True
            })
          let chan = eventChan st
          void $ liftIO $ forkIO $ do
            let modelId = resolveModelId provider modelOverride
                baseUrl = selectBaseUrl provider useBeta
                reqBody = applyChatParams params (defaultChatRequest modelId [userMessage prompt])
                reqCtx = buildRequestContext (buildChatUrl baseUrl) reqBody
            (statusMsg, outputMsg) <- runHooksWithLog provider apiKey useBeta reqCtx reqBody
            writeBChan chan (HooksCompleted statusMsg outputMsg)

runStructuredJsonAction :: EventM Name AppState ()
runStructuredJsonAction = runStructuredAction jsonObjectFormat "structured-json"

runStructuredSchemaAction :: EventM Name AppState ()
runStructuredSchemaAction = runStructuredAction jsonSchemaFormat "structured-schema"

extractAssistantContent :: ChatCompletionResponse -> Maybe Text
extractAssistantContent ChatCompletionResponse{choices} =
  case choices of
    (ChatChoice{message = Just Message{content}}:_) -> Just (messageContentText content)
    _ -> Nothing

encodeJsonText :: ToJSON a => a -> Text
encodeJsonText = TE.decodeUtf8Lenient . BL.toStrict . encode

buildRequestContext :: ToJSON a => Text -> a -> RequestContext
buildRequestContext url reqBody =
  RequestContext
    { requestUrl = url
    , requestJson = encodeJsonText reqBody
    }

selectBaseUrl :: Provider -> Bool -> Text
selectBaseUrl Provider{base_url, beta_base_url} useBeta =
  let beta = beta_base_url >>= nonEmpty
  in if useBeta then fromMaybe base_url beta else base_url
  where
    nonEmpty t =
      let trimmed = T.strip t
      in if T.null trimmed then Nothing else Just trimmed

buildResponsesRequest :: Text -> Responses.ResponsesParams -> Text -> ResponsesRequest
buildResponsesRequest modelId params inputText =
  ResponsesRequest
    { model = modelId
    , input = Aeson.String inputText
    , stream = Nothing
    , max_output_tokens = Responses.paramMaxOutputTokens params
    , temperature = Responses.paramTemperature params
    , top_p = Responses.paramTopP params
    , user = Responses.paramUser params
    , response_format = Responses.paramResponseFormat params
    }

buildEmbeddingsRequest :: Text -> Embeddings.EmbeddingParams -> Text -> EmbeddingRequest
buildEmbeddingsRequest modelId params inputText =
  EmbeddingRequest
    { model = modelId
    , input = Aeson.String inputText
    , encoding_format = Embeddings.paramEncodingFormat params
    , dimensions = Embeddings.paramDimensions params
    , user = Embeddings.paramUser params
    }

embeddingSummary :: EmbeddingResponse -> Text
embeddingSummary EmbeddingResponse{data_} =
  let count = length data_
      firstEmbedding = listToMaybe data_ >>= \EmbeddingData{embedding} -> Just embedding
      dim = maybe 0 length firstEmbedding
      preview = maybe "[]" (formatPreview 6) firstEmbedding
  in T.unlines
      [ "Vectors: " <> show count
      , "Dimensions: " <> show dim
      , "Head: " <> preview
      ]
  where
    formatPreview n xs =
      let items = map (T.pack . show) (take n xs)
      in "[" <> T.intercalate ", " items <> if length xs > n then ", ...]" else "]"

runHooksWithLog
  :: Provider
  -> Text
  -> Bool
  -> RequestContext
  -> ChatCompletionRequest
  -> IO (Text, Text)
runHooksWithLog provider apiKey useBeta reqCtx reqBody = do
  logRef <- IORef.newIORef ([] :: [Text])
  let appendLog t = IORef.modifyIORef' logRef (<> [t])
      hooks = ClientHooks
        { onRequest = Just (logRequest appendLog)
        , onResponse = Just (logResponse appendLog)
        , onError = Just (appendLog . renderClientError)
        }
  result <- sendChatCompletionRawWithHooks hooks provider apiKey reqBody useBeta
  logs <- IORef.readIORef logRef
  let logText = T.intercalate "\n" logs
  case result of
    Left err -> do
      let output = mdConcat
            ( requestSections reqCtx
              <> [ mdTextSection "Hook Log" logText
                 , mdTextSection "Error" (renderClientError err)
                 ]
            )
      pure ("Hooks runner failed.", output)
    Right body -> do
      let responseText = truncateText 800 (TE.decodeUtf8Lenient (BL.toStrict body))
          output = mdConcat
            ( requestSections reqCtx
              <> [ mdTextSection "Hook Log" logText
                 , mdCodeSection "Response JSON (truncated)" "json" responseText
                 ]
            )
      pure ("Hooks runner completed.", output)

logRequest :: (Text -> IO ()) -> Request -> IO ()
logRequest appendLog req = do
  appendLog "--- Hook: Request ---"
  appendLog (formatRequestLine req)
  forM_ (requestHeaders req) $ \(name, value) ->
    appendLog (formatHeader name value)

logResponse :: (Text -> IO ()) -> Status -> [(HeaderName, BS.ByteString)] -> BL.ByteString -> IO ()
logResponse appendLog status headers body = do
  appendLog "--- Hook: Response ---"
  appendLog ("Status: " <> show (statusCode status))
  forM_ headers $ \(name, value) ->
    appendLog (formatHeader name value)
  appendLog ("Body bytes: " <> show (BL.length body))

formatRequestLine :: Request -> Text
formatRequestLine req =
  let scheme = if secure req then "https" else "http"
      hostText = TE.decodeUtf8Lenient (host req)
      pathText = TE.decodeUtf8Lenient (path req)
      queryText = TE.decodeUtf8Lenient (queryString req)
      url = scheme <> "://" <> hostText <> pathText <> queryText
      methodText = TE.decodeUtf8Lenient (method req)
  in methodText <> " " <> url

formatHeader :: HeaderName -> BS.ByteString -> Text
formatHeader name value
  | name == hAuthorization = TE.decodeUtf8Lenient (original name) <> ": <redacted>"
  | otherwise = TE.decodeUtf8Lenient (original name) <> ": " <> TE.decodeUtf8Lenient value

truncateText :: Int -> Text -> Text
truncateText maxLen txt
  | T.length txt <= maxLen = txt
  | otherwise = T.take maxLen txt <> "..."

runStructuredAction :: Value -> Text -> EventM Name AppState ()
runStructuredAction responseFormat runnerLabel = do
  st <- get
  case selectedProvider st of
    Nothing ->
      modify (\s -> s { statusText = "Select a provider first." })
    Just providerName -> do
      resolved <- liftIO (resolveProvider (config st) providerName)
      case resolved of
        Nothing ->
          modify (\s -> s { statusText = "Provider not found: " <> providerName })
        Just (provider, apiKey) -> do
          let modelOverride = selectedModel st
              params = chatParams st
              useBeta = betaUrlSetting st
          modify (\s -> s
            { statusText = "Running " <> runnerLabel <> " runner..."
            , outputText = ""
            , runnerStarted = True
            })
          let chan = eventChan st
          void $ liftIO $ forkIO $ do
            let modelId = resolveModelId provider modelOverride
                reqMessages =
                  [ systemMessage structuredSystemMessage
                  , userMessage structuredQuestionText
                  ]
                reqBase = defaultChatRequest modelId reqMessages
                ChatCompletionRequest{..} = applyChatParams params reqBase
                reqBody = ChatCompletionRequest
                  { OpenAI.stream = True
                  , OpenAI.response_format = Just responseFormat
                  , ..
                  }
            accumRef <- IORef.newIORef ""
            result <- streamChatCompletionWithRequestWithHooks emptyClientHooks provider apiKey reqBody (handleStructuredChunk accumRef) useBeta
            rawText <- IORef.readIORef accumRef
            let (statusMsg, outputMsg) =
                  case result of
                    Left err ->
                      let output = mdConcat
                            [ mdTextSection "Raw Stream" rawText
                            , mdTextSection "Parsed JSON" ("Error: " <> renderClientError err)
                            ]
                      in (runnerLabel <> " runner failed.", output)
                    Right _ ->
                      let parsedSection =
                            case parseJsonText rawText of
                              Left perr -> mdTextSection "Parsed JSON" ("Invalid JSON: " <> perr)
                              Right pretty -> mdCodeSection "Parsed JSON" "json" pretty
                          output = mdConcat
                            [ mdTextSection "Raw Stream" rawText
                            , parsedSection
                            ]
                      in (runnerLabel <> " runner completed.", output)
            writeBChan chan (StructuredCompleted statusMsg outputMsg)

handleStructuredChunk :: IORef.IORef Text -> ChatCompletionStreamChunk -> IO ()
handleStructuredChunk accumRef chunk =
  forEachDeltaContentLocal chunk $ \t ->
    IORef.modifyIORef' accumRef (<> t)

forEachDeltaContentLocal :: ChatCompletionStreamChunk -> (Text -> IO ()) -> IO ()
forEachDeltaContentLocal ChatCompletionStreamChunk{choices} f =
  forM_ choices $ \StreamChoice{delta} ->
    forM_ (maybe [] deltaContent delta) f

deltaContent :: StreamDelta -> [Text]
deltaContent StreamDelta{content} = maybe [] pure content

parseJsonText :: Text -> Either Text Text
parseJsonText raw =
  case eitherDecodeStrict (encodeUtf8 raw) :: Either String Value of
    Left err -> Left (T.pack err)
    Right _ -> Right (prettyJson raw)

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

providerModels :: Config -> Text -> [Text]
providerModels cfg providerName =
  case M.lookup providerName (providers cfg) of
    Nothing -> []
    Just Provider{chat_model_id, coder_model_id, reasoner_model_id} ->
      List.nub [chat_model_id, coder_model_id, reasoner_model_id]
