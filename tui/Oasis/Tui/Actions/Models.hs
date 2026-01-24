module Oasis.Tui.Actions.Models
  ( runResponsesAction
  , runModelsAction
  , runEmbeddingsAction
  , customModelItem
  , providerModels
  ) where

import Relude
import Brick.Types (EventM)
import qualified Data.List as List
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import Oasis.Client.OpenAI
  ( ResponsesRequest(..)
  , ResponsesResponse(..)
  , EmbeddingRequest(..)
  , EmbeddingResponse(..)
  , EmbeddingData(..)
  , buildResponsesUrl
  , buildModelsUrl
  , buildEmbeddingsUrl
  , sendResponsesRaw
  , sendEmbeddingsRaw
  )
import Oasis.Model (resolveModelId, resolveEmbeddingModelId)
import Oasis.Runner.GetModels (runGetModels)
import qualified Oasis.Runner.Embeddings as Embeddings
import qualified Oasis.Runner.Responses as Responses
import Oasis.Tui.Actions.Common
  ( runProviderAction
  , buildRequestContext
  , encodeJsonText
  , buildDebugInfo
  , jsonRequestHeaders
  , modelsRequestHeaders
  , decodeJsonText
  , parseRawResponseStrict
  )
import Oasis.Tui.Render.Output
  ( RequestContext(..)
  , mdJsonSection
  , mdTextSection
  , mdConcat
  , requestSections
  , renderErrorOutput
  )
import Oasis.Tui.State (AppState(..), Name(..), TuiEvent(..))
import Oasis.Types (Config(..), Provider(..), RequestResponse(..))

runResponsesAction :: Text -> EventM Name AppState ()
runResponsesAction inputText =
  runProviderAction "Running responses runner..." $ \st provider apiKey -> do
    let modelOverride = selectedModel st
        params = Responses.emptyResponsesParams
        providerName = fromMaybe "-" (selectedProvider st)
        modelId = resolveModelId provider modelOverride
        reqBody = buildResponsesRequest modelId params inputText
        reqJson = encodeJsonText reqBody
        info = buildDebugInfo providerName modelId (buildResponsesUrl (base_url provider)) (jsonRequestHeaders apiKey)
        handler bodyText = do
          reqBody' <- (decodeJsonText bodyText :: Either Text ResponsesRequest)
          if stream reqBody' == Just True
            then Left "responses runner requires stream=false"
            else Right $ do
              let reqCtx = buildRequestContext (buildResponsesUrl (base_url provider)) reqBody'
              result <- sendResponsesRaw provider apiKey reqBody'
              let (statusMsg, outputMsg) =
                    case parseRawResponseStrict result of
                      Left err ->
                        ("Responses runner failed.", renderErrorOutput reqCtx err)
                      Right (raw, response) ->
                        let assistantContent = output_text response
                            output = mdConcat
                              ( requestSections reqCtx
                                <> catMaybes
                                    [ Just (mdJsonSection "Response" raw)
                                    , fmap (mdTextSection "Assistant") assistantContent
                                    ]
                              )
                        in ("Responses runner completed.", output)
              pure (ResponsesCompleted statusMsg outputMsg)
    pure (info, reqJson, handler)

runModelsAction :: EventM Name AppState ()
runModelsAction =
  runProviderAction "Running models runner..." $ \st provider apiKey -> do
    let providerName = fromMaybe "-" (selectedProvider st)
        reqCtx = RequestContext (buildModelsUrl (base_url provider)) ""
        info = buildDebugInfo providerName "-" (buildModelsUrl (base_url provider)) (modelsRequestHeaders apiKey)
        handler _ = Right $ do
          result <- runGetModels provider apiKey
          let (statusMsg, outputMsg) =
                case result of
                  Left err ->
                    ("Models runner failed.", renderErrorOutput reqCtx err)
                  Right rr ->
                    let output = mdConcat
                          ( requestSections reqCtx
                            <> [mdJsonSection "Response" (responseJson rr)]
                          )
                    in ("Models runner completed.", output)
          pure (ModelsCompleted statusMsg outputMsg)
    pure (info, "", handler)

runEmbeddingsAction :: Text -> EventM Name AppState ()
runEmbeddingsAction inputText =
  runProviderAction "Running embeddings runner..." $ \st provider apiKey -> do
    let modelOverride = selectedModel st
        params = Embeddings.emptyEmbeddingParams
        providerName = fromMaybe "-" (selectedProvider st)
        modelId = resolveEmbeddingModelId provider modelOverride
        reqBody = buildEmbeddingsRequest modelId params inputText
        reqJson = encodeJsonText reqBody
        info = buildDebugInfo providerName modelId (buildEmbeddingsUrl (base_url provider)) (jsonRequestHeaders apiKey)
        handler bodyText = do
          reqBody' <- (decodeJsonText bodyText :: Either Text EmbeddingRequest)
          Right $ do
            let reqCtx = buildRequestContext (buildEmbeddingsUrl (base_url provider)) reqBody'
            result <- sendEmbeddingsRaw provider apiKey reqBody'
            let (statusMsg, outputMsg) =
                  case parseRawResponseStrict result of
                    Left err ->
                      let output = mdConcat
                            ( requestSections reqCtx
                              <> [ mdTextSection "Actual Model" modelId
                                 , mdTextSection "Error" err
                                 ]
                            )
                      in ("Embeddings runner failed.", output)
                    Right (raw, response) ->
                      let summaryText = embeddingSummary response
                          output = mdConcat
                            ( requestSections reqCtx
                              <> [ mdTextSection "Actual Model" modelId ]
                              <> [ mdJsonSection "Response" raw
                                 , mdTextSection "Summary" summaryText
                                 ]
                            )
                      in ("Embeddings runner completed.", output)
            pure (EmbeddingsCompleted statusMsg outputMsg)
    pure (info, reqJson, handler)

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


providerModels :: Config -> Text -> [Text]
providerModels cfg providerName =
  case M.lookup providerName (providers cfg) of
    Nothing -> []
    Just Provider{chat_model_id, coder_model_id, reasoner_model_id} ->
      let models = List.nub [chat_model_id, coder_model_id, reasoner_model_id]
      in models <> [customModelItem]

customModelItem :: Text
customModelItem = "<···>"
