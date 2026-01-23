module Oasis.Tui.Actions.Models
  ( runResponsesAction
  , runModelsAction
  , runEmbeddingsAction
  , providerModels
  ) where

import Relude
import Brick.Types (EventM)
import Control.Monad.State.Class (get)
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
  )
import Oasis.Model (resolveModelId, resolveEmbeddingModelId)
import Oasis.Runner.GetModels (runGetModels)
import qualified Oasis.Runner.Embeddings as Embeddings
import qualified Oasis.Runner.Responses as Responses
import Oasis.Tui.Actions.Common
  ( resolveSelectedProvider
  , startRunner
  , runInBackground
  , buildRequestContext
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
runResponsesAction inputText = do
  st <- get
  mResolved <- resolveSelectedProvider
  forM_ mResolved $ \(provider, apiKey) -> do
    let modelOverride = selectedModel st
        params = Responses.emptyResponsesParams
    startRunner "Running responses runner..."
    runInBackground st $ do
      let modelId = resolveModelId provider modelOverride
          reqBody = buildResponsesRequest modelId params inputText
          reqCtx = buildRequestContext (buildResponsesUrl (base_url provider)) reqBody
      result <- Responses.runResponses provider apiKey modelOverride params inputText
      let (statusMsg, outputMsg) =
            case result of
              Left err ->
                ("Responses runner failed.", renderErrorOutput reqCtx err)
              Right rr ->
                let assistantContent = response rr >>= \r -> output_text r
                    output = mdConcat
                      ( requestSections reqCtx
                        <> catMaybes
                            [ Just (mdJsonSection "Response" (responseJson rr))
                            , fmap (mdTextSection "Assistant") assistantContent
                            ]
                      )
                in ("Responses runner completed.", output)
      pure (ResponsesCompleted statusMsg outputMsg)

runModelsAction :: EventM Name AppState ()
runModelsAction = do
  st <- get
  mResolved <- resolveSelectedProvider
  forM_ mResolved $ \(provider, apiKey) -> do
    startRunner "Running models runner..."
    runInBackground st $ do
      let reqCtx = RequestContext (buildModelsUrl (base_url provider)) ""
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

runEmbeddingsAction :: Text -> EventM Name AppState ()
runEmbeddingsAction inputText = do
  st <- get
  mResolved <- resolveSelectedProvider
  forM_ mResolved $ \(provider, apiKey) -> do
    let modelOverride = selectedModel st
        params = Embeddings.emptyEmbeddingParams
    startRunner "Running embeddings runner..."
    runInBackground st $ do
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
                let summaryText = embeddingSummary <$> response rr
                    output = mdConcat
                      ( requestSections reqCtx
                        <> [ mdTextSection "Actual Model" modelId ]
                        <> catMaybes
                            [ Just (mdJsonSection "Response" (responseJson rr))
                            , fmap (mdTextSection "Summary") summaryText
                            ]
                      )
                in ("Embeddings runner completed.", output)
      pure (EmbeddingsCompleted statusMsg outputMsg)

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
      List.nub [chat_model_id, coder_model_id, reasoner_model_id]
