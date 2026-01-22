module Oasis.Tui.Actions
  ( runBasicAction
  , runResponsesAction
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
import Oasis.Runner.Responses (runResponses, emptyResponsesParams, ResponsesParams(..))
import Oasis.Tui.Render.Output (prettyJson, mdCodeSection, mdTextSection, mdConcat)
import Oasis.Tui.State (AppState(..), Name(..), TuiEvent(..))
import Oasis.Types (Config(..), Provider(..), RequestResponse(..), Message(..), messageContentText)
import Oasis.Client.OpenAI (ResponsesResponse(..), ChatCompletionResponse(..), ChatChoice(..), ChatCompletionRequest(..), ResponsesRequest(..), defaultChatRequest, buildResponsesUrl, buildChatUrl)
import Oasis.Client.OpenAI.Param (applyChatParams)
import Oasis.Chat.Message (userMessage)
import Oasis.Model (resolveModelId)
import Data.Aeson (encode, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

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
                requestUrl = buildChatUrl baseUrl
                reqBody = applyChatParams params (defaultChatRequest modelId [userMessage prompt])
                reqJsonText = encodeJsonText reqBody
            result <- runBasic provider apiKey modelOverride params prompt useBeta
            let (statusMsg, outputMsg) =
                  case result of
                    Left err ->
                      let prettyRequest = prettyJson reqJsonText
                          output = mdConcat
                            [ mdTextSection "Request URL" requestUrl
                            , mdCodeSection "Request" "json" prettyRequest
                            , mdTextSection "Error" err
                            ]
                      in ("Basic runner failed.", output)
                    Right rr ->
                      let prettyRequest = prettyJson (requestJson rr)
                          prettyResponse = prettyJson (responseJson rr)
                          assistantContent = response rr >>= extractAssistantContent
                          output = mdConcat (catMaybes
                            [ Just (mdTextSection "Request URL" requestUrl)
                            , Just (mdCodeSection "Request" "json" prettyRequest)
                            , Just (mdCodeSection "Response" "json" prettyResponse)
                            , fmap (mdTextSection "Assistant") assistantContent
                            ])
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
              params = emptyResponsesParams
          modify (\s -> s
            { statusText = "Running responses runner..."
            , outputText = ""
            , runnerStarted = True
            })
          let chan = eventChan st
          void $ liftIO $ forkIO $ do
            let modelId = resolveModelId provider modelOverride
                reqBody = buildResponsesRequest modelId params inputText
                reqJsonText = encodeJsonText reqBody
                requestUrl = buildResponsesUrl (base_url provider)
            result <- runResponses provider apiKey modelOverride params inputText
            let (statusMsg, outputMsg) =
                  case result of
                    Left err ->
                      let prettyRequest = prettyJson reqJsonText
                          output = mdConcat
                            [ mdTextSection "Request URL" requestUrl
                            , mdCodeSection "Request" "json" prettyRequest
                            , mdTextSection "Error" err
                            ]
                      in ("Responses runner failed.", output)
                    Right rr ->
                      let prettyRequest = prettyJson (requestJson rr)
                          prettyResponse = prettyJson (responseJson rr)
                          assistantContent = response rr >>= \r -> output_text r
                          output = mdConcat (catMaybes
                            [ Just (mdTextSection "Request URL" requestUrl)
                            , Just (mdCodeSection "Request" "json" prettyRequest)
                            , Just (mdCodeSection "Response" "json" prettyResponse)
                            , fmap (mdTextSection "Assistant") assistantContent
                            ])
                      in ("Responses runner completed.", output)
            writeBChan chan (ResponsesCompleted statusMsg outputMsg)

extractAssistantContent :: ChatCompletionResponse -> Maybe Text
extractAssistantContent ChatCompletionResponse{choices} =
  case choices of
    (ChatChoice{message = Just Message{content}}:_) -> Just (messageContentText content)
    _ -> Nothing

encodeJsonText :: ToJSON a => a -> Text
encodeJsonText = TE.decodeUtf8Lenient . BL.toStrict . encode

selectBaseUrl :: Provider -> Bool -> Text
selectBaseUrl Provider{base_url, beta_base_url} useBeta =
  let beta = beta_base_url >>= nonEmpty
  in if useBeta then fromMaybe base_url beta else base_url
  where
    nonEmpty t =
      let trimmed = T.strip t
      in if T.null trimmed then Nothing else Just trimmed

buildResponsesRequest :: Text -> ResponsesParams -> Text -> ResponsesRequest
buildResponsesRequest modelId params inputText =
  ResponsesRequest
    { model = modelId
    , input = Aeson.String inputText
    , stream = Nothing
    , max_output_tokens = paramMaxOutputTokens params
    , temperature = paramTemperature params
    , top_p = paramTopP params
    , user = paramUser params
    , response_format = paramResponseFormat params
    }

providerModels :: Config -> Text -> [Text]
providerModels cfg providerName =
  case M.lookup providerName (providers cfg) of
    Nothing -> []
    Just Provider{chat_model_id, coder_model_id, reasoner_model_id} ->
      List.nub [chat_model_id, coder_model_id, reasoner_model_id]
