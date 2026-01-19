{-# LANGUAGE StrictData #-}

module Oasis.Client.OpenAI
  ( ChatCompletionRequest(..)
  , ChatCompletionResponse(..)
  , ChatChoice(..)
  , Usage(..)
  , ChatCompletionStreamChunk(..)
  , StreamChoice(..)
  , StreamDelta(..)
  , defaultChatRequest
  , EmbeddingRequest(..)
  , EmbeddingResponse(..)
  , EmbeddingData(..)
  , EmbeddingUsage(..)
  , ResponsesRequest(..)
  , ResponsesResponse(..)
  , CompletionRequest(..)
  , CompletionResponse(..)
  , CompletionChoice(..)
  , ErrorDetail(..)
  , ErrorResponse(..)
  , ClientError(..)
  , sendChatCompletion
  , sendChatCompletionRaw
  , streamChatCompletion
  , streamChatCompletionWithRequest
  , sendCompletions
  , sendCompletionsRaw
  , sendCompletionsRawWithHooks
  , sendCompletionsRawWithManager
  , buildChatUrl
  , buildModelsUrl
  , buildEmbeddingsUrl
  , buildResponsesUrl
  , buildCompletionsUrl
  , sendEmbeddings
  , sendEmbeddingsRaw
  , sendEmbeddingsRawWithHooks
  , sendEmbeddingsRawWithManager
  , sendResponses
  , sendResponsesRaw
  , sendResponsesRawWithHooks
  , sendResponsesRawWithManager
  , sendModelsRaw
  , sendModelsRawWithHooks
  , sendModelsRawWithManager
  , sendChatCompletionRawWithHooks
  , sendChatCompletionRawWithManager
  , renderClientError
  , ClientHooks(..)
  , emptyClientHooks
  , streamChatCompletionWithRequestWithHooks
  , streamChatCompletionWithRequestWithManager
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI.Types
import Oasis.Client.OpenAI.Http
import Oasis.Client.OpenAI.Request
import Oasis.Client.OpenAI.Stream
import Data.Aeson
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

sendChatCompletion :: Provider -> Text -> Text -> [Message] -> IO (Either ClientError ChatCompletionResponse)
sendChatCompletion provider apiKey modelId msgs = do
  let reqBody = defaultChatRequest modelId msgs
  resp <- sendChatCompletionRaw provider apiKey reqBody
  case resp of
    Left err -> pure (Left err)
    Right body -> pure (decodeOrError body)

sendChatCompletionRaw :: Provider -> Text -> ChatCompletionRequest -> IO (Either ClientError BL.ByteString)
sendChatCompletionRaw provider apiKey reqBody = do
  sendChatCompletionRawWithHooks emptyClientHooks provider apiKey reqBody False

sendChatCompletionRawWithHooks :: ClientHooks -> Provider -> Text -> ChatCompletionRequest -> Bool -> IO (Either ClientError BL.ByteString)
sendChatCompletionRawWithHooks hooks provider apiKey reqBody useBeta = do
  manager <- newTlsManager
  sendChatCompletionRawWithManager manager hooks provider apiKey reqBody useBeta

sendChatCompletionRawWithManager :: Manager -> ClientHooks -> Provider -> Text -> ChatCompletionRequest -> Bool -> IO (Either ClientError BL.ByteString)
sendChatCompletionRawWithManager manager hooks provider apiKey reqBody useBeta = do
  let baseUrl = if useBeta then fromMaybe (base_url provider) (beta_base_url provider) else base_url provider
      url = buildChatUrl baseUrl
  req <- buildJsonRequest url "POST" (encode reqBody) apiKey
  executeRequestWithHooks hooks req manager

streamChatCompletion :: Provider -> Text -> Text -> [Message] -> (ChatCompletionStreamChunk -> IO ()) -> IO (Either ClientError ())
streamChatCompletion provider apiKey modelId msgs onChunk = do
  let reqBase = defaultChatRequest modelId msgs
      reqBody = setChatStream True reqBase
  streamChatCompletionWithRequest provider apiKey reqBody onChunk

streamChatCompletionWithRequest :: Provider -> Text -> ChatCompletionRequest -> (ChatCompletionStreamChunk -> IO ()) -> IO (Either ClientError ())
streamChatCompletionWithRequest provider apiKey reqBody onChunk = do
  streamChatCompletionWithRequestWithHooks emptyClientHooks provider apiKey reqBody onChunk False

streamChatCompletionWithRequestWithHooks :: ClientHooks -> Provider -> Text -> ChatCompletionRequest -> (ChatCompletionStreamChunk -> IO ()) -> Bool -> IO (Either ClientError ())
streamChatCompletionWithRequestWithHooks hooks provider apiKey reqBody onChunk useBeta = do
  manager <- newTlsManager
  streamChatCompletionWithRequestWithManager manager hooks provider apiKey reqBody onChunk useBeta

streamChatCompletionWithRequestWithManager :: Manager -> ClientHooks -> Provider -> Text -> ChatCompletionRequest -> (ChatCompletionStreamChunk -> IO ()) -> Bool -> IO (Either ClientError ())
streamChatCompletionWithRequestWithManager manager hooks provider apiKey reqBody onChunk useBeta = do
  let baseUrl = if useBeta then fromMaybe (base_url provider) (beta_base_url provider) else base_url provider
      url = buildChatUrl baseUrl
  req <- buildSseRequest url (encode reqBody) apiKey
  forM_ (onRequest hooks) ($ req)
  withResponse req manager $ \resp -> do
    let status = responseStatus resp
        headers = responseHeaders resp
    if statusCode status < 200 || statusCode status >= 300
      then do
        chunks <- brConsume (responseBody resp)
        let body = BL.fromChunks chunks
            err = clientErrorFromResponse status headers body
        forM_ (onError hooks) ($ err)
        pure (Left err)
      else do
        forM_ (onResponse hooks) (\f -> f status headers BL.empty)
        let reader = responseBody resp
        streamSseEvents reader handleEvent
  where
    handleEvent event =
      case sseEventData event of
        Nothing -> pure (Right ())
        Just payload ->
          case eitherDecode (BL.fromStrict payload) of
            Left err ->
              let raw = TE.decodeUtf8Lenient payload
              in pure $ Left (ClientError 0 "StreamDecodeError" Nothing Nothing ("Failed to decode stream chunk: " <> toText err <> "\nRaw: " <> raw))
            Right chunk -> do
              onChunk chunk
              pure (Right ())

sendCompletions :: Provider -> Text -> CompletionRequest -> IO (Either ClientError CompletionResponse)
sendCompletions provider apiKey reqBody = do
  resp <- sendCompletionsRaw provider apiKey reqBody
  case resp of
    Left err -> pure (Left err)
    Right body -> pure (decodeOrError body)

sendCompletionsRaw :: Provider -> Text -> CompletionRequest -> IO (Either ClientError BL.ByteString)
sendCompletionsRaw provider apiKey reqBody = do
  sendCompletionsRawWithHooks emptyClientHooks provider apiKey reqBody

sendCompletionsRawWithHooks :: ClientHooks -> Provider -> Text -> CompletionRequest -> IO (Either ClientError BL.ByteString)
sendCompletionsRawWithHooks hooks provider apiKey reqBody = do
  manager <- newTlsManager
  sendCompletionsRawWithManager manager hooks provider apiKey reqBody

sendCompletionsRawWithManager :: Manager -> ClientHooks -> Provider -> Text -> CompletionRequest -> IO (Either ClientError BL.ByteString)
sendCompletionsRawWithManager manager hooks provider apiKey reqBody = do
  -- Completion API usually defaults to beta base url if it's FIM
  let baseUrl = fromMaybe (base_url provider) (beta_base_url provider)
      url = buildCompletionsUrl baseUrl
  req <- buildJsonRequest url "POST" (encode reqBody) apiKey
  executeRequestWithHooks hooks req manager

sendModelsRaw :: Provider -> Text -> IO (Either ClientError BL.ByteString)
sendModelsRaw provider apiKey = do
  sendModelsRawWithHooks emptyClientHooks provider apiKey

sendModelsRawWithHooks :: ClientHooks -> Provider -> Text -> IO (Either ClientError BL.ByteString)
sendModelsRawWithHooks hooks provider apiKey = do
  manager <- newTlsManager
  sendModelsRawWithManager manager hooks provider apiKey

sendModelsRawWithManager :: Manager -> ClientHooks -> Provider -> Text -> IO (Either ClientError BL.ByteString)
sendModelsRawWithManager manager hooks provider apiKey = do
  let url = buildModelsUrl (base_url provider)
  req <- buildGetRequest url apiKey
  executeRequestWithHooks hooks req manager

sendEmbeddings :: Provider -> Text -> EmbeddingRequest -> IO (Either ClientError EmbeddingResponse)
sendEmbeddings provider apiKey reqBody = do
  resp <- sendEmbeddingsRaw provider apiKey reqBody
  case resp of
    Left err -> pure (Left err)
    Right body -> pure (decodeOrError body)

sendEmbeddingsRaw :: Provider -> Text -> EmbeddingRequest -> IO (Either ClientError BL.ByteString)
sendEmbeddingsRaw provider apiKey reqBody = do
  sendEmbeddingsRawWithHooks emptyClientHooks provider apiKey reqBody

sendEmbeddingsRawWithHooks :: ClientHooks -> Provider -> Text -> EmbeddingRequest -> IO (Either ClientError BL.ByteString)
sendEmbeddingsRawWithHooks hooks provider apiKey reqBody = do
  manager <- newTlsManager
  sendEmbeddingsRawWithManager manager hooks provider apiKey reqBody

sendEmbeddingsRawWithManager :: Manager -> ClientHooks -> Provider -> Text -> EmbeddingRequest -> IO (Either ClientError BL.ByteString)
sendEmbeddingsRawWithManager manager hooks provider apiKey reqBody = do
  let url = buildEmbeddingsUrl (base_url provider)
  req <- buildJsonRequest url "POST" (encode reqBody) apiKey
  executeRequestWithHooks hooks req manager

sendResponses :: Provider -> Text -> ResponsesRequest -> IO (Either ClientError ResponsesResponse)
sendResponses provider apiKey reqBody = do
  resp <- sendResponsesRaw provider apiKey reqBody
  case resp of
    Left err -> pure (Left err)
    Right body -> pure (decodeOrError body)

sendResponsesRaw :: Provider -> Text -> ResponsesRequest -> IO (Either ClientError BL.ByteString)
sendResponsesRaw provider apiKey reqBody = do
  sendResponsesRawWithHooks emptyClientHooks provider apiKey reqBody

sendResponsesRawWithHooks :: ClientHooks -> Provider -> Text -> ResponsesRequest -> IO (Either ClientError BL.ByteString)
sendResponsesRawWithHooks hooks provider apiKey reqBody = do
  manager <- newTlsManager
  sendResponsesRawWithManager manager hooks provider apiKey reqBody

sendResponsesRawWithManager :: Manager -> ClientHooks -> Provider -> Text -> ResponsesRequest -> IO (Either ClientError BL.ByteString)
sendResponsesRawWithManager manager hooks provider apiKey reqBody = do
  let url = buildResponsesUrl (base_url provider)
  req <- buildJsonRequest url "POST" (encode reqBody) apiKey
  executeRequestWithHooks hooks req manager

decodeOrError :: FromJSON a => BL.ByteString -> Either ClientError a
decodeOrError body =
  case eitherDecode body of
    Left err ->
      let raw = TE.decodeUtf8Lenient (BL.toStrict body)
      in Left (ClientError 0 "DecodeError" Nothing Nothing ("Failed to decode response: " <> toText err <> "\nRaw: " <> raw))
    Right val -> Right val
