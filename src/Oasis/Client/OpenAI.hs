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
  , setChatStream
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
  , requestChat
  , requestChatWithHooks
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
import Oasis.Client.OpenAI.Param (ChatParams, applyChatParams)
import Data.Aeson
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

selectBaseUrl :: Provider -> Bool -> Text
selectBaseUrl provider useBeta =
  let beta = beta_base_url provider >>= nonEmpty
  in if useBeta then fromMaybe (base_url provider) beta else base_url provider
  where
    nonEmpty t =
      let trimmed = T.strip t
      in if T.null trimmed then Nothing else Just trimmed

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
  let url = buildChatUrl (selectBaseUrl provider useBeta)
  req <- buildJsonRequest url "POST" (encode reqBody) apiKey
  executeRequestWithHooks hooks req manager

requestChat :: Provider -> Text -> ChatParams -> ChatCompletionRequest -> Bool -> IO (Either ClientError BL.ByteString)
requestChat provider apiKey params reqBase useBeta = do
  requestChatWithHooks emptyClientHooks provider apiKey params reqBase useBeta

requestChatWithHooks :: ClientHooks -> Provider -> Text -> ChatParams -> ChatCompletionRequest -> Bool -> IO (Either ClientError BL.ByteString)
requestChatWithHooks hooks provider apiKey params reqBase useBeta = do
  let reqBody = applyChatParams params reqBase
  sendChatCompletionRawWithHooks hooks provider apiKey reqBody useBeta

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
  let url = buildChatUrl (selectBaseUrl provider useBeta)
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

sendCompletions :: Provider -> Text -> CompletionRequest -> Bool -> IO (Either ClientError CompletionResponse)
sendCompletions provider apiKey reqBody useBeta = do
  resp <- sendCompletionsRaw provider apiKey reqBody useBeta
  case resp of
    Left err -> pure (Left err)
    Right body -> pure (decodeOrError body)

sendCompletionsRaw :: Provider -> Text -> CompletionRequest -> Bool -> IO (Either ClientError BL.ByteString)
sendCompletionsRaw provider apiKey reqBody useBeta = do
  sendCompletionsRawWithHooks emptyClientHooks provider apiKey reqBody useBeta

sendCompletionsRawWithHooks :: ClientHooks -> Provider -> Text -> CompletionRequest -> Bool -> IO (Either ClientError BL.ByteString)
sendCompletionsRawWithHooks hooks provider apiKey reqBody useBeta = do
  manager <- newTlsManager
  sendCompletionsRawWithManager manager hooks provider apiKey reqBody useBeta

sendCompletionsRawWithManager :: Manager -> ClientHooks -> Provider -> Text -> CompletionRequest -> Bool -> IO (Either ClientError BL.ByteString)
sendCompletionsRawWithManager manager hooks provider apiKey reqBody useBeta = do
  -- Completion API usually defaults to beta base url if it's FIM
  let url = buildCompletionsUrl (selectBaseUrl provider useBeta)
  req <- buildJsonRequest url "POST" (encode reqBody) apiKey
  executeRequestWithHooks hooks req manager

sendModelsRaw :: Provider -> Text -> Bool -> IO (Either ClientError BL.ByteString)
sendModelsRaw provider apiKey useBeta = do
  sendModelsRawWithHooks emptyClientHooks provider apiKey useBeta

sendModelsRawWithHooks :: ClientHooks -> Provider -> Text -> Bool -> IO (Either ClientError BL.ByteString)
sendModelsRawWithHooks hooks provider apiKey useBeta = do
  manager <- newTlsManager
  sendModelsRawWithManager manager hooks provider apiKey useBeta

sendModelsRawWithManager :: Manager -> ClientHooks -> Provider -> Text -> Bool -> IO (Either ClientError BL.ByteString)
sendModelsRawWithManager manager hooks provider apiKey useBeta = do
  let url = buildModelsUrl (selectBaseUrl provider useBeta)
  req <- buildGetRequest url apiKey
  executeRequestWithHooks hooks req manager

sendEmbeddings :: Provider -> Text -> EmbeddingRequest -> Bool -> IO (Either ClientError EmbeddingResponse)
sendEmbeddings provider apiKey reqBody useBeta = do
  resp <- sendEmbeddingsRaw provider apiKey reqBody useBeta
  case resp of
    Left err -> pure (Left err)
    Right body -> pure (decodeOrError body)

sendEmbeddingsRaw :: Provider -> Text -> EmbeddingRequest -> Bool -> IO (Either ClientError BL.ByteString)
sendEmbeddingsRaw provider apiKey reqBody useBeta = do
  sendEmbeddingsRawWithHooks emptyClientHooks provider apiKey reqBody useBeta

sendEmbeddingsRawWithHooks :: ClientHooks -> Provider -> Text -> EmbeddingRequest -> Bool -> IO (Either ClientError BL.ByteString)
sendEmbeddingsRawWithHooks hooks provider apiKey reqBody useBeta = do
  manager <- newTlsManager
  sendEmbeddingsRawWithManager manager hooks provider apiKey reqBody useBeta

sendEmbeddingsRawWithManager :: Manager -> ClientHooks -> Provider -> Text -> EmbeddingRequest -> Bool -> IO (Either ClientError BL.ByteString)
sendEmbeddingsRawWithManager manager hooks provider apiKey reqBody useBeta = do
  let url = buildEmbeddingsUrl (selectBaseUrl provider useBeta)
  req <- buildJsonRequest url "POST" (encode reqBody) apiKey
  executeRequestWithHooks hooks req manager

sendResponses :: Provider -> Text -> ResponsesRequest -> Bool -> IO (Either ClientError ResponsesResponse)
sendResponses provider apiKey reqBody useBeta = do
  resp <- sendResponsesRaw provider apiKey reqBody useBeta
  case resp of
    Left err -> pure (Left err)
    Right body -> pure (decodeOrError body)

sendResponsesRaw :: Provider -> Text -> ResponsesRequest -> Bool -> IO (Either ClientError BL.ByteString)
sendResponsesRaw provider apiKey reqBody useBeta = do
  sendResponsesRawWithHooks emptyClientHooks provider apiKey reqBody useBeta

sendResponsesRawWithHooks :: ClientHooks -> Provider -> Text -> ResponsesRequest -> Bool -> IO (Either ClientError BL.ByteString)
sendResponsesRawWithHooks hooks provider apiKey reqBody useBeta = do
  manager <- newTlsManager
  sendResponsesRawWithManager manager hooks provider apiKey reqBody useBeta

sendResponsesRawWithManager :: Manager -> ClientHooks -> Provider -> Text -> ResponsesRequest -> Bool -> IO (Either ClientError BL.ByteString)
sendResponsesRawWithManager manager hooks provider apiKey reqBody useBeta = do
  let url = buildResponsesUrl (selectBaseUrl provider useBeta)
  req <- buildJsonRequest url "POST" (encode reqBody) apiKey
  executeRequestWithHooks hooks req manager

decodeOrError :: FromJSON a => BL.ByteString -> Either ClientError a
decodeOrError body =
  case eitherDecode body of
    Left err ->
      let raw = TE.decodeUtf8Lenient (BL.toStrict body)
      in Left (ClientError 0 "DecodeError" Nothing Nothing ("Failed to decode response: " <> toText err <> "\nRaw: " <> raw))
    Right val -> Right val
