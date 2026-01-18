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
  , ErrorDetail(..)
  , ErrorResponse(..)
  , ClientError(..)
  , sendChatCompletion
  , sendChatCompletionRaw
  , streamChatCompletion
  , streamChatCompletionWithRequest
  , buildChatUrl
  , buildModelsUrl
  , buildEmbeddingsUrl
  , buildResponsesUrl
  , sendEmbeddings
  , sendEmbeddingsRaw
  , sendEmbeddingsRawWithHooks
  , sendResponses
  , sendResponsesRaw
  , sendModelsRaw
  , sendModelsRawWithHooks
  , sendChatCompletionRawWithHooks
  , renderClientError
  , ClientHooks(..)
  , emptyClientHooks
  , streamChatCompletionWithRequestWithHooks
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI.Types
import Oasis.Client.OpenAI.Http
import Oasis.Client.OpenAI.Stream
import Data.Aeson
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

sendChatCompletion :: Provider -> Text -> Text -> [Message] -> IO (Either ClientError ChatCompletionResponse)
sendChatCompletion provider apiKey modelId msgs = do
  let url = buildChatUrl (base_url provider)
      reqBody = defaultChatRequest modelId msgs
  resp <- sendChatCompletionRaw provider apiKey reqBody
  case resp of
    Left err -> pure (Left err)
    Right body ->
      case eitherDecode body of
        Left err ->
          let raw = TE.decodeUtf8Lenient (BL.toStrict body)
          in pure $ Left (ClientError 0 "DecodeError" Nothing Nothing ("Failed to decode response: " <> toText err <> "\nRaw: " <> raw))
        Right val -> pure $ Right val

sendChatCompletionRaw :: Provider -> Text -> ChatCompletionRequest -> IO (Either ClientError BL.ByteString)
sendChatCompletionRaw provider apiKey reqBody = do
  sendChatCompletionRawWithHooks emptyClientHooks provider apiKey reqBody

sendChatCompletionRawWithHooks :: ClientHooks -> Provider -> Text -> ChatCompletionRequest -> IO (Either ClientError BL.ByteString)
sendChatCompletionRawWithHooks hooks provider apiKey reqBody = do
  manager <- newTlsManager
  let url = buildChatUrl (base_url provider)
  req <- buildRequest url "POST" (RequestBodyLBS (encode reqBody)) (jsonHeaders apiKey)
  executeRequestWithHooks hooks req manager

streamChatCompletion :: Provider -> Text -> Text -> [Message] -> (ChatCompletionStreamChunk -> IO ()) -> IO (Either ClientError ())
streamChatCompletion provider apiKey modelId msgs onChunk = do
  let reqBase = defaultChatRequest modelId msgs
      reqBody = setChatStream True reqBase
  streamChatCompletionWithRequest provider apiKey reqBody onChunk

streamChatCompletionWithRequest :: Provider -> Text -> ChatCompletionRequest -> (ChatCompletionStreamChunk -> IO ()) -> IO (Either ClientError ())
streamChatCompletionWithRequest provider apiKey reqBody onChunk = do
  streamChatCompletionWithRequestWithHooks emptyClientHooks provider apiKey reqBody onChunk

streamChatCompletionWithRequestWithHooks :: ClientHooks -> Provider -> Text -> ChatCompletionRequest -> (ChatCompletionStreamChunk -> IO ()) -> IO (Either ClientError ())
streamChatCompletionWithRequestWithHooks hooks provider apiKey reqBody onChunk = do
  manager <- newTlsManager
  let url = buildChatUrl (base_url provider)
  req <- buildRequest url "POST" (RequestBodyLBS (encode reqBody)) (sseHeaders apiKey)
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
        streamSseData reader handlePayload
  where
    handlePayload payload =
      case eitherDecode (BL.fromStrict payload) of
        Left err ->
          let raw = TE.decodeUtf8Lenient payload
          in pure $ Left (ClientError 0 "StreamDecodeError" Nothing Nothing ("Failed to decode stream chunk: " <> toText err <> "\nRaw: " <> raw))
        Right chunk -> do
          onChunk chunk
          pure (Right ())

sendModelsRaw :: Provider -> Text -> IO (Either ClientError BL.ByteString)
sendModelsRaw provider apiKey = do
  sendModelsRawWithHooks emptyClientHooks provider apiKey

sendModelsRawWithHooks :: ClientHooks -> Provider -> Text -> IO (Either ClientError BL.ByteString)
sendModelsRawWithHooks hooks provider apiKey = do
  manager <- newTlsManager
  let url = buildModelsUrl (base_url provider)
  req <- buildRequest url "GET" (RequestBodyBS BS.empty) (modelsHeaders apiKey)
  executeRequestWithHooks hooks req manager

sendEmbeddings :: Provider -> Text -> EmbeddingRequest -> IO (Either ClientError EmbeddingResponse)
sendEmbeddings provider apiKey reqBody = do
  resp <- sendEmbeddingsRaw provider apiKey reqBody
  case resp of
    Left err -> pure (Left err)
    Right body ->
      case eitherDecode body of
        Left err ->
          let raw = TE.decodeUtf8Lenient (BL.toStrict body)
          in pure $ Left (ClientError 0 "DecodeError" Nothing Nothing ("Failed to decode response: " <> toText err <> "\nRaw: " <> raw))
        Right val -> pure $ Right val

sendEmbeddingsRaw :: Provider -> Text -> EmbeddingRequest -> IO (Either ClientError BL.ByteString)
sendEmbeddingsRaw provider apiKey reqBody = do
  sendEmbeddingsRawWithHooks emptyClientHooks provider apiKey reqBody

sendEmbeddingsRawWithHooks :: ClientHooks -> Provider -> Text -> EmbeddingRequest -> IO (Either ClientError BL.ByteString)
sendEmbeddingsRawWithHooks hooks provider apiKey reqBody = do
  manager <- newTlsManager
  let url = buildEmbeddingsUrl (base_url provider)
  req <- buildRequest url "POST" (RequestBodyLBS (encode reqBody)) (jsonHeaders apiKey)
  executeRequestWithHooks hooks req manager

sendResponses :: Provider -> Text -> ResponsesRequest -> IO (Either ClientError ResponsesResponse)
sendResponses provider apiKey reqBody = do
  resp <- sendResponsesRaw provider apiKey reqBody
  case resp of
    Left err -> pure (Left err)
    Right body ->
      case eitherDecode body of
        Left err ->
          let raw = TE.decodeUtf8Lenient (BL.toStrict body)
          in pure $ Left (ClientError 0 "DecodeError" Nothing Nothing ("Failed to decode response: " <> toText err <> "\nRaw: " <> raw))
        Right val -> pure $ Right val

sendResponsesRaw :: Provider -> Text -> ResponsesRequest -> IO (Either ClientError BL.ByteString)
sendResponsesRaw provider apiKey reqBody = do
  manager <- newTlsManager
  let url = buildResponsesUrl (base_url provider)
  req <- buildRequest url "POST" (RequestBodyLBS (encode reqBody)) (jsonHeaders apiKey)
  executeRequest req manager

renderClientError :: ClientError -> Text
renderClientError ClientError{status, statusText, requestId, errorResponse, rawBody} =
  let header = "HTTP " <> show status <> " " <> statusText
      reqLine = maybe "" ("\nRequest-Id: " <>) requestId
      errLine = case errorResponse of
        Nothing -> ""
        Just ErrorResponse{error = ErrorDetail{message, type_, code}} ->
          let typeLine = maybe "" ("\nType: " <>) type_
              codeLine = maybe "" ("\nCode: " <>) code
          in "\nError: " <> message <> typeLine <> codeLine
      rawLine = if rawBody == "" then "" else "\nRaw: " <> rawBody
  in header <> reqLine <> errLine <> rawLine
