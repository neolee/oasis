{-# LANGUAGE StrictData #-}

module Oasis.Client.OpenAI
  ( ChatCompletionRequest(..)
  , ChatCompletionResponse(..)
  , ChatChoice(..)
  , Usage(..)
  , ChatCompletionStreamChunk(..)
  , StreamChoice(..)
  , StreamDelta(..)
  , sendChatCompletion
  , sendChatCompletionRaw
  , streamChatCompletion
  , streamChatCompletionWithRequest
  , buildChatUrl
  , buildModelsUrl
  , sendModelsRaw
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI.Types
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (HeaderName, hAccept, hAuthorization, hContentType)

buildChatUrl :: Text -> Text
buildChatUrl baseUrl =
  let trimmed = T.dropWhileEnd (== '/') baseUrl
      lower   = T.toLower trimmed
      versionSuffixes = ["/v1", "/v2", "/v3", "/v4", "/v5"]
      hasVersionSuffix = any (`T.isSuffixOf` lower) versionSuffixes
      pathSuffix = if hasVersionSuffix
                     then "/chat/completions"
                     else "/v1/chat/completions"
  in trimmed <> pathSuffix

buildModelsUrl :: Text -> Text
buildModelsUrl baseUrl =
  let trimmed = T.dropWhileEnd (== '/') baseUrl
      lower   = T.toLower trimmed
      versionSuffixes = ["/v1", "/v2", "/v3", "/v4", "/v5"]
      hasVersionSuffix = any (`T.isSuffixOf` lower) versionSuffixes
      pathSuffix = if hasVersionSuffix
                     then "/models"
                     else "/v1/models"
  in trimmed <> pathSuffix

sendChatCompletion :: Provider -> Text -> Text -> [Message] -> IO (Either Text ChatCompletionResponse)
sendChatCompletion provider apiKey modelId msgs = do
  let url = buildChatUrl (base_url provider)
      reqBody = ChatCompletionRequest
        { model = modelId
        , messages = msgs
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
        , stream = False
        , response_format = Nothing
        , tools = Nothing
        , tool_choice = Nothing
        , parallel_tool_calls = Nothing
        }
  resp <- sendChatCompletionRaw provider apiKey reqBody
  case resp of
    Left err -> pure (Left err)
    Right body ->
      case eitherDecode body of
        Left err ->
          let raw = TE.decodeUtf8Lenient (BL.toStrict body)
          in pure $ Left ("Failed to decode response: " <> toText err <> "\nRaw: " <> raw)
        Right val -> pure $ Right val

sendChatCompletionRaw :: Provider -> Text -> ChatCompletionRequest -> IO (Either Text BL.ByteString)
sendChatCompletionRaw provider apiKey reqBody = do
  manager <- newManager tlsManagerSettings
  let url = buildChatUrl (base_url provider)
  initReq <- parseRequest (toString url)
  let headers =
        [ (hContentType, "application/json")
        , (hAccept, "application/json")
        ] <> authHeader apiKey
      req = initReq
        { method = "POST"
        , requestBody = RequestBodyLBS (encode reqBody)
        , requestHeaders = headers
        }
  resp <- httpLbs req manager
  pure (Right (responseBody resp))

streamChatCompletion :: Provider -> Text -> Text -> [Message] -> (ChatCompletionStreamChunk -> IO ()) -> IO (Either Text ())
streamChatCompletion provider apiKey modelId msgs onChunk = do
  let reqBody = ChatCompletionRequest
        { model = modelId
        , messages = msgs
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
        , response_format = Nothing
      , tools = Nothing
      , tool_choice = Nothing
      , parallel_tool_calls = Nothing
      }
  streamChatCompletionWithRequest provider apiKey reqBody onChunk

streamChatCompletionWithRequest :: Provider -> Text -> ChatCompletionRequest -> (ChatCompletionStreamChunk -> IO ()) -> IO (Either Text ())
streamChatCompletionWithRequest provider apiKey reqBody onChunk = do
  manager <- newManager tlsManagerSettings
  let url = buildChatUrl (base_url provider)
  initReq <- parseRequest (toString url)
  let headers =
        [ (hContentType, "application/json")
        , (hAccept, "text/event-stream")
        ] <> authHeader apiKey
      req = initReq
        { method = "POST"
        , requestBody = RequestBodyLBS (encode reqBody)
        , requestHeaders = headers
        }
  withResponse req manager $ \resp -> do
    let reader = responseBody resp
    streamLoop reader BS.empty
  where
    streamLoop reader buffer = do
      chunk <- brRead reader
      if BS.null chunk
        then pure (Right ())
        else do
          let combined = buffer <> chunk
              (lines, rest) = splitLines combined
          result <- processLines lines
          case result of
            Left err -> pure (Left err)
            Right done ->
              if done
                then pure (Right ())
                else streamLoop reader rest

    splitLines bs
      | BS.null bs = ([], BS.empty)
      | BS8.last bs == '\n' = (BS8.split '\n' bs, BS.empty)
      | otherwise =
          let parts = BS8.split '\n' bs
          in case reverse parts of
               [] -> ([], BS.empty)
               (lastPart:revInit) -> (reverse revInit, lastPart)

    processLines [] = pure (Right False)
    processLines (l:ls) = do
      let line = BS8.dropWhile isSpace l
      if BS8.null line
        then processLines ls
        else if BS8.isPrefixOf "data:" line
          then do
            let payload = BS8.dropWhile isSpace (BS8.drop 5 line)
            if payload == "[DONE]"
              then pure (Right True)
              else case eitherDecode (BL.fromStrict payload) of
                Left err ->
                  let raw = TE.decodeUtf8Lenient payload
                  in pure $ Left ("Failed to decode stream chunk: " <> toText err <> "\nRaw: " <> raw)
                Right chunk -> do
                  onChunk chunk
                  processLines ls
          else processLines ls

authHeader :: Text -> [(HeaderName, BS8.ByteString)]
authHeader apiKey
  | T.null apiKey = []
  | otherwise    = [(hAuthorization, "Bearer " <> TE.encodeUtf8 apiKey)]

sendModelsRaw :: Provider -> Text -> IO (Either Text BL.ByteString)
sendModelsRaw provider apiKey = do
  manager <- newManager tlsManagerSettings
  let url = buildModelsUrl (base_url provider)
  initReq <- parseRequest (toString url)
  let headers =
        [ (hAccept, "application/json")
        ] <> authHeader apiKey
      req = initReq
        { method = "GET"
        , requestHeaders = headers
        }
  resp <- httpLbs req manager
  pure (Right (responseBody resp))
