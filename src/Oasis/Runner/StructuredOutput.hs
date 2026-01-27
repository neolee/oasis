module Oasis.Runner.StructuredOutput
  ( StructuredOutputResult(..)
  , buildStructuredRequest
  , runStructuredOutput
  , runStructuredOutputDetailed
  , streamStructuredOutputWithRequest
  , streamStructuredOutputWithRequestWithHooks
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI
  ( streamChatCompletionWithRequestWithHooks
  , renderClientError
  )
import Oasis.Client.OpenAI.Hooks (ClientHooks(..), emptyClientHooks)
import Oasis.Client.OpenAI.Types
  ( ChatCompletionRequest(..)
  , ChatCompletionStreamChunk(..)
  , defaultChatRequest
  , setChatStream
  , setChatResponseFormat
  )
import Oasis.Model (resolveModelId)
import Oasis.Client.OpenAI.Param (ChatParams, applyChatParams)
import Oasis.Runner.Stream (forEachDeltaContent)
import Data.Aeson (Value, decode, encode)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL

data StructuredOutputResult = StructuredOutputResult
  { rawText :: Text
  , parsedJson :: Either Text Text
  } deriving (Show, Eq)
runStructuredOutput :: Provider -> Text -> Maybe Text -> ChatParams -> [Message] -> Value -> Bool -> IO (Either Text StructuredOutputResult)
runStructuredOutput = runStructuredOutputDetailed

runStructuredOutputDetailed :: Provider -> Text -> Maybe Text -> ChatParams -> [Message] -> Value -> Bool -> IO (Either Text StructuredOutputResult)
runStructuredOutputDetailed provider apiKey modelOverride params messages responseFormat useBeta = do
  let modelId = resolveModelId provider modelOverride
      reqBody = buildStructuredRequest modelId params messages responseFormat
  streamStructuredOutputWithRequest provider apiKey reqBody (\_ -> pure ()) useBeta

buildStructuredRequest :: Text -> ChatParams -> [Message] -> Value -> ChatCompletionRequest
buildStructuredRequest modelId params messages responseFormat =
  let reqBase = defaultChatRequest modelId messages
      reqBaseStream = setChatStream True reqBase
      reqBaseFormat = setChatResponseFormat (Just responseFormat) reqBaseStream
  in applyChatParams params reqBaseFormat

streamStructuredOutputWithRequest
  :: Provider
  -> Text
  -> ChatCompletionRequest
  -> (Text -> IO ())
  -> Bool
  -> IO (Either Text StructuredOutputResult)
streamStructuredOutputWithRequest = streamStructuredOutputWithRequestWithHooks emptyClientHooks

streamStructuredOutputWithRequestWithHooks
  :: ClientHooks
  -> Provider
  -> Text
  -> ChatCompletionRequest
  -> (Text -> IO ())
  -> Bool
  -> IO (Either Text StructuredOutputResult)
streamStructuredOutputWithRequestWithHooks hooks provider apiKey reqBody onUpdate useBeta = do
  accumRef <- newIORef ""
  result <- streamChatCompletionWithRequestWithHooks hooks provider apiKey reqBody (handleChunk accumRef onUpdate) useBeta
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

handleChunk :: IORef Text -> (Text -> IO ()) -> ChatCompletionStreamChunk -> IO ()
handleChunk accumRef onUpdate chunk =
  forEachDeltaContent chunk $ \t -> do
    modifyIORef' accumRef (<> t)
    updated <- readIORef accumRef
    onUpdate updated
