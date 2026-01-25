module Oasis.Runner.StructuredOutput
  ( StructuredOutputResult(..)
  , runStructuredOutput
  , runStructuredOutputDetailed
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI
import Oasis.Client.OpenAI.Types (setChatStream, setChatResponseFormat)
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
      reqBase = defaultChatRequest modelId messages
      reqBaseStream = setChatStream True reqBase
      reqBaseFormat = setChatResponseFormat (Just responseFormat) reqBaseStream
      reqBody = applyChatParams params reqBaseFormat
  accumRef <- newIORef ""
  result <- streamChatCompletionWithRequestWithHooks emptyClientHooks provider apiKey reqBody (handleChunk accumRef) useBeta
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

handleChunk :: IORef Text -> ChatCompletionStreamChunk -> IO ()
handleChunk accumRef chunk =
  forEachDeltaContent chunk $ \t ->
    modifyIORef' accumRef (<> t)
