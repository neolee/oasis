module Oasis.Runner.Chat
  ( ChatOptions(..)
  , runChat
  , handleStreamChunkContentOnly
  , selectModelId
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI
import qualified Data.Text as T

data ChatOptions = ChatOptions
  { streaming :: Bool
  } deriving (Show, Eq)

selectModelId :: Provider -> Text
selectModelId Provider{..} =
  case fmap T.toLower default_model_type of
    Just "reasoner" | not (T.null reasoner_model_id) -> reasoner_model_id
    Just "coder"    | not (T.null coder_model_id)    -> coder_model_id
    _                                             -> chat_model_id

runChat :: Provider -> Text -> ChatOptions -> [Message] -> (ChatCompletionStreamChunk -> IO ()) -> IO (Either Text (Maybe ChatCompletionResponse))
runChat provider apiKey ChatOptions{streaming} messages onChunk = do
  let modelId = selectModelId provider
  if streaming
    then do
      result <- streamChatCompletion provider apiKey modelId messages onChunk
      pure (fmap (const Nothing) result)
    else do
      result <- sendChatCompletion provider apiKey modelId messages
      pure (fmap Just result)

handleStreamChunkContentOnly :: (Text -> IO ()) -> ChatCompletionStreamChunk -> IO ()
handleStreamChunkContentOnly onToken ChatCompletionStreamChunk{choices = streamChoices} =
  forM_ streamChoices $ \c ->
    forM_ (delta c) $ \d ->
      case d of
        StreamDelta{content = deltaContent} ->
          forM_ deltaContent onToken
