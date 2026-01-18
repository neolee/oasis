module Oasis.Runner.Chat
  ( runSingleTurn
  , selectModelId
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI
import qualified Data.Text as T

selectModelId :: Provider -> Text
selectModelId Provider{..} =
  case fmap T.toLower default_model_type of
    Just "reasoner" | not (T.null reasoner_model_id) -> reasoner_model_id
    Just "coder"    | not (T.null coder_model_id)    -> coder_model_id
    _                                             -> chat_model_id

runSingleTurn :: Provider -> Text -> Text -> IO (Either Text ChatCompletionResponse)
runSingleTurn provider apiKey prompt = do
  let modelId = selectModelId provider
      messages = [Message "user" prompt]
  sendChatCompletion provider apiKey modelId messages
