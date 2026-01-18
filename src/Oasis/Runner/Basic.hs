module Oasis.Runner.Basic
  ( BasicResult(..)
  , runBasic
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI
import Oasis.Runner.Chat (selectModelId)

newtype BasicResult = BasicResult
  { response :: ChatCompletionResponse
  } deriving (Show, Eq)

runBasic :: Provider -> Text -> Text -> IO (Either Text BasicResult)
runBasic provider apiKey prompt = do
  let modelId = selectModelId provider
      messages = [Message "user" prompt]
  result <- sendChatCompletion provider apiKey modelId messages
  pure (fmap BasicResult result)
