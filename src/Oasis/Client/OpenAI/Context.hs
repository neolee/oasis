module Oasis.Client.OpenAI.Context
  ( buildUserMessages
  , extractAssistantContent
  , extractToolCall
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI.Types (ChatCompletionResponse(..), ChatChoice(..))
import Oasis.Chat.Message (userMessage)

buildUserMessages :: Text -> [Message]
buildUserMessages prompt = [userMessage prompt]

extractAssistantContent :: ChatCompletionResponse -> Maybe Text
extractAssistantContent ChatCompletionResponse{choices} =
  case choices of
    (ChatChoice{message = Just Message{content}}:_) -> Just (messageContentText content)
    _ -> Nothing

extractToolCall :: ChatCompletionResponse -> Maybe (Message, ToolCall)
extractToolCall ChatCompletionResponse{choices} =
  case choices of
    (ChatChoice{message = Just msg@Message{tool_calls = Just (tc:_)}}:_) -> Just (msg, tc)
    _ -> Nothing
