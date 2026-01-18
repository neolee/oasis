module Oasis.Chat.Message
  ( systemMessage
  , userMessage
  , assistantMessage
  , toolMessage
  , toolCallMessage
  ) where

import Relude
import Oasis.Types

systemMessage :: Text -> Message
systemMessage txt = Message "system" (ContentText txt) Nothing Nothing

userMessage :: Text -> Message
userMessage txt = Message "user" (ContentText txt) Nothing Nothing

assistantMessage :: Text -> Message
assistantMessage txt = Message "assistant" (ContentText txt) Nothing Nothing

toolMessage :: Text -> Text -> Message
toolMessage toolCallId txt = Message "tool" (ContentText txt) (Just toolCallId) Nothing

toolCallMessage :: [ToolCall] -> Message
toolCallMessage calls = Message "assistant" (ContentText "") Nothing (Just calls)
