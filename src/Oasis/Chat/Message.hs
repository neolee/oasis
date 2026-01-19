module Oasis.Chat.Message
  ( systemMessage
  , userMessage
  , assistantMessage
  , toolMessage
  , toolCallMessage
  , plainMessage
  ) where

import Relude
import Oasis.Types

plainMessage :: Text -> Text -> Message
plainMessage roleText txt = Message
  { role = roleText
  , content = ContentText txt
  , tool_call_id = Nothing
  , tool_calls = Nothing
  , prefix = Nothing
  , partial = Nothing
  }

systemMessage :: Text -> Message
systemMessage = plainMessage "system"

userMessage :: Text -> Message
userMessage = plainMessage "user"

assistantMessage :: Text -> Message
assistantMessage = plainMessage "assistant"

toolMessage :: Text -> Text -> Message
toolMessage toolCallId txt = (plainMessage "tool" txt)
  { tool_call_id = Just toolCallId
  }

toolCallMessage :: [ToolCall] -> Message
toolCallMessage calls = (plainMessage "assistant" "")
  { tool_calls = Just calls
  }
