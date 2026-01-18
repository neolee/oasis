module Oasis.Runner.Basic
  ( BasicResult
  , runBasic
  , runBasicRaw
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI
import Oasis.Runner.Common (resolveModelId, buildUserMessages, ChatParams, applyChatParams)
import Oasis.Runner.Result (RunnerResult(..), encodeRequestJson, buildRunnerResult)

type BasicResult = RunnerResult ChatCompletionResponse

runBasic :: Provider -> Text -> Maybe Text -> ChatParams -> Text -> IO (Either Text BasicResult)
runBasic provider apiKey modelOverride params prompt = do
  let modelId = resolveModelId provider modelOverride
      messages = buildUserMessages prompt
      reqBase = defaultChatRequest modelId messages
      reqBody = applyChatParams params reqBase
      reqJsonText = encodeRequestJson reqBody
  resp <- sendChatCompletionRaw provider apiKey reqBody
  pure (buildRunnerResult reqJsonText resp)

runBasicRaw :: Provider -> Text -> Maybe Text -> ChatParams -> [Message] -> IO (Either Text BasicResult)
runBasicRaw provider apiKey modelOverride params messages = do
  let modelId = resolveModelId provider modelOverride
      reqBase = defaultChatRequest modelId messages
      reqBody = applyChatParams params reqBase
      reqJsonText = encodeRequestJson reqBody
  resp <- sendChatCompletionRaw provider apiKey reqBody
  pure (buildRunnerResult reqJsonText resp)
