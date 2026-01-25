module Oasis.Runner.PrefixCompletion
  ( PrefixCompletionResult
  , runPrefixCompletion
  , runPrefixCompletionDetailed
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI
import Oasis.Model (resolveModelId)
import Oasis.Client.OpenAI.Param (ChatParams, applyChatParams)
import Oasis.Runner.Result (encodeRequestJson, buildRequestResponse)

type PrefixCompletionResult = RequestResponse ChatCompletionResponse

runPrefixCompletion :: Provider -> Text -> Maybe Text -> ChatParams -> [Message] -> Bool -> IO (Either Text PrefixCompletionResult)
runPrefixCompletion = runPrefixCompletionDetailed

runPrefixCompletionDetailed :: Provider -> Text -> Maybe Text -> ChatParams -> [Message] -> Bool -> IO (Either Text PrefixCompletionResult)
runPrefixCompletionDetailed provider apiKey modelOverride params messages useBeta = do
  let modelId = resolveModelId provider modelOverride
      reqBase = ChatCompletionRequest
        { model = modelId
        , messages = messages
        , temperature = Nothing
        , top_p = Nothing
        , max_completion_tokens = Nothing
        , stop = Just (StopList ["```"])
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
      reqBody = applyChatParams params reqBase
      reqJsonText = encodeRequestJson reqBody
  result <- sendChatCompletionRawWithHooks emptyClientHooks provider apiKey reqBody useBeta
  pure (buildRequestResponse reqJsonText result)
