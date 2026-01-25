module Oasis.Runner.FIMCompletion
  ( FimCompletionResult
  , runFIMCompletion
  , runFIMCompletionDetailed
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI
import Oasis.Model (resolveModelId)
import Oasis.Runner.Result (encodeRequestJson, buildRequestResponse)

type FimCompletionResult = RequestResponse CompletionResponse

runFIMCompletion :: Provider -> Text -> Maybe Text -> CompletionRequest -> Bool -> IO (Either Text FimCompletionResult)
runFIMCompletion = runFIMCompletionDetailed

runFIMCompletionDetailed :: Provider -> Text -> Maybe Text -> CompletionRequest -> Bool -> IO (Either Text FimCompletionResult)
runFIMCompletionDetailed provider apiKey modelOverride reqBase useBeta = do
  let modelId = resolveModelId provider modelOverride
      CompletionRequest{prompt, suffix, max_tokens, temperature, top_p, stream, stop, echo, logprobs} = (reqBase :: CompletionRequest)
      reqBody :: CompletionRequest
      reqBody = CompletionRequest
        { model = modelId
        , prompt = prompt
        , suffix = suffix
        , max_tokens = max_tokens
        , temperature = temperature
        , top_p = top_p
        , stream = stream
        , stop = stop
        , echo = echo
        , logprobs = logprobs
        }
      reqJsonText = encodeRequestJson reqBody
  result <- sendCompletionsRaw provider apiKey reqBody useBeta
  pure (buildRequestResponse reqJsonText result)
