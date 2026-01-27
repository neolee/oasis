module Oasis.Runner.FIMCompletion
  ( FimCompletionResult
  , buildFIMRequest
  , runFIMCompletion
  , runFIMCompletionDetailed
  , runFIMCompletionRequest
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
      reqBody = buildFIMRequest modelId reqBase
  runFIMCompletionRequest provider apiKey reqBody useBeta

buildFIMRequest :: Text -> CompletionRequest -> CompletionRequest
buildFIMRequest modelId reqBase =
  let CompletionRequest{prompt, suffix, max_tokens, temperature, top_p, stream, stop, echo, logprobs} = reqBase
  in CompletionRequest
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

runFIMCompletionRequest :: Provider -> Text -> CompletionRequest -> Bool -> IO (Either Text FimCompletionResult)
runFIMCompletionRequest provider apiKey reqBody useBeta = do
  let reqJsonText = encodeRequestJson reqBody
  result <- sendCompletionsRaw provider apiKey reqBody useBeta
  pure (buildRequestResponse reqJsonText result)
