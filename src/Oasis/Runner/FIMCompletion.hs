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

runFIMCompletion :: Provider -> Text -> Maybe Text -> Bool -> IO (Either Text ())
runFIMCompletion provider apiKey modelOverride useBeta = do
  detailed <- runFIMCompletionDetailed provider apiKey modelOverride useBeta
  case detailed of
    Left err -> pure (Left err)
    Right RequestResponse{response} ->
      case response of
        Just CompletionResponse { choices = (CompletionChoice{text}:_) } -> do
          putTextLn text
          pure (Right ())
        _ -> pure (Left "No completion choices returned.")

runFIMCompletionDetailed :: Provider -> Text -> Maybe Text -> Bool -> IO (Either Text FimCompletionResult)
runFIMCompletionDetailed provider apiKey modelOverride useBeta = do
  let modelId = resolveModelId provider modelOverride
      reqBody = CompletionRequest
        { model = modelId
        , prompt = "def fib(a):"
        , suffix = Just "    return fib(a-1) + fib(a-2)"
        , max_tokens = Just 128
        , temperature = Nothing
        , top_p = Nothing
        , stream = False
        , stop = Nothing
        , echo = Nothing
        , logprobs = Nothing
        }
      reqJsonText = encodeRequestJson reqBody
  result <- sendCompletionsRaw provider apiKey reqBody useBeta
  pure (buildRequestResponse reqJsonText result)
