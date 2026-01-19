module Oasis.Runner.FIMCompletion
  ( runFIMCompletion
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI
import Oasis.Runner.Common (resolveModelId)
import Oasis.Runner.Result (parseRawResponseStrict)

runFIMCompletion :: Provider -> Text -> Maybe Text -> IO (Either Text ())
runFIMCompletion provider apiKey modelOverride = do
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
  
  result <- sendCompletionsRaw provider apiKey reqBody
  case parseRawResponseStrict result of
    Left err -> pure (Left err)
    Right (_, response) ->
      case response of
        CompletionResponse { choices = (CompletionChoice{text}:_) } -> do
          putTextLn text
          pure (Right ())
        _ -> pure (Left "No completion choices returned.")
