module Oasis.Runner.PartialMode
  ( PartialModeResult
  , runPartialMode
  , runPartialModeDetailed
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI
import Oasis.Model (resolveModelId)
import Oasis.Client.OpenAI.Param (ChatParams, applyChatParams)
import Oasis.Runner.Result (encodeRequestJson, buildRequestResponse)

type PartialModeResult = RequestResponse ChatCompletionResponse

runPartialMode :: Provider -> Text -> Maybe Text -> ChatParams -> [Message] -> Bool -> IO (Either Text PartialModeResult)
runPartialMode = runPartialModeDetailed

runPartialModeDetailed :: Provider -> Text -> Maybe Text -> ChatParams -> [Message] -> Bool -> IO (Either Text PartialModeResult)
runPartialModeDetailed provider apiKey modelOverride params messages useBeta = do
  let modelId = resolveModelId provider modelOverride
      reqBase = defaultChatRequest modelId messages
      reqBody = applyChatParams params reqBase
      reqJsonText = encodeRequestJson reqBody
  result <- sendChatCompletionRawWithHooks emptyClientHooks provider apiKey reqBody useBeta
  pure (buildRequestResponse reqJsonText result)
