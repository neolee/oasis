module Oasis.Runner.PartialMode
  ( PartialModeResult
  , buildPartialModeRequest
  , runPartialMode
  , runPartialModeDetailed
  , runPartialModeRequest
  , runPartialModeRequestWithHooks
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI
  ( sendChatCompletionRawWithHooks
  , encodeRequestJsonWithFlatExtra
  )
import Oasis.Client.OpenAI.Hooks (ClientHooks(..), emptyClientHooks)
import Oasis.Client.OpenAI.Types
  ( ChatCompletionRequest(..)
  , ChatCompletionResponse(..)
  , defaultChatRequest
  )
import Oasis.Model (resolveModelId)
import Oasis.Client.OpenAI.Param (ChatParams, applyChatParams)
import Oasis.Runner.Result (buildRequestResponse)

type PartialModeResult = RequestResponse ChatCompletionResponse

runPartialMode :: Provider -> Text -> Maybe Text -> ChatParams -> [Message] -> Bool -> IO (Either Text PartialModeResult)
runPartialMode = runPartialModeDetailed

runPartialModeDetailed :: Provider -> Text -> Maybe Text -> ChatParams -> [Message] -> Bool -> IO (Either Text PartialModeResult)
runPartialModeDetailed provider apiKey modelOverride params messages useBeta = do
  let modelId = resolveModelId provider modelOverride
      reqBody = buildPartialModeRequest modelId params messages
  runPartialModeRequestWithHooks emptyClientHooks provider apiKey reqBody useBeta

buildPartialModeRequest :: Text -> ChatParams -> [Message] -> ChatCompletionRequest
buildPartialModeRequest modelId params messages =
  applyChatParams params (defaultChatRequest modelId messages)

runPartialModeRequest :: Provider -> Text -> ChatCompletionRequest -> Bool -> IO (Either Text PartialModeResult)
runPartialModeRequest = runPartialModeRequestWithHooks emptyClientHooks

runPartialModeRequestWithHooks :: ClientHooks -> Provider -> Text -> ChatCompletionRequest -> Bool -> IO (Either Text PartialModeResult)
runPartialModeRequestWithHooks hooks provider apiKey reqBody useBeta = do
  let reqJsonText = encodeRequestJsonWithFlatExtra reqBody
  result <- sendChatCompletionRawWithHooks hooks provider apiKey reqBody useBeta
  pure (buildRequestResponse reqJsonText result)
