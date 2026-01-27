module Oasis.Runner.Basic
  ( BasicResult
  , buildBasicRequest
  , runBasic
  , runBasicWithHooks
  , runBasicRaw
  , runBasicRawWithHooks
  , runBasicRequest
  , runBasicRequestWithHooks
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
import Oasis.Client.OpenAI.Context (buildUserMessages)
import Oasis.Client.OpenAI.Param (ChatParams, applyChatParams)
import Oasis.Runner.Result (buildRequestResponse)

type BasicResult = RequestResponse ChatCompletionResponse

runBasic :: Provider -> Text -> Maybe Text -> ChatParams -> Text -> Bool -> IO (Either Text BasicResult)
runBasic provider apiKey modelOverride params prompt useBeta = do
  runBasicWithHooks emptyClientHooks provider apiKey modelOverride params prompt useBeta

runBasicWithHooks :: ClientHooks -> Provider -> Text -> Maybe Text -> ChatParams -> Text -> Bool -> IO (Either Text BasicResult)
runBasicWithHooks hooks provider apiKey modelOverride params prompt useBeta = do
  let modelId = resolveModelId provider modelOverride
      reqBody = buildBasicRequest modelId params prompt
  runBasicRequestWithHooks hooks provider apiKey reqBody useBeta

runBasicRaw :: Provider -> Text -> Maybe Text -> ChatParams -> [Message] -> Bool -> IO (Either Text BasicResult)
runBasicRaw provider apiKey modelOverride params messages useBeta = do
  runBasicRawWithHooks emptyClientHooks provider apiKey modelOverride params messages useBeta

runBasicRawWithHooks :: ClientHooks -> Provider -> Text -> Maybe Text -> ChatParams -> [Message] -> Bool -> IO (Either Text BasicResult)
runBasicRawWithHooks hooks provider apiKey modelOverride params messages useBeta = do
  let modelId = resolveModelId provider modelOverride
      reqBase = defaultChatRequest modelId messages
      reqBody = applyChatParams params reqBase
  runBasicRequestWithHooks hooks provider apiKey reqBody useBeta

buildBasicRequest :: Text -> ChatParams -> Text -> ChatCompletionRequest
buildBasicRequest modelId params prompt =
  let messages = buildUserMessages prompt
      reqBase = defaultChatRequest modelId messages
  in applyChatParams params reqBase

runBasicRequest :: Provider -> Text -> ChatCompletionRequest -> Bool -> IO (Either Text BasicResult)
runBasicRequest = runBasicRequestWithHooks emptyClientHooks

runBasicRequestWithHooks :: ClientHooks -> Provider -> Text -> ChatCompletionRequest -> Bool -> IO (Either Text BasicResult)
runBasicRequestWithHooks hooks provider apiKey reqBody useBeta = do
  let reqJsonText = encodeRequestJsonWithFlatExtra reqBody
  resp <- sendChatCompletionRawWithHooks hooks provider apiKey reqBody useBeta
  pure (buildRequestResponse reqJsonText resp)
