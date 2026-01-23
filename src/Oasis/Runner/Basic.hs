module Oasis.Runner.Basic
  ( BasicResult
  , runBasic
  , runBasicWithHooks
  , runBasicRaw
  , runBasicRawWithHooks
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI
import Oasis.Model (resolveModelId)
import Oasis.Client.OpenAI.Context (buildUserMessages)
import Oasis.Client.OpenAI.Param (ChatParams, applyChatParams)
import Oasis.Runner.Result (encodeRequestJson, buildRequestResponse)

type BasicResult = RequestResponse ChatCompletionResponse

runBasic :: Provider -> Text -> Maybe Text -> ChatParams -> Text -> Bool -> IO (Either Text BasicResult)
runBasic provider apiKey modelOverride params prompt useBeta = do
  runBasicWithHooks emptyClientHooks provider apiKey modelOverride params prompt useBeta

runBasicWithHooks :: ClientHooks -> Provider -> Text -> Maybe Text -> ChatParams -> Text -> Bool -> IO (Either Text BasicResult)
runBasicWithHooks hooks provider apiKey modelOverride params prompt useBeta = do
  let modelId = resolveModelId provider modelOverride
      messages = buildUserMessages prompt
      reqBase = defaultChatRequest modelId messages
      reqBody = applyChatParams params reqBase
      reqJsonText = encodeRequestJson reqBody
  resp <- sendChatCompletionRawWithHooks hooks provider apiKey reqBody useBeta
  pure (buildRequestResponse reqJsonText resp)

runBasicRaw :: Provider -> Text -> Maybe Text -> ChatParams -> [Message] -> Bool -> IO (Either Text BasicResult)
runBasicRaw provider apiKey modelOverride params messages useBeta = do
  runBasicRawWithHooks emptyClientHooks provider apiKey modelOverride params messages useBeta

runBasicRawWithHooks :: ClientHooks -> Provider -> Text -> Maybe Text -> ChatParams -> [Message] -> Bool -> IO (Either Text BasicResult)
runBasicRawWithHooks hooks provider apiKey modelOverride params messages useBeta = do
  let modelId = resolveModelId provider modelOverride
      reqBase = defaultChatRequest modelId messages
      reqBody = applyChatParams params reqBase
      reqJsonText = encodeRequestJson reqBody
  resp <- sendChatCompletionRawWithHooks hooks provider apiKey reqBody useBeta
  pure (buildRequestResponse reqJsonText resp)
