module Oasis.Tui.Actions.Completions
  ( runPartialModeAction
  , runPrefixCompletionAction
  , runFimCompletionAction
  ) where

import Relude
import Brick.Types (EventM)
import Oasis.Client.OpenAI
  ( ChatCompletionRequest(..)
  , CompletionRequest(..)
  , CompletionResponse(..)
  , CompletionChoice(..)
  , defaultChatRequest
  , buildChatUrl
  , buildCompletionsUrl
  , sendChatCompletionRawWithHooks
  , sendCompletionsRaw
  , emptyClientHooks
  )
import Oasis.Client.OpenAI.Param (applyChatParams)
import Oasis.Model (resolveModelId)
import Oasis.Demo.Completions
  ( partialModeMessages
  , prefixCompletionMessages
  , fimCompletionRequest
  )
import Oasis.Tui.Actions.Common
  ( runProviderAction
  , buildRequestContext
  , encodeJsonText
  , buildDebugInfo
  , jsonRequestHeaders
  , decodeJsonText
  , parseRawResponseStrict
  , selectBaseUrl
  , extractAssistantContent
  , withMessageListHooks
  )
import Oasis.Tui.Render.Output
  ( mdConcat
  , mdCodeSection
  , mdJsonSection
  , mdTextSection
  , requestSections
  , renderErrorOutput
  )
import Oasis.Tui.State (AppState(..), Name(..), TuiEvent(..))
import Oasis.Types (Message(..), MessageContent(..), StopParam(..))
import qualified Oasis.Types as Types

runPartialModeAction :: EventM Name AppState ()
runPartialModeAction =
  runProviderAction "Running partial-mode runner..." $ \st provider apiKey -> do
    let modelOverride = selectedModel st
        params = chatParams st
        useBeta = betaUrlSetting st
        chan = eventChan st
        providerName = fromMaybe "-" (selectedProvider st)
        modelId = resolveModelId provider modelOverride
        messages = partialModeMessages
        reqBase = defaultChatRequest modelId messages
        reqBody = applyChatParams params reqBase
        reqJson = encodeJsonText reqBody
        hooks = withMessageListHooks chan messages emptyClientHooks
        info = buildDebugInfo providerName modelId (buildChatUrl (selectBaseUrl provider useBeta)) (jsonRequestHeaders apiKey)
        handler bodyText = do
          reqBody' <- (decodeJsonText bodyText :: Either Text ChatCompletionRequest)
          let isStream = case reqBody' of
                ChatCompletionRequest{stream} -> stream
          if isStream
            then Left "partial-mode runner requires stream=false"
            else Right $ do
              let reqMessages = case reqBody' of
                    ChatCompletionRequest{messages} -> messages
                  hooks' = withMessageListHooks chan reqMessages emptyClientHooks
                  reqCtx = buildRequestContext (buildChatUrl (selectBaseUrl provider useBeta)) reqBody'
              result <- sendChatCompletionRawWithHooks hooks' provider apiKey reqBody' useBeta
              let (statusMsg, outputMsg) =
                    case parseRawResponseStrict result of
                      Left err ->
                        ("Partial-mode runner failed.", renderErrorOutput reqCtx err)
                      Right (raw, response) ->
                        let assistantText = fromMaybe "No assistant message returned." (extractAssistantContent response)
                            output = mdConcat
                              ( requestSections reqCtx
                                <> [ mdJsonSection "Response" raw
                                   , mdCodeSection "Assistant" "text" assistantText
                                   ]
                              )
                        in ("Partial-mode runner completed.", output)
              pure (PartialModeCompleted statusMsg outputMsg)
    pure (info, reqJson, handler)

runPrefixCompletionAction :: EventM Name AppState ()
runPrefixCompletionAction =
  runProviderAction "Running prefix-completion runner..." $ \st provider apiKey -> do
    let modelOverride = selectedModel st
        params = chatParams st
        useBeta = betaUrlSetting st
        chan = eventChan st
        providerName = fromMaybe "-" (selectedProvider st)
        modelId = resolveModelId provider modelOverride
        messages = prefixCompletionMessages
        reqBase = ChatCompletionRequest
          { model = modelId
          , messages = messages
          , temperature = Nothing
          , top_p = Nothing
          , max_completion_tokens = Nothing
            , stop = Just (Types.StopList ["```"])
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
        reqJson = encodeJsonText reqBody
        hooks = withMessageListHooks chan messages emptyClientHooks
        info = buildDebugInfo providerName modelId (buildChatUrl (selectBaseUrl provider useBeta)) (jsonRequestHeaders apiKey)
        handler bodyText = do
          reqBody' <- (decodeJsonText bodyText :: Either Text ChatCompletionRequest)
          let isStream = case reqBody' of
                ChatCompletionRequest{stream} -> stream
          if isStream
            then Left "prefix-completion runner requires stream=false"
            else Right $ do
              let reqMessages = case reqBody' of
                    ChatCompletionRequest{messages} -> messages
                  hooks' = withMessageListHooks chan reqMessages emptyClientHooks
                  reqCtx = buildRequestContext (buildChatUrl (selectBaseUrl provider useBeta)) reqBody'
              result <- sendChatCompletionRawWithHooks hooks' provider apiKey reqBody' useBeta
              let (statusMsg, outputMsg) =
                    case parseRawResponseStrict result of
                      Left err ->
                        ("Prefix-completion runner failed.", renderErrorOutput reqCtx err)
                      Right (raw, response) ->
                        let assistantText = fromMaybe "No assistant message returned." (extractAssistantContent response)
                            output = mdConcat
                              ( requestSections reqCtx
                                <> [ mdJsonSection "Response" raw
                                   , mdCodeSection "Assistant" "text" assistantText
                                   ]
                              )
                        in ("Prefix-completion runner completed.", output)
              pure (PrefixCompletionCompleted statusMsg outputMsg)
    pure (info, reqJson, handler)

runFimCompletionAction :: EventM Name AppState ()
runFimCompletionAction =
  runProviderAction "Running fim-completion runner..." $ \st provider apiKey -> do
    let modelOverride = selectedModel st
        useBeta = betaUrlSetting st
        providerName = fromMaybe "-" (selectedProvider st)
        modelId = resolveModelId provider modelOverride
        CompletionRequest{prompt, suffix, max_tokens, temperature, top_p, stream, stop, echo, logprobs} = fimCompletionRequest
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
        reqJson = encodeJsonText reqBody
        info = buildDebugInfo providerName modelId (buildCompletionsUrl (selectBaseUrl provider useBeta)) (jsonRequestHeaders apiKey)
        handler bodyText = do
          reqBody' <- (decodeJsonText bodyText :: Either Text CompletionRequest)
          let isStream = case reqBody' of
                CompletionRequest{stream} -> stream
          if isStream
            then Left "fim-completion runner requires stream=false"
            else Right $ do
              let reqCtx = buildRequestContext (buildCompletionsUrl (selectBaseUrl provider useBeta)) reqBody'
              result <- sendCompletionsRaw provider apiKey reqBody' useBeta
              let (statusMsg, outputMsg) =
                    case parseRawResponseStrict result of
                      Left err ->
                        ("FIM-completion runner failed.", renderErrorOutput reqCtx err)
                      Right (raw, response) ->
                        let completionText =
                              case response of
                                CompletionResponse{choices = (CompletionChoice{text}:_)} -> text
                                _ -> "No completion choices returned."
                            output = mdConcat
                              ( requestSections reqCtx
                                <> [ mdJsonSection "Response" raw
                                   , mdCodeSection "Completion" "text" completionText
                                   ]
                              )
                        in ("FIM-completion runner completed.", output)
              pure (FimCompletionCompleted statusMsg outputMsg)
    pure (info, reqJson, handler)

