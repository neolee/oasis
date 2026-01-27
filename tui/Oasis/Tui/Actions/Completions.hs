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
  , buildChatUrl
  , buildCompletionsUrl
  , emptyClientHooks
  )
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
import Oasis.Types (Message(..), MessageContent(..), StopParam(..), RequestResponse(..))
import Oasis.Runner.PartialMode
  ( buildPartialModeRequest
  , runPartialModeRequestWithHooks
  )
import Oasis.Runner.PrefixCompletion
  ( buildPrefixCompletionRequest
  , runPrefixCompletionRequestWithHooks
  )
import Oasis.Runner.FIMCompletion
  ( buildFIMRequest
  , runFIMCompletionRequest
  )

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
        reqBody = buildPartialModeRequest modelId params messages
        reqJson = encodeJsonText reqBody
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
              result <- runPartialModeRequestWithHooks hooks' provider apiKey reqBody' useBeta
              let (statusMsg, outputMsg) =
                    case result of
                      Left err ->
                        ("Partial-mode runner failed.", renderErrorOutput reqCtx err)
                      Right RequestResponse{responseJson, response} ->
                        let assistantText = fromMaybe "No assistant message returned." (response >>= extractAssistantContent)
                            output = mdConcat
                              ( requestSections reqCtx
                                <> [ mdJsonSection "Response" responseJson
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
        reqBody = buildPrefixCompletionRequest modelId params messages
        reqJson = encodeJsonText reqBody
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
              result <- runPrefixCompletionRequestWithHooks hooks' provider apiKey reqBody' useBeta
              let (statusMsg, outputMsg) =
                    case result of
                      Left err ->
                        ("Prefix-completion runner failed.", renderErrorOutput reqCtx err)
                      Right RequestResponse{responseJson, response} ->
                        let assistantText = fromMaybe "No assistant message returned." (response >>= extractAssistantContent)
                            output = mdConcat
                              ( requestSections reqCtx
                                <> [ mdJsonSection "Response" responseJson
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
        reqBody = buildFIMRequest modelId fimCompletionRequest
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
              result <- runFIMCompletionRequest provider apiKey reqBody' useBeta
              let (statusMsg, outputMsg) =
                    case result of
                      Left err ->
                        ("FIM-completion runner failed.", renderErrorOutput reqCtx err)
                      Right RequestResponse{responseJson, response} ->
                        let completionText =
                              case response of
                                Just CompletionResponse{choices = (CompletionChoice{text}:_)} -> text
                                _ -> "No completion choices returned."
                            output = mdConcat
                              ( requestSections reqCtx
                                <> [ mdJsonSection "Response" responseJson
                                   , mdCodeSection "Completion" "text" completionText
                                   ]
                              )
                        in ("FIM-completion runner completed.", output)
              pure (FimCompletionCompleted statusMsg outputMsg)
    pure (info, reqJson, handler)

