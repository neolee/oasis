module Oasis.Tui.Actions.Completions
  ( runPartialModeAction
  , runPrefixCompletionAction
  , runFimCompletionAction
  ) where

import Relude
import Brick.Types (EventM)
import Control.Monad.State.Class (get)
import Oasis.Client.OpenAI
  ( ChatCompletionRequest(..)
  , CompletionRequest(..)
  , CompletionResponse(..)
  , CompletionChoice(..)
  , ClientError
  , defaultChatRequest
  , buildChatUrl
  , buildCompletionsUrl
  , sendChatCompletionRawWithHooks
  , sendCompletionsRaw
  , renderClientError
  , emptyClientHooks
  )
import Oasis.Client.OpenAI.Param (applyChatParams)
import Oasis.Model (resolveModelId)
import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import Oasis.Tui.Actions.Common
  ( resolveSelectedProvider
  , startRunner
  , buildRequestContext
  , encodeJsonText
  , runWithDebug
  , buildDebugInfo
  , jsonRequestHeaders
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

runPartialModeAction :: EventM Name AppState ()
runPartialModeAction = do
  st <- get
  mResolved <- resolveSelectedProvider
  forM_ mResolved $ \(provider, apiKey) -> do
    let modelOverride = selectedModel st
        params = chatParams st
        useBeta = betaUrlSetting st
        chan = eventChan st
        providerName = fromMaybe "-" (selectedProvider st)
    startRunner "Running partial-mode runner..."
    let modelId = resolveModelId provider modelOverride
        messages =
          [ Message "user" (ContentText "请对“春天来了，大地”这句话进行续写，来表达春天的美好和作者的喜悦之情") Nothing Nothing Nothing Nothing
          , Message "assistant" (ContentText "春天来了，大地") Nothing Nothing (Just True) (Just True)
          ]
        reqBase = defaultChatRequest modelId messages
        reqBody = applyChatParams params reqBase
        reqJson = encodeJsonText reqBody
        hooks = withMessageListHooks chan messages emptyClientHooks
        info = buildDebugInfo providerName modelId (buildChatUrl (selectBaseUrl provider useBeta)) (jsonRequestHeaders apiKey)
        handler bodyText = do
          reqBody' <- decodeChatRequestText bodyText
          let isStream = case reqBody' of
                ChatCompletionRequest{stream} -> stream
          if isStream
            then Left "partial-mode runner requires stream=false"
            else Right $ do
              let reqCtx = buildRequestContext (buildChatUrl (selectBaseUrl provider useBeta)) reqBody'
              result <- sendChatCompletionRawWithHooks hooks provider apiKey reqBody' useBeta
              let (statusMsg, outputMsg) =
                    case parseRawResponseStrictLocal result of
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
    runWithDebug info reqJson handler

runPrefixCompletionAction :: EventM Name AppState ()
runPrefixCompletionAction = do
  st <- get
  mResolved <- resolveSelectedProvider
  forM_ mResolved $ \(provider, apiKey) -> do
    let modelOverride = selectedModel st
        params = chatParams st
        useBeta = betaUrlSetting st
        chan = eventChan st
        providerName = fromMaybe "-" (selectedProvider st)
    startRunner "Running prefix-completion runner..."
    let modelId = resolveModelId provider modelOverride
        messages =
          [ Message "user" (ContentText "Please write quick sort code") Nothing Nothing Nothing Nothing
          , Message "assistant" (ContentText "```python\n") Nothing Nothing (Just True) (Just True)
          ]
        reqBase = ChatCompletionRequest
          { model = modelId
          , messages = messages
          , temperature = Nothing
          , top_p = Nothing
          , max_completion_tokens = Nothing
          , stop = Just (StopList ["```"])
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
          reqBody' <- decodeChatRequestText bodyText
          let isStream = case reqBody' of
                ChatCompletionRequest{stream} -> stream
          if isStream
            then Left "prefix-completion runner requires stream=false"
            else Right $ do
              let reqCtx = buildRequestContext (buildChatUrl (selectBaseUrl provider useBeta)) reqBody'
              result <- sendChatCompletionRawWithHooks hooks provider apiKey reqBody' useBeta
              let (statusMsg, outputMsg) =
                    case parseRawResponseStrictLocal result of
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
    runWithDebug info reqJson handler

runFimCompletionAction :: EventM Name AppState ()
runFimCompletionAction = do
  st <- get
  mResolved <- resolveSelectedProvider
  forM_ mResolved $ \(provider, apiKey) -> do
    let modelOverride = selectedModel st
        useBeta = betaUrlSetting st
        providerName = fromMaybe "-" (selectedProvider st)
    startRunner "Running fim-completion runner..."
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
        reqJson = encodeJsonText reqBody
        info = buildDebugInfo providerName modelId (buildCompletionsUrl (selectBaseUrl provider useBeta)) (jsonRequestHeaders apiKey)
        handler bodyText = do
          reqBody' <- decodeCompletionRequestText bodyText
          let isStream = case reqBody' of
                CompletionRequest{stream} -> stream
          if isStream
            then Left "fim-completion runner requires stream=false"
            else Right $ do
              let reqCtx = buildRequestContext (buildCompletionsUrl (selectBaseUrl provider useBeta)) reqBody'
              result <- sendCompletionsRaw provider apiKey reqBody' useBeta
              let (statusMsg, outputMsg) =
                    case parseRawResponseStrictLocal result of
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
    runWithDebug info reqJson handler

parseRawResponseStrictLocal :: FromJSON a => Either ClientError BL.ByteString -> Either Text (Text, a)
parseRawResponseStrictLocal = \case
  Left err -> Left (renderClientError err)
  Right body ->
    case eitherDecode body of
      Left err ->
        let raw = TE.decodeUtf8Lenient (BL.toStrict body)
        in Left ("Failed to decode response: " <> toText err <> "\nRaw: " <> raw)
      Right val -> Right (TE.decodeUtf8Lenient (BL.toStrict body), val)

decodeChatRequestText :: Text -> Either Text ChatCompletionRequest
decodeChatRequestText raw =
  case eitherDecode (BL.fromStrict (TE.encodeUtf8 raw)) of
    Left err -> Left (toText err)
    Right req -> Right req

decodeCompletionRequestText :: Text -> Either Text CompletionRequest
decodeCompletionRequestText raw =
  case eitherDecode (BL.fromStrict (TE.encodeUtf8 raw)) of
    Left err -> Left (toText err)
    Right req -> Right req
