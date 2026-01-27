module Oasis.Tui.Actions.Chat
  ( runBasicAction
  , runChatInitAction
  , runChatAction
  , runHooksAction
  , runStructuredJsonAction
  , runStructuredSchemaAction
  ) where

import Relude
import Brick.BChan (BChan, writeBChan)
import Brick.Types (EventM)
import Control.Monad.State.Class (modify)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified Data.IORef as IORef
import Data.Aeson (Value, eitherDecodeStrict)
import Oasis.Client.OpenAI
  ( buildChatUrl
  , encodeRequestJsonWithFlatExtra
  , renderClientError
  )
import Oasis.Client.OpenAI.Hooks (emptyClientHooks)
import Oasis.Client.OpenAI.Types
  ( ChatCompletionRequest(..)
  , setChatStream
  )
import Oasis.Chat.Message (assistantMessage, userMessage)
import Oasis.Demo.StructuredOutput
  ( structuredMessages
  , jsonObjectFormat
  , jsonSchemaFormat
  )
import Oasis.Model (resolveModelId)
import Oasis.Tui.Actions.Common
  ( runProviderAction
  , resolveSelectedProvider
  , startRunner
  , runWithDebug
  , runInBackground
  , buildRequestContext
  , buildChatRequestContext
  , encodeJsonText
  , buildDebugInfo
  , jsonRequestHeaders
  , sseRequestHeaders
  , decodeJsonText
  , parseRawResponseStrict
  , selectBaseUrl
  , extractAssistantContent
  , withMessageListHooks
  , mergeClientHooks
  )
import Oasis.Tui.Render.Output
  ( RequestContext(..)
  , mdCodeSection
  , mdJsonSection
  , mdTextSection
  , mdConcat
  , requestSections
  , renderErrorOutput
  )
import Oasis.Tui.State (AppState(..), Name(..), TuiEvent(..))
import Oasis.Types (Message, Provider(..), RequestResponse(..))
import Oasis.Runner.Chat
  ( ChatStreamEvent(..)
  , buildChatRequest
  , streamChatWithRequest
  )
import Oasis.Runner.Basic
  ( BasicResult
  , buildBasicRequest
  , runBasicRequestWithHooks
  )
import Oasis.Runner.StructuredOutput
  ( StructuredOutputResult(..)
  , buildStructuredRequest
  , streamStructuredOutputWithRequestWithHooks
  )
import Oasis.Runner.Hooks
  ( HooksResult(..)
  , buildHooksRequest
  , HooksLogger(..)
  , runHooksRequestWithLogger
  )
import Network.HTTP.Client (Request(..))
import Network.HTTP.Types.Header (HeaderName, hAuthorization)
import Network.HTTP.Types.Status (Status, statusCode)
import Data.CaseInsensitive (original)

runBasicAction :: Text -> EventM Name AppState ()
runBasicAction prompt =
  runProviderAction "Running basic runner..." $ \st provider apiKey -> do
    let modelOverride = selectedModel st
        params = chatParams st
        useBeta = betaUrlSetting st
        chan = eventChan st
        providerName = fromMaybe "-" (selectedProvider st)
        modelId = resolveModelId provider modelOverride
        baseUrl = selectBaseUrl provider useBeta
        reqBody = buildBasicRequest modelId params prompt
        reqJsonPreview = encodeChatRequestText reqBody
        reqJsonDebug = encodeJsonText reqBody
        info = buildDebugInfo providerName modelId (buildChatUrl baseUrl) (jsonRequestHeaders apiKey)
        handler bodyText = do
          reqBody' <- (decodeJsonText bodyText :: Either Text ChatCompletionRequest)
          let isStream = case reqBody' of
                ChatCompletionRequest{stream} -> stream
          if isStream
            then Left "basic runner requires stream=false"
            else Right $ do
              let reqMessages = case reqBody' of
                    ChatCompletionRequest{messages} -> messages
                  hooks' = withMessageListHooks chan reqMessages emptyClientHooks
                  reqCtx = buildChatRequestContext (buildChatUrl baseUrl) reqBody'
              result <- runBasicRequestWithHooks hooks' provider apiKey reqBody' useBeta
              let (statusMsg, outputMsg) =
                    case result of
                      Left err ->
                        ("Basic runner failed.", renderErrorOutput reqCtx err)
                      Right RequestResponse{responseJson, response} ->
                        let assistantContent = response >>= extractAssistantContent
                            output = mdConcat
                              ( requestSections reqCtx
                                <> catMaybes
                                    [ Just (mdJsonSection "Response" responseJson)
                                    , fmap (mdTextSection "Assistant") assistantContent
                                    ]
                              )
                        in ("Basic runner completed.", output)
              pure (BasicCompleted statusMsg outputMsg)
    pure (info, reqJsonDebug, handler)

runHooksAction :: Text -> EventM Name AppState ()
runHooksAction prompt =
  runProviderAction "Running hooks runner..." $ \st provider apiKey -> do
    let modelOverride = selectedModel st
        params = chatParams st
        useBeta = betaUrlSetting st
        chan = eventChan st
        providerName = fromMaybe "-" (selectedProvider st)
        modelId = resolveModelId provider modelOverride
        baseUrl = selectBaseUrl provider useBeta
        reqBody = buildHooksRequest modelId params prompt
        reqJsonPreview = encodeChatRequestText reqBody
        reqJsonDebug = encodeJsonText reqBody
        info = buildDebugInfo providerName modelId (buildChatUrl baseUrl) (jsonRequestHeaders apiKey)
        handler bodyText = do
          reqBody' <- (decodeJsonText bodyText :: Either Text ChatCompletionRequest)
          let isStream = case reqBody' of
                ChatCompletionRequest{stream} -> stream
          if isStream
            then Left "hooks runner requires stream=false"
            else Right $ do
              let reqMessages = case reqBody' of
                    ChatCompletionRequest{messages} -> messages
                  reqCtx = buildChatRequestContext (buildChatUrl baseUrl) reqBody'
                  hooks' = withMessageListHooks chan reqMessages emptyClientHooks
                  logger = HooksLogger
                    { onRequestLog = logRequestWith (writeBChan chan . StructuredStreaming)
                    , onResponseLog = logResponseWith (writeBChan chan . StructuredStreaming)
                    , onErrorLog = writeBChan chan . StructuredStreaming . renderClientError
                    }
              result <- runHooksRequestWithLogger logger provider apiKey reqBody' useBeta
              let (statusMsg, outputMsg) =
                    case result of
                      Left err ->
                        ("Hooks runner failed.", renderErrorOutput reqCtx err)
                      Right HooksResult{hookLogText, responseJsonText, requestJsonText} ->
                        let output = mdConcat
                              ( requestSections reqCtx
                                <> [ mdTextSection "Hook Log" hookLogText
                                   , mdCodeSection "Response JSON (truncated)" "json" responseJsonText
                                   ]
                              )
                        in ("Hooks runner completed.", output)
              pure (HooksCompleted statusMsg outputMsg)
    pure (info, reqJsonDebug, handler)

runStructuredJsonAction :: EventM Name AppState ()
runStructuredJsonAction = runStructuredAction jsonObjectFormat "structured-json"

runStructuredSchemaAction :: EventM Name AppState ()
runStructuredSchemaAction = runStructuredAction jsonSchemaFormat "structured-schema"

runStructuredAction :: Value -> Text -> EventM Name AppState ()
runStructuredAction responseFormat runnerLabel =
  runProviderAction ("Running " <> runnerLabel <> " runner...") $ \st provider apiKey -> do
    let modelOverride = selectedModel st
        params = chatParams st
        useBeta = betaUrlSetting st
        chan = eventChan st
        providerName = fromMaybe "-" (selectedProvider st)
        modelId = resolveModelId provider modelOverride
        reqMessages = structuredMessages
        reqBody = buildStructuredRequest modelId params reqMessages responseFormat
        reqJsonPreview = encodeChatRequestText reqBody
        reqJsonDebug = encodeJsonText reqBody
        info = buildDebugInfo providerName modelId (buildChatUrl (selectBaseUrl provider useBeta)) (sseRequestHeaders apiKey)
        handler bodyText = do
          reqBody' <- (decodeJsonText bodyText :: Either Text ChatCompletionRequest)
          let isStream = case reqBody' of
                ChatCompletionRequest{stream} -> stream
          if not isStream
            then Left (runnerLabel <> " runner requires stream=true")
            else Right $ do
              let reqMessages = case reqBody' of
                    ChatCompletionRequest{messages} -> messages
                  hooks = withMessageListHooks chan reqMessages emptyClientHooks
              result <- streamStructuredOutputWithRequestWithHooks
                hooks
                provider
                apiKey
                reqBody'
                (writeBChan chan . StructuredStreaming . streamingOutput)
                useBeta
              let (statusMsg, outputMsg) =
                    case result of
                      Left err ->
                        let output = mdConcat
                              [ mdCodeSection "Raw Stream" "text" ""
                              , mdTextSection "Parsed JSON" ("Error: " <> err)
                              ]
                        in (runnerLabel <> " runner failed.", output)
                      Right StructuredOutputResult{rawText, parsedJson} ->
                        let parsedSection =
                              case parsedJson of
                                Left perr -> mdTextSection "Parsed JSON" ("Invalid JSON: " <> perr)
                                Right pretty -> mdCodeSection "Parsed JSON" "json" pretty
                            output = mdConcat
                              [ mdCodeSection "Raw Stream" "text" rawText
                              , parsedSection
                              ]
                        in (runnerLabel <> " runner completed.", output)
              pure (StructuredCompleted statusMsg outputMsg)
    pure (info, reqJsonDebug, handler)

runChatInitAction :: EventM Name AppState ()
runChatInitAction =
  modify (\s -> s
    { statusText = "Chat ready."
    , activeList = ChatInputEditor
    , runnerStarted = True
    })

runChatAction :: [Message] -> EventM Name AppState ()
runChatAction messages =
  do
    st <- get
    mResolved <- resolveSelectedProvider
    forM_ mResolved $ \(provider, apiKey) -> do
      startRunner "Running chat..."
      let modelOverride = selectedModel st
          params = chatParams st
          useBeta = betaUrlSetting st
          chan = eventChan st
          providerName = fromMaybe "-" (selectedProvider st)
          modelId = resolveModelId provider modelOverride
          reqBody = setChatStream True (buildChatRequest modelId params messages)
          reqJsonPreview = encodeChatRequestText reqBody
          reqJsonDebug = encodeJsonText reqBody
          info = buildDebugInfo providerName modelId (buildChatUrl (selectBaseUrl provider useBeta)) (sseRequestHeaders apiKey)
          runStream body = do
            renderRef <- IORef.newIORef (ChatRenderState False False)
            result <- streamChatWithRequest provider apiKey body (handleChatStreamEvent chan renderRef) useBeta
            case result of
              Left err -> pure (ChatCompleted ("Chat failed: " <> err))
              Right _ -> pure (ChatCompleted "Chat completed.")
          handler bodyText = do
            reqBody' <- (decodeJsonText bodyText :: Either Text ChatCompletionRequest)
            let isStream = case reqBody' of
                  ChatCompletionRequest{stream} -> stream
            if not isStream
              then Left "chat runner requires stream=true"
              else Right (runStream reqBody')
      if debugEnabled st
        then runWithDebug info reqJsonDebug handler
        else runInBackground st (runStream reqBody)

encodeChatRequestText :: ChatCompletionRequest -> Text
encodeChatRequestText = encodeRequestJsonWithFlatExtra

data ChatRenderState = ChatRenderState
  { printedThinking :: Bool
  , printedAnswer :: Bool
  }

handleChatStreamEvent :: BChan TuiEvent -> IORef.IORef ChatRenderState -> ChatStreamEvent -> IO ()
handleChatStreamEvent chan stateRef event = do
  ChatRenderState{printedThinking, printedAnswer} <- IORef.readIORef stateRef
  case event of
    ChatThinking t
      | T.null (T.strip t) -> pure ()
      | otherwise -> do
          let prefix = if printedThinking then "" else "## Thinking\n"
          writeBChan chan (ChatStreaming (prefix <> t))
          IORef.writeIORef stateRef (ChatRenderState True printedAnswer)
    ChatAnswer t
      | T.null (T.strip t) -> pure ()
      | otherwise -> do
          let prefix
                | printedAnswer = ""
                | printedThinking = "\n\n## Answer\n"
                | otherwise = ""
          writeBChan chan (ChatStreaming (prefix <> t))
          IORef.writeIORef stateRef (ChatRenderState printedThinking True)

streamingOutput :: Text -> Text
streamingOutput rawText =
  mdConcat
    [ mdCodeSection "Raw Stream" "text" rawText
    , mdTextSection "Parsed JSON" "Streaming..."
    ]


logRequestWith :: (Text -> IO ()) -> Request -> IO ()
logRequestWith appendLog req = do
  appendLog "--- Hook: Request ---"
  appendLog (formatRequestLine req)
  forM_ (requestHeaders req) $ \(name, value) ->
    appendLog (formatHeader name value)

logResponseWith :: (Text -> IO ()) -> Status -> [(HeaderName, ByteString)] -> BL.ByteString -> IO ()
logResponseWith appendLog status headers body = do
  appendLog "--- Hook: Response ---"
  appendLog ("Status: " <> show (statusCode status))
  forM_ headers $ \(name, value) ->
    appendLog (formatHeader name value)
  appendLog ("Body bytes: " <> show (BL.length body))

formatRequestLine :: Request -> Text
formatRequestLine req =
  let scheme = if secure req then "https" else "http"
      hostText = TE.decodeUtf8Lenient (host req)
      pathText = TE.decodeUtf8Lenient (path req)
      queryText = TE.decodeUtf8Lenient (queryString req)
      url = scheme <> "://" <> hostText <> pathText <> queryText
      methodText = TE.decodeUtf8Lenient (method req)
  in methodText <> " " <> url

formatHeader :: HeaderName -> ByteString -> Text
formatHeader name value
  | name == hAuthorization = TE.decodeUtf8Lenient (original name) <> ": <redacted>"
  | otherwise = TE.decodeUtf8Lenient (original name) <> ": " <> TE.decodeUtf8Lenient value
