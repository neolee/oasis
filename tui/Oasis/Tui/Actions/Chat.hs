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
import Control.Monad.State.Class (get, modify)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified Data.IORef as IORef
import Data.Aeson (FromJSON, Value, eitherDecode, eitherDecodeStrict)
import qualified Data.Aeson as Aeson
import Oasis.Client.OpenAI
  ( ChatCompletionRequest(..)
  , ChatCompletionStreamChunk(..)
  , StreamChoice(..)
  , StreamDelta(..)
  , ClientError
  , defaultChatRequest
  , setChatStream
  , buildChatUrl
  , sendChatCompletionRawWithHooks
  , renderClientError
  , ClientHooks(..)
  , streamChatCompletionWithRequestWithHooks
  , emptyClientHooks
  )
import qualified Oasis.Client.OpenAI as OpenAI
import Oasis.Client.OpenAI.Param (applyChatParams)
import Oasis.Chat.Message (assistantMessage, userMessage, systemMessage)
import Oasis.Model (resolveModelId)
import Oasis.Tui.Actions.Common
  ( resolveSelectedProvider
  , startRunner
  , buildRequestContext
  , encodeJsonText
  , runWithDebug
  , buildDebugInfo
  , jsonRequestHeaders
  , sseRequestHeaders
  , selectBaseUrl
  , truncateText
  , extractAssistantContent
  , withMessageListHooks
  , mergeClientHooks
  )
import Oasis.Tui.Render.Output
  ( RequestContext(..)
  , prettyJson
  , mdCodeSection
  , mdJsonSection
  , mdTextSection
  , mdConcat
  , requestSections
  , renderErrorOutput
  )
import Oasis.Tui.State (AppState(..), Name(..), TuiEvent(..))
import Oasis.Types (Message, Provider(..))
import Network.HTTP.Client (Request(..))
import Network.HTTP.Types.Header (HeaderName, hAuthorization)
import Network.HTTP.Types.Status (Status, statusCode)
import Data.CaseInsensitive (original)

runBasicAction :: Text -> EventM Name AppState ()
runBasicAction prompt = do
  st <- get
  mResolved <- resolveSelectedProvider
  forM_ mResolved $ \(provider, apiKey) -> do
    let modelOverride = selectedModel st
        params = chatParams st
        useBeta = betaUrlSetting st
        chan = eventChan st
        providerName = fromMaybe "-" (selectedProvider st)
    startRunner "Running basic runner..."
    let modelId = resolveModelId provider modelOverride
        baseUrl = selectBaseUrl provider useBeta
        reqBody = applyChatParams params (defaultChatRequest modelId [userMessage prompt])
        reqJson = encodeJsonText reqBody
        hooks = withMessageListHooks chan [userMessage prompt] emptyClientHooks
        info = buildDebugInfo providerName modelId (buildChatUrl baseUrl) (jsonRequestHeaders apiKey)
        handler bodyText = do
          reqBody' <- decodeChatRequestText bodyText
          let isStream = case reqBody' of
                ChatCompletionRequest{stream} -> stream
          if isStream
            then Left "basic runner requires stream=false"
            else Right $ do
              let reqCtx = buildRequestContext (buildChatUrl baseUrl) reqBody'
              result <- sendChatCompletionRawWithHooks hooks provider apiKey reqBody' useBeta
              let (statusMsg, outputMsg) =
                    case parseRawResponseStrictLocal result of
                      Left err ->
                        ("Basic runner failed.", renderErrorOutput reqCtx err)
                      Right (raw, response) ->
                        let assistantContent = extractAssistantContent response
                            output = mdConcat
                              ( requestSections reqCtx
                                <> catMaybes
                                    [ Just (mdJsonSection "Response" raw)
                                    , fmap (mdTextSection "Assistant") assistantContent
                                    ]
                              )
                        in ("Basic runner completed.", output)
              pure (BasicCompleted statusMsg outputMsg)
    runWithDebug info reqJson handler

runHooksAction :: Text -> EventM Name AppState ()
runHooksAction prompt = do
  st <- get
  mResolved <- resolveSelectedProvider
  forM_ mResolved $ \(provider, apiKey) -> do
    let modelOverride = selectedModel st
        params = chatParams st
        useBeta = betaUrlSetting st
        chan = eventChan st
        providerName = fromMaybe "-" (selectedProvider st)
    startRunner "Running hooks runner..."
    let modelId = resolveModelId provider modelOverride
        baseUrl = selectBaseUrl provider useBeta
        reqBody = applyChatParams params (defaultChatRequest modelId [userMessage prompt])
        reqJson = encodeJsonText reqBody
        hooks = withMessageListHooks chan [userMessage prompt] emptyClientHooks
        info = buildDebugInfo providerName modelId (buildChatUrl baseUrl) (jsonRequestHeaders apiKey)
        handler bodyText = do
          reqBody' <- decodeChatRequestText bodyText
          let isStream = case reqBody' of
                ChatCompletionRequest{stream} -> stream
          if isStream
            then Left "hooks runner requires stream=false"
            else Right $ do
              let reqCtx = buildRequestContext (buildChatUrl baseUrl) reqBody'
              (statusMsg, outputMsg) <- runHooksWithLog hooks provider apiKey useBeta reqCtx reqBody'
              pure (HooksCompleted statusMsg outputMsg)
    runWithDebug info reqJson handler

runStructuredJsonAction :: EventM Name AppState ()
runStructuredJsonAction = runStructuredAction jsonObjectFormat "structured-json"

runStructuredSchemaAction :: EventM Name AppState ()
runStructuredSchemaAction = runStructuredAction jsonSchemaFormat "structured-schema"

runStructuredAction :: Value -> Text -> EventM Name AppState ()
runStructuredAction responseFormat runnerLabel = do
  st <- get
  mResolved <- resolveSelectedProvider
  forM_ mResolved $ \(provider, apiKey) -> do
    let modelOverride = selectedModel st
        params = chatParams st
        useBeta = betaUrlSetting st
        chan = eventChan st
        providerName = fromMaybe "-" (selectedProvider st)
    startRunner ("Running " <> runnerLabel <> " runner...")
    let modelId = resolveModelId provider modelOverride
        reqMessages =
          [ systemMessage structuredSystemMessage
          , userMessage structuredQuestionText
          ]
        reqBase = defaultChatRequest modelId reqMessages
        ChatCompletionRequest{..} = applyChatParams params reqBase
        reqBody = ChatCompletionRequest
          { OpenAI.stream = True
          , OpenAI.response_format = Just responseFormat
          , ..
          }
        reqJson = encodeJsonText reqBody
        info = buildDebugInfo providerName modelId (buildChatUrl (selectBaseUrl provider useBeta)) (sseRequestHeaders apiKey)
        handler bodyText = do
          reqBody' <- decodeChatRequestText bodyText
          let isStream = case reqBody' of
                ChatCompletionRequest{stream} -> stream
          if not isStream
            then Left (runnerLabel <> " runner requires stream=true")
            else Right $ do
              accumRef <- IORef.newIORef ""
              let hooks = withMessageListHooks chan reqMessages emptyClientHooks
              result <- streamChatCompletionWithRequestWithHooks hooks provider apiKey reqBody' (handleStructuredChunk chan accumRef) useBeta
              rawText <- IORef.readIORef accumRef
              unless (T.null (T.strip rawText)) $
                writeBChan chan (MessageListSynced (reqMessages <> [assistantMessage rawText]))
              let (statusMsg, outputMsg) =
                    case result of
                      Left err ->
                        let output = mdConcat
                              [ mdCodeSection "Raw Stream" "text" rawText
                              , mdTextSection "Parsed JSON" ("Error: " <> renderClientError err)
                              ]
                        in (runnerLabel <> " runner failed.", output)
                      Right _ ->
                        let parsedSection =
                              case parseJsonText rawText of
                                Left perr -> mdTextSection "Parsed JSON" ("Invalid JSON: " <> perr)
                                Right pretty -> mdCodeSection "Parsed JSON" "json" pretty
                            output = mdConcat
                              [ mdCodeSection "Raw Stream" "text" rawText
                              , parsedSection
                              ]
                        in (runnerLabel <> " runner completed.", output)
              pure (StructuredCompleted statusMsg outputMsg)
    runWithDebug info reqJson handler

handleStructuredChunk :: BChan TuiEvent -> IORef.IORef Text -> ChatCompletionStreamChunk -> IO ()
handleStructuredChunk chan accumRef chunk =
  forEachDeltaContent chunk $ \t -> do
    IORef.modifyIORef' accumRef (<> t)
    rawText <- IORef.readIORef accumRef
    writeBChan chan (StructuredStreaming (streamingOutput rawText))

forEachDeltaContent :: ChatCompletionStreamChunk -> (Text -> IO ()) -> IO ()
forEachDeltaContent ChatCompletionStreamChunk{choices} f =
  forM_ choices $ \StreamChoice{delta} ->
    forM_ (maybe [] deltaContent delta) f

deltaContent :: StreamDelta -> [Text]
deltaContent StreamDelta{content} = maybe [] pure content

decodeChatRequestText :: Text -> Either Text ChatCompletionRequest
decodeChatRequestText raw =
  case eitherDecodeStrict (encodeUtf8 raw) of
    Left err -> Left (toText err)
    Right req -> Right req

runChatInitAction :: EventM Name AppState ()
runChatInitAction =
  modify (\s -> s
    { statusText = "Chat ready."
    , activeList = ChatInputEditor
    , runnerStarted = True
    })

runChatAction :: [Message] -> EventM Name AppState ()
runChatAction messages = do
  st <- get
  mResolved <- resolveSelectedProvider
  forM_ mResolved $ \(provider, apiKey) -> do
    let modelOverride = selectedModel st
        params = chatParams st
        useBeta = betaUrlSetting st
        chan = eventChan st
        providerName = fromMaybe "-" (selectedProvider st)
    startRunner "Running chat..."
    let modelId = resolveModelId provider modelOverride
        reqBase = defaultChatRequest modelId messages
        reqBody = applyChatParams params (setChatStream True reqBase)
        reqJson = encodeJsonText reqBody
        hooks = withMessageListHooks chan messages emptyClientHooks
        info = buildDebugInfo providerName modelId (buildChatUrl (selectBaseUrl provider useBeta)) (sseRequestHeaders apiKey)
        handler bodyText = do
          reqBody' <- decodeChatRequestText bodyText
          let isStream = case reqBody' of
                ChatCompletionRequest{stream} -> stream
          if not isStream
            then Left "chat runner requires stream=true"
            else Right $ do
              result <- streamChatCompletionWithRequestWithHooks hooks provider apiKey reqBody' (handleChatChunk chan) useBeta
              case result of
                Left err -> pure (ChatCompleted ("Chat failed: " <> renderClientError err))
                Right _ -> pure (ChatCompleted "Chat completed.")
    runWithDebug info reqJson handler

handleChatChunk :: BChan TuiEvent -> ChatCompletionStreamChunk -> IO ()
handleChatChunk chan chunk =
  forM_ (chatDeltaText chunk) (writeBChan chan . ChatStreaming)

chatDeltaText :: ChatCompletionStreamChunk -> [Text]
chatDeltaText ChatCompletionStreamChunk{choices} =
  concatMap extractChoice choices
  where
    extractChoice StreamChoice{delta} =
      case delta of
        Nothing -> []
        Just StreamDelta{content} -> maybe [] pure content

parseRawResponseStrictLocal :: FromJSON a => Either ClientError BL.ByteString -> Either Text (Text, a)
parseRawResponseStrictLocal = \case
  Left err -> Left (renderClientError err)
  Right body ->
    case eitherDecode body of
      Left err ->
        let raw = TE.decodeUtf8Lenient (BL.toStrict body)
        in Left ("Failed to decode response: " <> toText err <> "\nRaw: " <> raw)
      Right val -> Right (TE.decodeUtf8Lenient (BL.toStrict body), val)

parseJsonText :: Text -> Either Text Text
parseJsonText raw =
  case eitherDecodeStrict (encodeUtf8 raw) :: Either String Value of
    Left err -> Left (T.pack err)
    Right _ -> Right (prettyJson raw)

streamingOutput :: Text -> Text
streamingOutput rawText =
  mdConcat
    [ mdCodeSection "Raw Stream" "text" rawText
    , mdTextSection "Parsed JSON" "Streaming..."
    ]

structuredSystemMessage :: Text
structuredSystemMessage = T.unlines
  [ "The user will provide some exam text. Please parse the \"question\" and \"answer\" and output them in JSON format."
  , ""
  , "EXAMPLE INPUT:"
  , "Which is the highest mountain in the world? Mount Everest."
  , ""
  , "EXAMPLE JSON OUTPUT:"
  , "{"
  , "  \"question\": \"Which is the highest mountain in the world?\","
  , "  \"answer\": \"Mount Everest\""
  , "}"
  ]

structuredQuestionText :: Text
structuredQuestionText = "Which is the longest river in the world? The Nile River."

jsonObjectFormat :: Value
jsonObjectFormat =
  Aeson.object
    [ "type" Aeson..= ("json_object" :: Text)
    ]

jsonSchemaFormat :: Value
jsonSchemaFormat =
  Aeson.object
  [ "type" Aeson..= ("json_schema" :: Text)
  , "json_schema" Aeson..= Aeson.object
    [ "name" Aeson..= ("session" :: Text)
    , "schema" Aeson..= Aeson.object
      [ "type" Aeson..= ("object" :: Text)
      , "properties" Aeson..= Aeson.object
        [ "question" Aeson..= Aeson.object ["type" Aeson..= ("string" :: Text)]
        , "answer" Aeson..= Aeson.object ["type" Aeson..= ("string" :: Text)]
        ]
      , "required" Aeson..= (["question", "answer"] :: [Text])
      ]
    , "required" Aeson..= (["session"] :: [Text])
    ]
  ]

runHooksWithLog
  :: ClientHooks
  -> Provider
  -> Text
  -> Bool
  -> RequestContext
  -> ChatCompletionRequest
  -> IO (Text, Text)
runHooksWithLog extraHooks provider apiKey useBeta reqCtx reqBody = do
  logRef <- IORef.newIORef ([] :: [Text])
  let appendLog t = IORef.modifyIORef' logRef (<> [t])
      hooks = ClientHooks
        { onRequest = Just (logRequest appendLog)
        , onResponse = Just (logResponse appendLog)
        , onError = Just (appendLog . renderClientError)
        }
      mergedHooks = mergeClientHooks hooks extraHooks
  result <- sendChatCompletionRawWithHooks mergedHooks provider apiKey reqBody useBeta
  logs <- IORef.readIORef logRef
  let logText = T.intercalate "\n" logs
  case result of
    Left err -> do
      let output = mdConcat
            ( requestSections reqCtx
              <> [ mdTextSection "Hook Log" logText
                 , mdTextSection "Error" (renderClientError err)
                 ]
            )
      pure ("Hooks runner failed.", output)
    Right body -> do
      let responseText = truncateText 800 (TE.decodeUtf8Lenient (BL.toStrict body))
          output = mdConcat
            ( requestSections reqCtx
              <> [ mdTextSection "Hook Log" logText
                 , mdCodeSection "Response JSON (truncated)" "json" responseText
                 ]
            )
      pure ("Hooks runner completed.", output)

logRequest :: (Text -> IO ()) -> Request -> IO ()
logRequest appendLog req = do
  appendLog "--- Hook: Request ---"
  appendLog (formatRequestLine req)
  forM_ (requestHeaders req) $ \(name, value) ->
    appendLog (formatHeader name value)

logResponse :: (Text -> IO ()) -> Status -> [(HeaderName, ByteString)] -> BL.ByteString -> IO ()
logResponse appendLog status headers body = do
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
