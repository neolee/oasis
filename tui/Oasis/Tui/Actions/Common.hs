module Oasis.Tui.Actions.Common
  ( resolveSelectedProvider
  , startRunner
  , runInBackground
  , buildRequestContext
  , encodeJsonText
  , openDebugDialog
  , runWithDebug
  , buildDebugInfo
  , formatHeaders
  , jsonRequestHeaders
  , sseRequestHeaders
  , modelsRequestHeaders
  , selectBaseUrl
  , truncateText
  , extractAssistantContent
  , messageListHooks
  , withMessageListHooks
  , mergeClientHooks
  ) where

import Relude
import Brick.BChan (BChan, writeBChan)
import Brick.Types (EventM)
import Brick.Widgets.Edit (editor)
import Control.Concurrent (forkIO)
import Control.Monad.State.Class (get, modify)
import Oasis.Config (resolveProvider)
import Oasis.Tui.Render.Output (RequestContext(..))
import Oasis.Tui.State (AppState(..), Name(..), TuiEvent(..), DebugRequestInfo(..), DebugRequestHandler)
import Oasis.Types (Provider(..), Message(..), messageContentText)
import Oasis.Chat.Message (assistantMessage)
import Oasis.Client.OpenAI (ChatCompletionResponse(..), ChatChoice(..), ClientHooks(..), emptyClientHooks)
import Data.Aeson (ToJSON, encode, eitherDecode)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8
import Network.HTTP.Types.Header (HeaderName, hAccept, hAuthorization, hContentType)
import Data.CaseInsensitive (original)

resolveSelectedProvider :: EventM Name AppState (Maybe (Provider, Text))
resolveSelectedProvider = do
  st <- get
  case selectedProvider st of
    Nothing -> do
      modify (\s -> s { statusText = "Select a provider first." })
      pure Nothing
    Just providerName -> do
      resolved <- liftIO (resolveProvider (config st) providerName)
      case resolved of
        Nothing -> do
          modify (\s -> s { statusText = "Provider not found: " <> providerName })
          pure Nothing
        Just providerAndKey -> pure (Just providerAndKey)

startRunner :: Text -> EventM Name AppState ()
startRunner msg =
  modify (\s -> s
    { statusText = msg
    , outputText = ""
    , runnerStarted = True
    })

runInBackground :: AppState -> IO TuiEvent -> EventM Name AppState ()
runInBackground st action =
  void $ liftIO $ forkIO $ do
    let chan = eventChan st
    evt <- action
    writeBChan chan evt

encodeJsonText :: ToJSON a => a -> Text
encodeJsonText = TE.decodeUtf8Lenient . BL.toStrict . encode

buildDebugInfo :: Text -> Text -> Text -> [(HeaderName, BS8.ByteString)] -> DebugRequestInfo
buildDebugInfo providerName modelName endpoint headers =
  DebugRequestInfo
    { debugProviderName = providerName
    , debugModelName = modelName
    , debugEndpoint = endpoint
    , debugHeaders = formatHeaders headers
    }

formatHeaders :: [(HeaderName, BS8.ByteString)] -> [Text]
formatHeaders headers =
  map render headers
  where
    render (name, value) =
      let nameText = TE.decodeUtf8Lenient (original name)
          valueText = TE.decodeUtf8Lenient value
      in nameText <> ": " <> valueText

authHeaders :: Text -> [(HeaderName, BS8.ByteString)]
authHeaders apiKey
  | T.null apiKey = []
  | otherwise = [(hAuthorization, "Bearer " <> TE.encodeUtf8 apiKey)]

jsonRequestHeaders :: Text -> [(HeaderName, BS8.ByteString)]
jsonRequestHeaders apiKey =
  [ (hContentType, "application/json")
  , (hAccept, "application/json")
  ] <> authHeaders apiKey

sseRequestHeaders :: Text -> [(HeaderName, BS8.ByteString)]
sseRequestHeaders apiKey =
  [ (hContentType, "application/json")
  , (hAccept, "text/event-stream")
  ] <> authHeaders apiKey

modelsRequestHeaders :: Text -> [(HeaderName, BS8.ByteString)]
modelsRequestHeaders apiKey =
  [ (hAccept, "application/json")
  ] <> authHeaders apiKey

openDebugDialog :: Name -> DebugRequestInfo -> Text -> DebugRequestHandler -> EventM Name AppState ()
openDebugDialog returnFocus info original handler =
  modify (\s -> s
    { debugDialogOpen = True
    , debugRequestInfo = Just info
    , debugRequestOriginal = original
    , debugRequestDraft = original
    , debugRequestError = Nothing
    , debugPendingAction = Just handler
    , debugDialogReturnFocus = returnFocus
    , debugRequestEditor = editor DebugRequestEditor (Just 12) original
    , activeList = DebugRequestEditor
    })

runWithDebug :: DebugRequestInfo -> Text -> DebugRequestHandler -> EventM Name AppState ()
runWithDebug info original handler = do
  st <- get
  if debugEnabled st
    then openDebugDialog (activeList st) info original handler
    else case handler original of
      Left err ->
        modify (\s -> s { statusText = "Request build failed: " <> err })
      Right action ->
        runInBackground st action

buildRequestContext :: ToJSON a => Text -> a -> RequestContext
buildRequestContext url reqBody =
  RequestContext
    { requestUrl = url
    , requestJson = encodeJsonText reqBody
    }

selectBaseUrl :: Provider -> Bool -> Text
selectBaseUrl Provider{base_url, beta_base_url} useBeta =
  let beta = beta_base_url >>= nonEmpty
  in if useBeta then fromMaybe base_url beta else base_url
  where
    nonEmpty t =
      let trimmed = T.strip t
      in if T.null trimmed then Nothing else Just trimmed

truncateText :: Int -> Text -> Text
truncateText maxLen txt
  | T.length txt <= maxLen = txt
  | otherwise = T.take maxLen txt <> "..."

extractAssistantContent :: ChatCompletionResponse -> Maybe Text
extractAssistantContent ChatCompletionResponse{choices} =
  case choices of
    (ChatChoice{message = Just Message{content}}:_) -> Just (messageContentText content)
    _ -> Nothing

messageListHooks :: BChan TuiEvent -> [Message] -> ClientHooks
messageListHooks chan msgs =
  emptyClientHooks
    { onRequest = Just (\_ -> writeBChan chan (MessageListSynced msgs))
    , onResponse = Just (\_ _ body ->
        case eitherDecode body :: Either String ChatCompletionResponse of
          Left _ -> pure ()
          Right resp ->
            case extractAssistantContent resp of
              Nothing -> pure ()
              Just contentText ->
                writeBChan chan (MessageListSynced (msgs <> [assistantMessage contentText]))
        )
    }

withMessageListHooks :: BChan TuiEvent -> [Message] -> ClientHooks -> ClientHooks
withMessageListHooks chan msgs hooks =
  mergeClientHooks hooks (messageListHooks chan msgs)

mergeClientHooks :: ClientHooks -> ClientHooks -> ClientHooks
mergeClientHooks left right =
  ClientHooks
    { onRequest = mergeReq (onRequest left) (onRequest right)
    , onResponse = mergeResp (onResponse left) (onResponse right)
    , onError = mergeErr (onError left) (onError right)
    }
  where
    mergeReq Nothing r = r
    mergeReq l Nothing = l
    mergeReq (Just f) (Just g) = Just (\req -> f req >> g req)

    mergeResp Nothing r = r
    mergeResp l Nothing = l
    mergeResp (Just f) (Just g) = Just (\status headers body -> f status headers body >> g status headers body)

    mergeErr Nothing r = r
    mergeErr l Nothing = l
    mergeErr (Just f) (Just g) = Just (\err -> f err >> g err)
