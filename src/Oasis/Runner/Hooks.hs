module Oasis.Runner.Hooks
  ( HooksResult(..)
  , HooksLogger(..)
  , buildHooksRequest
  , runHooks
  , runHooksDetailed
  , runHooksRequest
  , runHooksRequestWithLogger
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI
  ( sendChatCompletionRawWithHooks
  , encodeRequestJsonWithFlatExtra
  , renderClientError
  )
import Oasis.Client.OpenAI.Hooks (ClientHooks(..))
import Oasis.Client.OpenAI.Types
  ( ChatCompletionRequest(..)
  , ClientError(..)
  , defaultChatRequest
  )
import Oasis.Model (resolveModelId)
import Oasis.Client.OpenAI.Context (buildUserMessages)
import Oasis.Client.OpenAI.Param (ChatParams, applyChatParams)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.CaseInsensitive (original)
import Network.HTTP.Client (Request(..))
import Network.HTTP.Types.Header (HeaderName, hAuthorization)
import Network.HTTP.Types.Status (Status, statusCode)

data HooksResult = HooksResult
  { hookLogText :: Text
  , responseJsonText :: Text
  , requestJsonText :: Text
  } deriving (Show, Eq)

runHooks :: Provider -> Text -> Maybe Text -> ChatParams -> Text -> Bool -> IO (Either Text HooksResult)
runHooks = runHooksDetailed

runHooksDetailed :: Provider -> Text -> Maybe Text -> ChatParams -> Text -> Bool -> IO (Either Text HooksResult)
runHooksDetailed provider apiKey modelOverride params prompt useBeta = do
  logRef <- newIORef ([] :: [Text])
  let appendLog t = modifyIORef' logRef (<> [t])
      modelId = resolveModelId provider modelOverride
      reqBody = buildHooksRequest modelId params prompt
      logger = HooksLogger
        { onRequestLog = logRequestWith appendLog
        , onResponseLog = logResponseWith appendLog
        , onErrorLog = appendLog . renderClientError
        }
  result <- runHooksRequestWithLogger logger provider apiKey reqBody useBeta
  logs <- readIORef logRef
  let logText = T.intercalate "\n" logs
  case result of
    Left err -> pure (Left err)
    Right HooksResult{responseJsonText, requestJsonText} ->
      pure (Right HooksResult
        { hookLogText = logText
        , responseJsonText = responseJsonText
        , requestJsonText = requestJsonText
        })

data HooksLogger = HooksLogger
  { onRequestLog :: Request -> IO ()
  , onResponseLog :: Status -> [(HeaderName, BS.ByteString)] -> BL.ByteString -> IO ()
  , onErrorLog :: ClientError -> IO ()
  }

buildHooksRequest :: Text -> ChatParams -> Text -> ChatCompletionRequest
buildHooksRequest modelId params prompt =
  let messages = buildUserMessages prompt
      reqBase = defaultChatRequest modelId messages
  in applyChatParams params reqBase

runHooksRequest :: Provider -> Text -> ChatCompletionRequest -> Bool -> IO (Either Text HooksResult)
runHooksRequest = runHooksRequestWithLogger defaultLogger

runHooksRequestWithLogger
  :: HooksLogger
  -> Provider
  -> Text
  -> ChatCompletionRequest
  -> Bool
  -> IO (Either Text HooksResult)
runHooksRequestWithLogger HooksLogger{..} provider apiKey reqBody useBeta = do
  logRef <- newIORef ([] :: [Text])
  let appendLog t = modifyIORef' logRef (<> [t])
      onReq req = do
        logRequestWith appendLog req
        onRequestLog req
      onResp status headers body = do
        logResponseWith appendLog status headers body
        onResponseLog status headers body
      onErr err = do
        appendLog (renderClientError err)
        onErrorLog err
  let reqJsonText = encodeRequestJsonWithFlatExtra reqBody
      hooks = ClientHooks
        { onRequest = Just onReq
        , onResponse = Just onResp
        , onError = Just onErr
        }
  result <- sendChatCompletionRawWithHooks hooks provider apiKey reqBody useBeta
  logs <- readIORef logRef
  let logText = T.intercalate "\n" logs
  case result of
    Left err -> pure (Left (renderClientError err))
    Right body -> do
      let responseText = truncateText 800 (TE.decodeUtf8Lenient (BL.toStrict body))
      pure (Right HooksResult
        { hookLogText = logText
        , responseJsonText = responseText
        , requestJsonText = reqJsonText
        })

defaultLogger :: HooksLogger
defaultLogger = HooksLogger
  { onRequestLog = \_ -> pure ()
  , onResponseLog = \_ _ _ -> pure ()
  , onErrorLog = \_ -> pure ()
  }

logRequestWith :: (Text -> IO ()) -> Request -> IO ()
logRequestWith appendLog req = do
  appendLog "--- Hook: Request ---"
  appendLog (formatRequestLine req)
  forM_ (requestHeaders req) $ \(name, value) ->
    appendLog (formatHeader name value)

logResponseWith :: (Text -> IO ()) -> Status -> [(HeaderName, BS.ByteString)] -> BL.ByteString -> IO ()
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

formatHeader :: HeaderName -> BS.ByteString -> Text
formatHeader name value
  | name == hAuthorization = TE.decodeUtf8Lenient (original name) <> ": <redacted>"
  | otherwise = TE.decodeUtf8Lenient (original name) <> ": " <> TE.decodeUtf8Lenient value

truncateText :: Int -> Text -> Text
truncateText maxLen txt
  | T.length txt <= maxLen = txt
  | otherwise = T.take maxLen txt <> "..."
