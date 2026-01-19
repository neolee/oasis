module Oasis.Runner.Hooks
  ( runHooks
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI
import Oasis.Runner.Common (resolveModelId, ChatParams, applyChatParams, buildUserMessages)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.CaseInsensitive (original)
import Network.HTTP.Client (Request(..))
import Network.HTTP.Types.Header (HeaderName, hAuthorization)
import Network.HTTP.Types.Status (Status, statusCode)

runHooks :: Provider -> Text -> Maybe Text -> ChatParams -> Text -> Bool -> IO (Either Text ())
runHooks provider apiKey modelOverride params prompt useBeta = do
  let modelId = resolveModelId provider modelOverride
      messages = buildUserMessages prompt
      reqBase = defaultChatRequest modelId messages
      reqBody = applyChatParams params reqBase
      hooks = ClientHooks
        { onRequest = Just logRequest
        , onResponse = Just logResponse
        , onError = Just (putTextLn . renderClientError)
        }
  result <- sendChatCompletionRawWithHooks hooks provider apiKey reqBody useBeta
  case result of
    Left err -> pure (Left (renderClientError err))
    Right body -> do
      putTextLn "--- Response JSON (truncated) ---"
      putTextLn (truncateText 800 (TE.decodeUtf8Lenient (BL.toStrict body)))
      pure (Right ())

logRequest :: Request -> IO ()
logRequest req = do
  putTextLn "--- Hook: Request ---"
  putTextLn (formatRequestLine req)
  forM_ (requestHeaders req) $ \(name, value) ->
    putTextLn (formatHeader name value)

logResponse :: Status -> [(HeaderName, BS.ByteString)] -> BL.ByteString -> IO ()
logResponse status headers body = do
  putTextLn "--- Hook: Response ---"
  putTextLn ("Status: " <> show (statusCode status))
  forM_ headers $ \(name, value) ->
    putTextLn (formatHeader name value)
  putTextLn ("Body bytes: " <> show (BL.length body))

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
