module Oasis.Tui.Actions.Common
  ( resolveSelectedProvider
  , startRunner
  , runInBackground
  , buildRequestContext
  , encodeJsonText
  , selectBaseUrl
  , truncateText
  , extractAssistantContent
  ) where

import Relude
import Brick.BChan (writeBChan)
import Brick.Types (EventM)
import Control.Concurrent (forkIO)
import Control.Monad.State.Class (get, modify)
import Oasis.Config (resolveProvider)
import Oasis.Tui.Render.Output (RequestContext(..))
import Oasis.Tui.State (AppState(..), Name(..), TuiEvent(..))
import Oasis.Types (Provider(..), Message(..), messageContentText)
import Oasis.Client.OpenAI (ChatCompletionResponse(..), ChatChoice(..))
import Data.Aeson (ToJSON, encode)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL

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
