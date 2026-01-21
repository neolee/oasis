module Oasis.Tui.Actions
  ( runBasicAction
  , providerModels
  ) where

import Relude
import Brick.BChan (writeBChan)
import Brick.Types (EventM)
import Control.Concurrent (forkIO)
import Control.Monad.State.Class (get, modify)
import qualified Data.List as List
import qualified Data.Map.Strict as M
import Oasis.Client.OpenAI.Param (emptyChatParams)
import Oasis.Config (resolveProvider)
import Oasis.Runner.Basic (runBasic)
import Oasis.Tui.State (AppState(..), Name(..), TuiEvent(..))
import Oasis.Types (Config(..), Provider(..), RequestResponse(..))

runBasicAction :: EventM Name AppState ()
runBasicAction = do
  st <- get
  case selectedProvider st of
    Nothing ->
      modify (\s -> s { statusText = "Select a provider first." })
    Just providerName -> do
      resolved <- liftIO (resolveProvider (config st) providerName)
      case resolved of
        Nothing ->
          modify (\s -> s { statusText = "Provider not found: " <> providerName })
        Just (provider, apiKey) -> do
          let modelOverride = selectedModel st
          let prompt = "Hello from oasis-tui basic runner."
          modify (\s -> s
            { statusText = "Running basic runner..."
            , outputText = ""
            })
          let chan = eventChan st
          void $ liftIO $ forkIO $ do
            result <- runBasic provider apiKey modelOverride emptyChatParams prompt False
            let (statusMsg, outputMsg) =
                  case result of
                    Left err ->
                      ("Basic runner failed.", "Error:\n" <> err)
                    Right rr ->
                      ("Basic runner completed.", "Request:\n" <> requestJson rr <> "\n\nResponse:\n" <> responseJson rr)
            writeBChan chan (BasicCompleted statusMsg outputMsg)

providerModels :: Config -> Text -> [Text]
providerModels cfg providerName =
  case M.lookup providerName (providers cfg) of
    Nothing -> []
    Just Provider{chat_model_id, coder_model_id, reasoner_model_id} ->
      List.nub [chat_model_id, coder_model_id, reasoner_model_id]
