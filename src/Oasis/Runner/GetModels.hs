module Oasis.Runner.GetModels
  ( GetModelsResult
  , runGetModels
  ) where

import Relude
import Data.Aeson (Value)
import Oasis.Client.OpenAI
import Oasis.Types
import Oasis.Runner.Result (RunnerResult(..), buildRunnerResult)

type GetModelsResult = RunnerResult Value

runGetModels :: Provider -> Text -> IO (Either Text GetModelsResult)
runGetModels provider apiKey = do
  resp <- sendModelsRaw provider apiKey
  pure (buildRunnerResult "" resp)
