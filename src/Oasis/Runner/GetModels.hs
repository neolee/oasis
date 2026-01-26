module Oasis.Runner.GetModels
  ( GetModelsResult
  , runGetModels
  ) where

import Relude
import Data.Aeson (Value)
import Oasis.Client.OpenAI
import Oasis.Types
import Oasis.Runner.Result (buildRequestResponse)

type GetModelsResult = RequestResponse Value

runGetModels :: Provider -> Text -> Bool -> IO (Either Text GetModelsResult)
runGetModels provider apiKey useBeta = do
  resp <- sendModelsRaw provider apiKey useBeta
  pure (buildRequestResponse "" resp)
