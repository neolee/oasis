module Oasis.Runner.Render
  ( renderRunnerResult
  , renderResponseOnly
  ) where

import Relude
import Oasis.Runner.Result (RunnerResult(..))

renderRunnerResult :: RunnerResult a -> IO ()
renderRunnerResult RunnerResult{requestJson, responseJson, response} = do
  unless (requestJson == "") $ do
    putTextLn "--- Request JSON ---"
    putTextLn requestJson
  putTextLn "--- Response JSON ---"
  putTextLn responseJson
  when (isNothing response) $
    putTextLn "Warning: response JSON could not be decoded."

renderResponseOnly :: RunnerResult a -> IO ()
renderResponseOnly RunnerResult{responseJson, response} = do
  putTextLn "--- Response JSON ---"
  putTextLn responseJson
  when (isNothing response) $
    putTextLn "Warning: response JSON could not be decoded."
