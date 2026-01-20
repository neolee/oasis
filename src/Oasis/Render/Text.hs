module Oasis.Render.Text
  ( renderRunnerResultText
  , renderResponseOnlyText
  ) where

import Relude
import Oasis.Types (RequestResponse(..))
import qualified Data.Text as T

renderRunnerResultText :: RequestResponse a -> Text
renderRunnerResultText RequestResponse{requestJson, responseJson, response} =
  T.intercalate "\n" $ catMaybes
    [ if requestJson == ""
        then Nothing
        else Just (T.intercalate "\n" ["--- Request JSON ---", requestJson])
    , Just (T.intercalate "\n" ["--- Response JSON ---", responseJson])
    , if isNothing response
        then Just "Warning: response JSON could not be decoded."
        else Nothing
    ]

renderResponseOnlyText :: RequestResponse a -> Text
renderResponseOnlyText RequestResponse{responseJson, response} =
  T.intercalate "\n" $ catMaybes
    [ Just (T.intercalate "\n" ["--- Response JSON ---", responseJson])
    , if isNothing response
        then Just "Warning: response JSON could not be decoded."
        else Nothing
    ]
