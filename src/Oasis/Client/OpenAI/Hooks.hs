{- HLINT ignore "Redundant bracket" -}
module Oasis.Client.OpenAI.Hooks
  ( RequestHook(..)
  , emptyRequestHook
  ) where

import Relude
import Oasis.Client.OpenAI.Types (ChatCompletionRequest, ClientError)
import qualified Data.ByteString.Lazy as BL

data RequestHook = RequestHook
  { beforeRequest :: ChatCompletionRequest -> IO (Either Text ChatCompletionRequest)
  , afterResponse :: ChatCompletionRequest -> Either ClientError BL.ByteString -> IO ()
  }

emptyRequestHook :: RequestHook
emptyRequestHook = RequestHook
  { beforeRequest = (pure . Right)
  , afterResponse = \_ _ -> pure ()
  }
