{- HLINT ignore "Redundant bracket" -}
module Oasis.Client.OpenAI.Hooks
  ( RequestHook(..)
  , emptyRequestHook
  , ClientHooks(..)
  , emptyClientHooks
  ) where

import Relude
import Oasis.Client.OpenAI.Types (ChatCompletionRequest, ClientError)
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Client (Request)
import Network.HTTP.Types.Status (Status)
import Network.HTTP.Types.Header (ResponseHeaders)

data RequestHook = RequestHook
  { beforeRequest :: ChatCompletionRequest -> IO (Either Text ChatCompletionRequest)
  , afterResponse :: ChatCompletionRequest -> Either ClientError BL.ByteString -> IO ()
  }

emptyRequestHook :: RequestHook
emptyRequestHook = RequestHook
  { beforeRequest = (pure . Right)
  , afterResponse = \_ _ -> pure ()
  }

data ClientHooks = ClientHooks
  { onRequest  :: Maybe (Request -> IO ())
  , onResponse :: Maybe (Status -> ResponseHeaders -> BL.ByteString -> IO ())
  , onError    :: Maybe (ClientError -> IO ())
  }

emptyClientHooks :: ClientHooks
emptyClientHooks = ClientHooks Nothing Nothing Nothing
