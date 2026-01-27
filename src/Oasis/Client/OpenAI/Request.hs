{-# LANGUAGE StrictData #-}

module Oasis.Client.OpenAI.Request
  ( buildJsonRequest
  , buildSseRequest
  , buildGetRequest
  ) where

import Relude
import Oasis.Client.OpenAI.Http (buildRequest)
import Oasis.Client.OpenAI.Types (jsonHeaders, sseHeaders, modelsHeaders)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Client (Request, RequestBody(..))
import Network.HTTP.Types.Method (Method)

buildJsonRequest :: Text -> Method -> BL.ByteString -> Text -> IO Request
buildJsonRequest url method body apiKey =
  buildRequest url method (RequestBodyLBS body) (jsonHeaders apiKey)

buildSseRequest :: Text -> BL.ByteString -> Text -> IO Request
buildSseRequest url body apiKey =
  buildRequest url "POST" (RequestBodyLBS body) (sseHeaders apiKey)

buildGetRequest :: Text -> Text -> IO Request
buildGetRequest url apiKey =
  buildRequest url "GET" (RequestBodyBS BS.empty) (modelsHeaders apiKey)
