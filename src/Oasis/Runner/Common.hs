module Oasis.Runner.Common
  ( selectModelId
  , resolveModelId
  , buildUserMessages
  ) where

import Relude
import Oasis.Types
import qualified Data.Text as T

selectModelId :: Provider -> Text
selectModelId Provider{..} =
  case fmap T.toLower default_model_type of
    Just "reasoner" | not (T.null reasoner_model_id) -> reasoner_model_id
    Just "coder"    | not (T.null coder_model_id)    -> coder_model_id
    _                                             -> chat_model_id

resolveModelId :: Provider -> Maybe Text -> Text
resolveModelId provider = \case
  Just m
    | let trimmed = T.strip m
    , not (T.null trimmed)
    , T.toLower trimmed /= "default"
    , trimmed /= "-" -> trimmed
  _ -> selectModelId provider

buildUserMessages :: Text -> [Message]
buildUserMessages prompt = [Message "user" prompt Nothing Nothing]