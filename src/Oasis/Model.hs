module Oasis.Model
  ( selectModelId
  , resolveModelId
  , resolveEmbeddingModelId
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

resolveEmbeddingModelId :: Provider -> Maybe Text -> Text
resolveEmbeddingModelId provider@Provider{embedding_model_id} modelOverride =
  case embedding_model_id >>= nonEmpty of
    Just emb -> emb
    Nothing -> resolveModelId provider modelOverride
  where
    nonEmpty t =
      let trimmed = T.strip t
      in if T.null trimmed then Nothing else Just trimmed
