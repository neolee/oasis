module Oasis.Chat.Prompt
  ( PromptTemplate(..)
  , renderPrompt
  ) where

import Relude
import qualified Data.Text as T

newtype PromptTemplate = PromptTemplate { templateText :: Text }
  deriving (Show, Eq)

renderPrompt :: PromptTemplate -> [(Text, Text)] -> Text
renderPrompt (PromptTemplate txt) pairs =
  foldl' (\acc (k, v) -> T.replace ("${" <> k <> "}") v acc) txt pairs
