module Oasis.Output.Types
  ( OutputPayload(..)
  , OutputSectionKind(..)
  , OutputSection(..)
  , RequestContext(..)
  ) where

import Relude
import Data.Aeson (Value)

data OutputPayload
  = OutputText Text
  | OutputJson Text
  | OutputJsonValue Value
  deriving (Show, Eq)

data OutputSectionKind
  = SectionRequest
  | SectionResponse
  | SectionAssistant
  | SectionCompletion
  | SectionToolResult
  | SectionLog
  | SectionError
  deriving (Show, Eq)

data OutputSection = OutputSection
  { kind :: OutputSectionKind
  , title :: Text
  , payload :: OutputPayload
  } deriving (Show, Eq)

data RequestContext = RequestContext
  { requestUrl :: Text
  , requestJson :: Text
  } deriving (Show, Eq)
