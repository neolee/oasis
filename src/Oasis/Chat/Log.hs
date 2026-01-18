module Oasis.Chat.Log
  ( ChatLogEntry(..)
  , ChatLog(..)
  , emptyLog
  , appendLog
  , mkLogEntry
  , getEntries
  ) where

import Relude
import Oasis.Types
import Data.Aeson (Value)
import Data.Time (UTCTime)

data ChatLogEntry = ChatLogEntry
  { message   :: Message
  , timestamp :: UTCTime
  , meta      :: Maybe Value
  } deriving (Show, Eq)

newtype ChatLog = ChatLog { entries :: [ChatLogEntry] }
  deriving (Show, Eq)

emptyLog :: ChatLog
emptyLog = ChatLog []

appendLog :: ChatLogEntry -> ChatLog -> ChatLog
appendLog entry (ChatLog xs) = ChatLog (xs <> [entry])

mkLogEntry :: UTCTime -> Message -> Maybe Value -> ChatLogEntry
mkLogEntry ts msg meta = ChatLogEntry
  { message = msg
  , timestamp = ts
  , meta = meta
  }

getEntries :: ChatLog -> [ChatLogEntry]
getEntries (ChatLog xs) = xs
