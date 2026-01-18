module Oasis.Chat.Log
  ( ChatLogEntry(..)
  , emptyLog
  , appendLog
  ) where

import Relude
import Oasis.Types

newtype ChatLogEntry = ChatLogEntry
  { message   :: Message
  } deriving (Show, Eq)

newtype ChatLog = ChatLog { entries :: [ChatLogEntry] }
  deriving (Show, Eq)

emptyLog :: ChatLog
emptyLog = ChatLog []

appendLog :: ChatLogEntry -> ChatLog -> ChatLog
appendLog entry (ChatLog xs) = ChatLog (xs <> [entry])
