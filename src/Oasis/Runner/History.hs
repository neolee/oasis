module Oasis.Runner.History
  ( History
  , emptyHistory
  , getMessages
  , appendMessage
  , insertMessage
  , updateMessage
  , deleteMessage
  , setSystemMessage
  ) where

import Relude
import Oasis.Types

newtype History = History { messages :: [Message] }
  deriving (Show, Eq)

emptyHistory :: History
emptyHistory = History []

getMessages :: History -> [Message]
getMessages (History msgs) = msgs

appendMessage :: Message -> History -> History
appendMessage msg (History msgs) = History (msgs <> [msg])

insertMessage :: Int -> Message -> History -> Either Text History
insertMessage idx msg (History msgs)
  | idx < 0 = Left "Index must be >= 0"
  | idx > length msgs = Left "Index out of range"
  | otherwise =
      let (before, after) = splitAt idx msgs
      in Right (History (before <> [msg] <> after))

updateMessage :: Int -> Message -> History -> Either Text History
updateMessage idx msg (History msgs)
  | idx < 0 = Left "Index must be >= 0"
  | idx >= length msgs = Left "Index out of range"
  | otherwise =
      let (before, after) = splitAt idx msgs
      in case after of
           [] -> Left "Index out of range"
           (_:rest) -> Right (History (before <> [msg] <> rest))

deleteMessage :: Int -> History -> Either Text History
deleteMessage idx (History msgs)
  | idx < 0 = Left "Index must be >= 0"
  | idx >= length msgs = Left "Index out of range"
  | otherwise =
      let (before, after) = splitAt idx msgs
      in case after of
           [] -> Left "Index out of range"
           (_:rest) -> Right (History (before <> rest))

setSystemMessage :: Text -> History -> History
setSystemMessage contentText (History msgs) =
  case msgs of
    (Message { role = "system" }:rest) -> History (Message "system" contentText : rest)
    _ -> History (Message "system" contentText : msgs)
