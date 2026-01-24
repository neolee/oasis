module Oasis.Tui.Event.Utils
  ( copyAllFromEditor
  , copyToClipboard
  , isBlank
  , syncVerboseList
  , clearVerboseList
  , insertMessageAt
  , updateMessageAt
  , deleteMessageAt
  , lastUserPrompt
  , hasAssistantTail
  , buildModelItems
  ) where

import Relude
import Brick.Types (EventM)
import Brick.Widgets.Edit (Editor)
import qualified Brick.Widgets.List as L
import Data.Char (isSpace)
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Vector as V
import Control.Exception (SomeException, displayException, try)
import Control.Monad.State.Class (modify)
import System.IO (hClose, hPutStr, hSetEncoding, utf8)
import System.Process (CreateProcess(..), StdStream(..), createProcess, proc, waitForProcess)
import Oasis.Tui.Event.Editor (editorTextRaw)
import Oasis.Tui.Actions.Models (providerModels, customModelItem)
import Oasis.Tui.State (AppState(..), Name)
import Oasis.Types (Config, Message(..), MessageContent(..), messageContentText)

copyAllFromEditor :: Editor Text n -> EventM Name AppState ()
copyAllFromEditor ed = do
  let content = editorTextRaw ed
  result <- liftIO (copyToClipboard content)
  case result of
    Left err -> modify (\s -> s { statusText = "Copy failed: " <> err })
    Right () -> modify (\s -> s { statusText = "Copied all text." })

copyToClipboard :: Text -> IO (Either Text ())
copyToClipboard content = do
  result <- try $ do
    (Just hin, _, _, ph) <- createProcess (proc "pbcopy" []) { std_in = CreatePipe }
    hSetEncoding hin utf8
    hPutStr hin (toString content)
    hClose hin
    _ <- waitForProcess ph
    pure ()
  case result of
    Left (err :: SomeException) -> pure (Left (T.pack (displayException err)))
    Right _ -> pure (Right ())

isBlank :: Text -> Bool
isBlank = all isSpace . toString

syncVerboseList :: [Message] -> L.List Name Message -> L.List Name Message
syncVerboseList msgs lst =
  L.listReplace (V.fromList msgs) (L.listSelected lst) lst

clearVerboseList :: L.List Name Message -> L.List Name Message
clearVerboseList = L.listReplace V.empty Nothing

insertMessageAt :: Int -> Message -> [Message] -> [Message]
insertMessageAt idx msg msgs =
  let (before, after) = splitAt idx msgs
  in before <> [msg] <> after

updateMessageAt :: Int -> Text -> [Message] -> [Message]
updateMessageAt idx contentText msgs =
  if idx < 0 || idx >= length msgs
    then msgs
    else
      let (before, after) = splitAt idx msgs
      in case after of
           [] -> msgs
           (m:rest) -> before <> [m { content = ContentText contentText }] <> rest

deleteMessageAt :: Int -> [Message] -> [Message]
deleteMessageAt idx msgs =
  if idx < 0 || idx >= length msgs
    then msgs
    else
      let (before, after) = splitAt idx msgs
      in case after of
           [] -> msgs
           (_:rest) -> before <> rest

lastUserPrompt :: [Message] -> Maybe Text
lastUserPrompt msgs =
  case reverse [messageContentText (content m) | m <- msgs, role m == "user"] of
    (t:_) | not (T.null (T.strip t)) -> Just t
    _ -> Nothing

hasAssistantTail :: [Message] -> Bool
hasAssistantTail msgs =
  case reverse msgs of
    (m:_) -> role m == "assistant"
    _ -> False

buildModelItems :: Config -> Text -> [Text] -> [Text]
buildModelItems cfg providerName customModels =
  let baseModels = filter (/= customModelItem) (providerModels cfg providerName)
      customModels' = filter (/= customModelItem) customModels
      merged = List.nub (baseModels <> customModels')
  in merged <> [customModelItem]
