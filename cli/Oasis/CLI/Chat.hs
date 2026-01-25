module Oasis.CLI.Chat
  ( runChatInteractive
  ) where

import Relude
import Oasis.Types
import Oasis.Model (resolveModelId)
import Oasis.Chat.History
import qualified Oasis.Chat.Message as Msg
import Oasis.Client.OpenAI.Param (ChatParams)
import Oasis.Runner.Chat
  ( ChatStreamEvent(..)
  , ChatResult(..)
  , runChatOnce
  , streamChatOnce
  )
import qualified Data.Text as T
import qualified System.IO as SIO
import Control.Monad (foldM)

runChatInteractive
  :: Provider
  -> Text
  -> Maybe Text
  -> ChatParams
  -> Bool
  -> Bool
  -> Bool
  -> Maybe Text
  -> IO (Either Text ())
runChatInteractive provider apiKey modelOverride params useStream showThinking useBeta initialPrompt = do
  let modelId = resolveModelId provider modelOverride
  putTextLn "--- Chat ---"
  putTextLn "Type /help for commands."
  historyAfterFirst <- case initialPrompt of
    Nothing -> pure (Right emptyHistory)
    Just firstLine -> do
      result <- chatOnce modelId emptyHistory firstLine
      pure (fmap snd result)
  case historyAfterFirst of
    Left err -> pure (Left err)
    Right h -> loop modelId h
  where
    loop modelId history = do
      putText ">>> "
      SIO.hFlush SIO.stdout
      input <- getLine
      let line = T.strip (toText input)
      if T.null line
        then loop modelId history
        else if T.isPrefixOf "/" line
          then handleCommand history line >>= loop modelId
          else do
            result <- chatOnce modelId history line
            case result of
              Left err -> pure (Left err)
              Right (_, newHistory) -> loop modelId newHistory

    chatOnce modelId history line = do
      let history' = appendMessage (Msg.userMessage line) history
          msgs = getMessages history'
      result <- if useStream
        then streamOnce msgs
        else nonStreamOnce msgs
      case result of
        Left err -> pure (Left err)
        Right answer -> pure (Right (answer, appendMessage (Msg.assistantMessage answer) history'))

    nonStreamOnce msgs = do
      result <- runChatOnce provider apiKey modelOverride params msgs useBeta
      case result of
        Left err -> pure (Left err)
        Right ChatResult{thinkingText, answerText} -> do
          renderNonStreaming showThinking thinkingText answerText
          pure (Right answerText)

    streamOnce msgs = do
      stateRef <- newIORef (StreamRender False False)
      result <- streamChatOnce provider apiKey modelOverride params msgs (handleEvent showThinking stateRef) useBeta
      case result of
        Left err -> pure (Left err)
        Right ChatResult{answerText} -> do
          putTextLn ""
          pure (Right answerText)

handleCommand :: History -> Text -> IO History
handleCommand history line =
  case T.words line of
    ["/help"] -> do
      putTextLn "Commands:"
      putTextLn "/show"
      putTextLn "/system <text>"
      putTextLn "/insert <index> <role> <text>"
      putTextLn "/update <index> <role> <text>"
      putTextLn "/delete <index>"
      putTextLn "/exit"
      pure history
    ["/exit"] -> do
      putTextLn "Bye."
      exitSuccess
    ["/show"] -> do
      showHistory history
      pure history
    ("/system":rest) -> do
      let contentText = T.unwords rest
      pure (setSystemMessage contentText history)
    ("/insert":idxText:roleText:rest) ->
      applyHistoryEdit history (mkRoleMessage roleText (T.unwords rest) >>= \msg -> insertMessage (parseIndex idxText) msg history)
    ("/update":idxText:roleText:rest) ->
      applyHistoryEdit history (mkRoleMessage roleText (T.unwords rest) >>= \msg -> updateMessage (parseIndex idxText) msg history)
    ("/delete":idxText:_) ->
      applyHistoryEdit history (deleteMessage (parseIndex idxText) history)
    _ -> do
      putTextLn "Unknown command. Type /help"
      pure history
  where
    parseIndex t = fromMaybe (-1) (readMaybe (toString t))

applyHistoryEdit :: History -> Either Text History -> IO History
applyHistoryEdit history = \case
  Left err -> do
    putTextLn ("History edit failed: " <> err)
    pure history
  Right h -> pure h

showHistory :: History -> IO ()
showHistory history =
  forM_ (zip [0..] (getMessages history)) $ \(idx, Message{role, content}) ->
    putTextLn (show idx <> " [" <> role <> "] " <> messageContentText content)

mkRoleMessage :: Text -> Text -> Either Text Message
mkRoleMessage roleText contentText =
  let roleLower = T.toLower (T.strip roleText)
  in case roleLower of
    "system" -> Right (Msg.systemMessage contentText)
    "user" -> Right (Msg.userMessage contentText)
    "assistant" -> Right (Msg.assistantMessage contentText)
    "tool" -> Right (Msg.plainMessage "tool" contentText)
    _ -> Left ("Unknown role: " <> roleText)

renderNonStreaming :: Bool -> Text -> Text -> IO ()
renderNonStreaming showThinking thinking answer = do
  when showThinking $ do
    unless (T.null (T.strip thinking)) $ do
      putTextLn ""
      putTextLn "## Thinking"
      putText thinking
  unless (T.null (T.strip answer)) $ do
    when showThinking $ do
      putTextLn ""
      putTextLn "## Answer"
    putText answer
  putTextLn ""

handleEvent :: Bool -> IORef StreamRender -> ChatStreamEvent -> IO ()
handleEvent showThinking stateRef event = do
  StreamRender{printedThinking, printedAnswer} <- readIORef stateRef
  case event of
    ChatThinking t
      | not showThinking -> pure ()
      | T.null t -> pure ()
      | otherwise -> do
          unless printedThinking $ do
            putTextLn ""
            putTextLn "## Thinking"
          putText t
          writeIORef stateRef (StreamRender True printedAnswer)
    ChatAnswer t
      | T.null t -> pure ()
      | otherwise -> do
          unless printedAnswer $ do
            when showThinking $ do
              putTextLn ""
              putTextLn "## Answer"
          putText t
          writeIORef stateRef (StreamRender printedThinking True)

data StreamRender = StreamRender
  { printedThinking :: Bool
  , printedAnswer :: Bool
  }
