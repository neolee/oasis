module Oasis.Runner.Interactive
  ( DisplayOptions(..)
  , runInteractiveChat
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI
import Oasis.Runner.History
import Oasis.Runner.Chat (selectModelId)
import qualified Data.Text as T
import qualified System.IO as SIO
import Control.Monad (foldM)

data DisplayOptions = DisplayOptions
  { showThinking :: Bool
  } deriving (Show, Eq)

data StreamMode = InAnswer | InThinking
  deriving (Show, Eq)

data TokenPart = ThinkingPart Text | AnswerPart Text
  deriving (Show, Eq)

data StreamAccum = StreamAccum
  { mode            :: StreamMode
  , thinkingBuffer  :: Text
  , answerBuffer    :: Text
  , printedThinking :: Bool
  , printedAnswer   :: Bool
  }

runInteractiveChat :: Provider -> Text -> DisplayOptions -> IO ()
runInteractiveChat provider apiKey opts = do
  putTextLn "--- Interactive chat ---"
  putTextLn "Type /help for commands."
  loop emptyHistory
  where
    loop history = do
      putText ">>> "
      SIO.hFlush SIO.stdout
      input <- getLine
      let line = T.strip (toText input)
      if T.null line
        then loop history
        else if T.isPrefixOf "/" line
          then handleCommand history line >>= loop
          else do
            let history' = appendMessage (Message "user" line) history
            result <- streamOnce (getMessages history')
            case result of
              Left err -> do
                putTextLn ("Request failed: " <> err)
                loop history'
              Right answer -> loop (appendMessage (Message "assistant" answer) history')

    streamOnce msgs = do
      let initAccum = StreamAccum InAnswer "" "" False False
      accumRef <- newIORef initAccum
      result <- streamChatCompletion provider apiKey (selectModelId provider) msgs (handleChunk opts accumRef)
      case result of
        Left err -> pure (Left err)
        Right _ -> do
          StreamAccum{answerBuffer} <- readIORef accumRef
          putTextLn ""
          pure (Right answerBuffer)

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
      applyHistoryEdit history (insertMessage (parseIndex idxText) (Message roleText (T.unwords rest)) history)
    ("/update":idxText:roleText:rest) ->
      applyHistoryEdit history (updateMessage (parseIndex idxText) (Message roleText (T.unwords rest)) history)
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
    putTextLn (show idx <> " [" <> role <> "] " <> content)

handleChunk :: DisplayOptions -> IORef StreamAccum -> ChatCompletionStreamChunk -> IO ()
handleChunk opts accumRef ChatCompletionStreamChunk{choices = streamChoices} =
  forM_ streamChoices $ \c ->
    forM_ (delta c) $ \d ->
      case d of
        StreamDelta{reasoning = deltaReasoning, thinking = deltaThinking, content = deltaContent} -> do
          let reasoningText = deltaReasoning <|> deltaThinking
          case reasoningText of
            Just r -> emitThinking opts accumRef r
            Nothing ->
              forM_ deltaContent $ \t -> emitContent opts accumRef t

emitContent :: DisplayOptions -> IORef StreamAccum -> Text -> IO ()
emitContent opts accumRef text = do
  StreamAccum{mode, thinkingBuffer, answerBuffer, printedThinking, printedAnswer} <- readIORef accumRef
  let (newMode, parts) = processContentWithTags mode text
  (newThinking, newAnswer, pt, pa) <- foldM (emitPart opts) (thinkingBuffer, answerBuffer, printedThinking, printedAnswer) parts
  writeIORef accumRef (StreamAccum newMode newThinking newAnswer pt pa)

emitThinking :: DisplayOptions -> IORef StreamAccum -> Text -> IO ()
emitThinking opts accumRef text = do
  StreamAccum{mode, thinkingBuffer, answerBuffer, printedThinking, printedAnswer} <- readIORef accumRef
  (newThinking, newAnswer, pt, pa) <- emitPart opts (thinkingBuffer, answerBuffer, printedThinking, printedAnswer) (ThinkingPart text)
  writeIORef accumRef (StreamAccum mode newThinking newAnswer pt pa)

emitPart :: DisplayOptions -> (Text, Text, Bool, Bool) -> TokenPart -> IO (Text, Text, Bool, Bool)
emitPart DisplayOptions{showThinking} (thinkingBuffer, answerBuffer, printedThinking, printedAnswer) part =
  case part of
    ThinkingPart t
      | T.null t -> pure (thinkingBuffer, answerBuffer, printedThinking, printedAnswer)
      | not showThinking -> pure (thinkingBuffer <> t, answerBuffer, printedThinking, printedAnswer)
      | otherwise -> do
          unless printedThinking $ do
            putTextLn ""
            putTextLn "## Thinking"
          putText t
          pure (thinkingBuffer <> t, answerBuffer, True, printedAnswer)
    AnswerPart t
      | T.null t -> pure (thinkingBuffer, answerBuffer, printedThinking, printedAnswer)
      | otherwise -> do
          unless printedAnswer $ do
            when showThinking $ do
              putTextLn ""
              putTextLn "## Answer"
          when (not showThinking) $ pure ()
          putText t
          pure (thinkingBuffer, answerBuffer <> t, printedThinking, True)

processContentWithTags :: StreamMode -> Text -> (StreamMode, [TokenPart])
processContentWithTags mode text = go mode text []
  where
    go m t acc
      | T.null t = (m, acc)
      | m == InAnswer =
          case T.breakOn "<think>" t of
            (before, rest)
              | T.null rest -> (InAnswer, acc <> [AnswerPart before])
              | otherwise ->
                  let remaining = T.drop 7 rest
                  in go InThinking remaining (acc <> [AnswerPart before])
      | otherwise =
          case T.breakOn "</think>" t of
            (before, rest)
              | T.null rest -> (InThinking, acc <> [ThinkingPart before])
              | otherwise ->
                  let remaining = T.drop 8 rest
                  in go InAnswer remaining (acc <> [ThinkingPart before])
