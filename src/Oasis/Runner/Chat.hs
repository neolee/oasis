module Oasis.Runner.Chat
  ( ChatOptions(..)
  , runChat
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI
import Oasis.Chat.History
import Oasis.Runner.Common (resolveModelId, ChatParams, applyChatParams)
import Data.Aeson (eitherDecode)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified System.IO as SIO
import Control.Monad (foldM)

data ChatOptions = ChatOptions
  { streaming    :: Bool
  , showThinking :: Bool
  } deriving (Show, Eq)

runChat :: Provider -> Text -> Maybe Text -> ChatParams -> ChatOptions -> Maybe Text -> IO (Either Text ())
runChat provider apiKey modelOverride params opts initialPrompt = do
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
      let history' = appendMessage (Message "user" line Nothing Nothing) history
          msgs = getMessages history'
      result <- if streaming opts
        then streamOnce modelId msgs
        else nonStreamOnce modelId msgs
      case result of
        Left err -> pure (Left err)
        Right answer -> pure (Right (answer, appendMessage (Message "assistant" answer Nothing Nothing) history'))

    nonStreamOnce modelId msgs = do
      let reqBase = ChatCompletionRequest
            { model = modelId
            , messages = msgs
            , temperature = Nothing
            , top_p = Nothing
            , max_completion_tokens = Nothing
            , stop = Nothing
            , presence_penalty = Nothing
            , frequency_penalty = Nothing
            , seed = Nothing
            , logit_bias = Nothing
            , user = Nothing
            , service_tier = Nothing
            , reasoning_effort = Nothing
            , stream_options = Nothing
            , stream = False
            , response_format = Nothing
            , tools = Nothing
            , tool_choice = Nothing
            , parallel_tool_calls = Nothing
            }
          reqBody = applyChatParams params reqBase
      raw <- sendChatCompletionRaw provider apiKey reqBody
      case raw of
        Left err -> pure (Left (renderClientError err))
        Right body ->
          case eitherDecode body of
            Left err ->
              let rawText = TE.decodeUtf8Lenient (BL.toStrict body)
              in pure $ Left ("Failed to decode response: " <> toText err <> "\nRaw: " <> rawText)
            Right response ->
              case extractAssistantContent response of
                Nothing -> pure (Left "No assistant message returned.")
                Just content -> do
                  rendered <- renderNonStreaming opts content
                  putTextLn ""
                  pure (Right rendered)

    streamOnce modelId msgs = do
      let initAccum = StreamAccum InAnswer "" "" False False
          reqBase = ChatCompletionRequest
            { model = modelId
            , messages = msgs
            , temperature = Nothing
            , top_p = Nothing
            , max_completion_tokens = Nothing
            , stop = Nothing
            , presence_penalty = Nothing
            , frequency_penalty = Nothing
            , seed = Nothing
            , logit_bias = Nothing
            , user = Nothing
            , service_tier = Nothing
            , reasoning_effort = Nothing
            , stream_options = Nothing
            , stream = True
            , response_format = Nothing
            , tools = Nothing
            , tool_choice = Nothing
            , parallel_tool_calls = Nothing
            }
          reqBody = applyChatParams params reqBase
      accumRef <- newIORef initAccum
      result <- streamChatCompletionWithRequest provider apiKey reqBody (handleChunk opts accumRef)
      case result of
        Left err -> pure (Left (renderClientError err))
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
      applyHistoryEdit history (insertMessage (parseIndex idxText) (Message roleText (T.unwords rest) Nothing Nothing) history)
    ("/update":idxText:roleText:rest) ->
      applyHistoryEdit history (updateMessage (parseIndex idxText) (Message roleText (T.unwords rest) Nothing Nothing) history)
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

extractAssistantContent :: ChatCompletionResponse -> Maybe Text
extractAssistantContent ChatCompletionResponse{choices} =
  case choices of
    (ChatChoice{message = Just Message{content}}:_) -> Just content
    _ -> Nothing

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

handleChunk :: ChatOptions -> IORef StreamAccum -> ChatCompletionStreamChunk -> IO ()
handleChunk opts accumRef ChatCompletionStreamChunk{choices = streamChoices} =
  forM_ streamChoices $ \c ->
    forM_ (delta c) $ \StreamDelta{reasoning = deltaReasoning, thinking = deltaThinking, reasoning_content = deltaReasoningContent, content = deltaContent} -> do
      let reasoningText = deltaReasoning <|> deltaThinking <|> deltaReasoningContent
      case reasoningText of
        Just r -> emitThinking (showThinking opts) accumRef r
        Nothing ->
          forM_ deltaContent $ \t -> emitContent (showThinking opts) accumRef t

renderNonStreaming :: ChatOptions -> Text -> IO Text
renderNonStreaming opts content = do
  let initAccum = StreamAccum InAnswer "" "" False False
  accumRef <- newIORef initAccum
  emitContent (showThinking opts) accumRef content
  StreamAccum{answerBuffer} <- readIORef accumRef
  pure answerBuffer

emitContent :: Bool -> IORef StreamAccum -> Text -> IO ()
emitContent showThinking accumRef text = do
  StreamAccum{mode, thinkingBuffer, answerBuffer, printedThinking, printedAnswer} <- readIORef accumRef
  let (newMode, parts) = processContentWithTags mode text
  (newThinking, newAnswer, pt, pa) <- foldM (emitPart showThinking) (thinkingBuffer, answerBuffer, printedThinking, printedAnswer) parts
  writeIORef accumRef (StreamAccum newMode newThinking newAnswer pt pa)

emitThinking :: Bool -> IORef StreamAccum -> Text -> IO ()
emitThinking showThinking accumRef text = do
  StreamAccum{mode, thinkingBuffer, answerBuffer, printedThinking, printedAnswer} <- readIORef accumRef
  (newThinking, newAnswer, pt, pa) <- emitPart showThinking (thinkingBuffer, answerBuffer, printedThinking, printedAnswer) (ThinkingPart text)
  writeIORef accumRef (StreamAccum mode newThinking newAnswer pt pa)

emitPart :: Bool -> (Text, Text, Bool, Bool) -> TokenPart -> IO (Text, Text, Bool, Bool)
emitPart showThinking (thinkingBuffer, answerBuffer, printedThinking, printedAnswer) part =
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
          unless showThinking $ pure ()
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
