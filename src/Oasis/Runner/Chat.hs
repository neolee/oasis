module Oasis.Runner.Chat
  ( ChatStreamEvent(..)
  , ChatResult(..)
  , buildChatRequest
  , runChatOnce
  , streamChatOnce
  , streamChatWithRequest
  , normalizeChatMessages
  ) where

import Relude
import Oasis.Types
import Oasis.Types (messageContentText)
import Oasis.Client.OpenAI
import Oasis.Client.OpenAI.Types (setChatStream)
import Oasis.Model (resolveModelId)
import Oasis.Client.OpenAI.Param (ChatParams, applyChatParams)
import Oasis.Client.OpenAI.Context (extractAssistantContent)
import Oasis.Runner.Stream (forEachDelta, deltaReasoningText)
import Oasis.Runner.Result (parseRawResponseStrict)
import qualified Data.Text as T
import Control.Monad (foldM)

data ChatStreamEvent
  = ChatThinking Text
  | ChatAnswer Text
  deriving (Show, Eq)

data ChatResult = ChatResult
  { thinkingText :: Text
  , answerText :: Text
  } deriving (Show, Eq)

runChatOnce :: Provider -> Text -> Maybe Text -> ChatParams -> [Message] -> Bool -> IO (Either Text ChatResult)
runChatOnce provider apiKey modelOverride params messages useBeta = do
  let modelId = resolveModelId provider modelOverride
      reqBody = buildChatRequest modelId params messages
  raw <- sendChatCompletionRawWithHooks emptyClientHooks provider apiKey reqBody useBeta
  case parseRawResponseStrict raw of
    Left err -> pure (Left err)
    Right (_, response) ->
      case extractAssistantContent response of
        Nothing -> pure (Left "No assistant message returned.")
        Just content ->
          let (_, parts) = processContentWithTags InAnswer content
              (thinking, answer) = collectParts ("", "") parts
          in pure (Right (ChatResult thinking answer))

streamChatOnce
  :: Provider
  -> Text
  -> Maybe Text
  -> ChatParams
  -> [Message]
  -> (ChatStreamEvent -> IO ())
  -> Bool
  -> IO (Either Text ChatResult)
streamChatOnce provider apiKey modelOverride params messages onEvent useBeta = do
  let modelId = resolveModelId provider modelOverride
      reqBase = setChatStream True (buildChatRequest modelId params messages)
  streamChatWithRequest provider apiKey reqBase onEvent useBeta

handleChunk :: IORef StreamState -> (ChatStreamEvent -> IO ()) -> ChatCompletionStreamChunk -> IO ()
handleChunk stateRef onEvent chunk =
  forEachDelta chunk $ \delta@StreamDelta{content} -> do
    StreamState{streamMode, streamThinking, streamAnswer} <- readIORef stateRef
    case deltaReasoningText delta of
      Nothing -> pure ()
      Just r -> do
        onEvent (ChatThinking r)
        writeIORef stateRef (StreamState streamMode (streamThinking <> r) streamAnswer)
    forM_ content $ \t -> do
      StreamState{streamMode = mode0, streamThinking = thinking0, streamAnswer = answer0} <- readIORef stateRef
      let (newMode, parts) = processContentWithTags mode0 t
      (thinking1, answer1) <- emitParts onEvent (thinking0, answer0) parts
      writeIORef stateRef (StreamState newMode thinking1 answer1)

data StreamMode = InAnswer | InThinking
  deriving (Show, Eq)

data TokenPart = ThinkingPart Text | AnswerPart Text
  deriving (Show, Eq)

data StreamState = StreamState
  { streamMode :: StreamMode
  , streamThinking :: Text
  , streamAnswer :: Text
  }

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

collectParts :: (Text, Text) -> [TokenPart] -> (Text, Text)
collectParts = foldl' step
  where
    step (thinkingAcc, answerAcc) part =
      case part of
        ThinkingPart t -> (thinkingAcc <> t, answerAcc)
        AnswerPart t -> (thinkingAcc, answerAcc <> t)

emitParts :: (ChatStreamEvent -> IO ()) -> (Text, Text) -> [TokenPart] -> IO (Text, Text)
emitParts onEvent = foldM step
  where
    step (thinkingAcc, answerAcc) part =
      case part of
        ThinkingPart t
          | T.null t -> pure (thinkingAcc, answerAcc)
          | otherwise -> do
              onEvent (ChatThinking t)
              pure (thinkingAcc <> t, answerAcc)
        AnswerPart t
          | T.null t -> pure (thinkingAcc, answerAcc)
          | otherwise -> do
              onEvent (ChatAnswer t)
              pure (thinkingAcc, answerAcc <> t)

normalizeChatMessages :: [Message] -> [Message]
normalizeChatMessages msgs =
  case reverse msgs of
    (Message{role = msgRole, content = msgContent, tool_calls = msgToolCalls, reasoning_content = msgReasoning}:rest)
      | msgRole == "assistant"
      , isPlaceholder msgContent msgToolCalls msgReasoning -> reverse rest
    _ -> msgs
  where
    isPlaceholder msgContent msgToolCalls msgReasoning =
      T.null (T.strip (messageContentText msgContent))
        && isNothing msgToolCalls
        && isNothing msgReasoning

buildChatRequest :: Text -> ChatParams -> [Message] -> ChatCompletionRequest
buildChatRequest modelId params messages =
  applyChatParams params (defaultChatRequest modelId (normalizeChatMessages messages))

streamChatWithRequest
  :: Provider
  -> Text
  -> ChatCompletionRequest
  -> (ChatStreamEvent -> IO ())
  -> Bool
  -> IO (Either Text ChatResult)
streamChatWithRequest provider apiKey reqBody onEvent useBeta = do
  let reqBody' = reqBody { messages = normalizeChatMessages (messages reqBody) }
      initState = StreamState InAnswer "" ""
  stateRef <- newIORef initState
  result <- streamChatCompletionWithRequestWithHooks emptyClientHooks provider apiKey reqBody' (handleChunk stateRef onEvent) useBeta
  case result of
    Left err -> pure (Left (renderClientError err))
    Right _ -> do
      StreamState{streamThinking, streamAnswer} <- readIORef stateRef
      pure (Right (ChatResult streamThinking streamAnswer))
