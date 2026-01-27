module Oasis.Runner.ToolCalling
  ( ToolCallingInput(..)
  , ToolCallingResult(..)
  , buildToolCallingRequest
  , runToolCalling
  , runToolCallingDetailed
  , runToolCallingWithRequest
  , runToolCallingWithRequestWithHooks
  ) where

import Relude
import Oasis.Types (Provider, Message, Tool)
import qualified Oasis.Types as OT
import Oasis.Client.OpenAI
  ( ChatCompletionRequest(..)
  , defaultChatRequest
  , requestChat
  , sendChatCompletionRawWithHooks
  , emptyClientHooks
  , ClientHooks(..)
  )
import qualified Oasis.Chat.Message as Msg
import Oasis.Model (resolveModelId)
import Oasis.Client.OpenAI.Param (ChatParams, applyChatParams)
import Oasis.Client.OpenAI.Context (extractAssistantContent, extractToolCall)
import Oasis.Output.Common (encodeJsonText)
import Oasis.Runner.Result (parseRawResponseStrict)

data ToolCallingInput = ToolCallingInput
  { toolMessages :: [Message]
  , toolDefs :: [Tool]
  , toolParallelCalls :: Maybe Bool
  } deriving (Show, Eq)

data ToolCallingResult
  = ToolCallingNoToolCall Text
  | ToolCallingToolError Text Text
  | ToolCallingSecondError Text Text Text
  | ToolCallingSuccess Text Text Text
  deriving (Show, Eq)

runToolCalling
  :: Provider
  -> Text
  -> Maybe Text
  -> ChatParams
  -> ToolCallingInput
  -> (OT.ToolCall -> IO (Either Text Text))
  -> Bool
  -> IO (Either Text ToolCallingResult)
runToolCalling = runToolCallingDetailed

runToolCallingDetailed
  :: Provider
  -> Text
  -> Maybe Text
  -> ChatParams
  -> ToolCallingInput
  -> (OT.ToolCall -> IO (Either Text Text))
  -> Bool
  -> IO (Either Text ToolCallingResult)
runToolCallingDetailed provider apiKey modelOverride params ToolCallingInput{toolMessages, toolDefs, toolParallelCalls} executeToolCall useBeta = do
  let modelId = resolveModelId provider modelOverride
      messages0 = toolMessages
  let reqBase0 = (defaultChatRequest modelId messages0)
        { tools = Just toolDefs
        , parallel_tool_calls = toolParallelCalls
        }
  firstResp <- requestChat provider apiKey params reqBase0 useBeta
  case parseRawResponseStrict firstResp of
    Left err -> pure (Left err)
    Right (_, response) -> do
      case extractToolCall response of
        Nothing -> do
          case extractAssistantContent response of
            Nothing -> pure (Left "No assistant message returned.")
            Just content -> pure (Right (ToolCallingNoToolCall content))
        Just (assistantMessage, toolCall) -> do
          result <- executeToolCall toolCall
          let toolCallJson = encodeJsonText toolCall
          case result of
            Left terr -> pure (Right (ToolCallingToolError toolCallJson terr))
            Right toolMsgText -> do
              let OT.ToolCall toolCallId _ _ = toolCall
                  toolMsg = Msg.toolMessage toolCallId toolMsgText
                  messages1 = messages0 <> [assistantMessage, toolMsg]
                  reqBase2 = (defaultChatRequest modelId messages1)
                    { tools = Just toolDefs
                    }
              secondResp <- requestChat provider apiKey params reqBase2 useBeta
              case parseRawResponseStrict secondResp of
                Left err -> pure (Right (ToolCallingSecondError toolCallJson toolMsgText err))
                Right (_, response2) ->
                  case extractAssistantContent response2 of
                    Nothing -> pure (Left "No assistant message returned.")
                    Just content -> pure (Right (ToolCallingSuccess toolCallJson toolMsgText content))

buildToolCallingRequest :: Text -> ChatParams -> [Message] -> [Tool] -> Maybe Bool -> ChatCompletionRequest
buildToolCallingRequest modelId params messages toolDefs toolParallelCalls =
  let reqBase = (defaultChatRequest modelId messages)
        { tools = Just toolDefs
        , parallel_tool_calls = toolParallelCalls
        }
  in applyChatParams params reqBase

runToolCallingWithRequest :: Provider -> Text -> ChatCompletionRequest -> (OT.ToolCall -> IO (Either Text Text)) -> Bool -> IO (Either Text ToolCallingResult)
runToolCallingWithRequest = runToolCallingWithRequestWithHooks emptyClientHooks

runToolCallingWithRequestWithHooks
  :: ClientHooks
  -> Provider
  -> Text
  -> ChatCompletionRequest
  -> (OT.ToolCall -> IO (Either Text Text))
  -> Bool
  -> IO (Either Text ToolCallingResult)
runToolCallingWithRequestWithHooks hooks provider apiKey reqBody0 executeToolCall useBeta = do
  if stream reqBody0
    then pure (Left "tool-calling runner requires stream=false")
    else do
      let messages0 = messages reqBody0
      firstResp <- sendChatCompletionRawWithHooks hooks provider apiKey reqBody0 useBeta
      case parseRawResponseStrict firstResp of
        Left err -> pure (Left err)
        Right (_, response) ->
          case extractToolCall response of
            Nothing -> do
              case extractAssistantContent response of
                Nothing -> pure (Left "No assistant message returned.")
                Just content -> pure (Right (ToolCallingNoToolCall content))
            Just (assistantMessage, toolCall) -> do
              result <- executeToolCall toolCall
              let toolCallJson = encodeJsonText toolCall
              case result of
                Left terr -> pure (Right (ToolCallingToolError toolCallJson terr))
                Right toolMsgText -> do
                  let OT.ToolCall toolCallId _ _ = toolCall
                      toolMsg = Msg.toolMessage toolCallId toolMsgText
                      messages1 = messages0 <> [assistantMessage, toolMsg]
                      reqBody1 = reqBody0 { messages = messages1, stream = False }
                  secondResp <- sendChatCompletionRawWithHooks hooks provider apiKey reqBody1 useBeta
                  case parseRawResponseStrict secondResp of
                    Left err -> pure (Right (ToolCallingSecondError toolCallJson toolMsgText err))
                    Right (_, response2) ->
                      case extractAssistantContent response2 of
                        Nothing -> pure (Left "No assistant message returned.")
                        Just content -> pure (Right (ToolCallingSuccess toolCallJson toolMsgText content))
