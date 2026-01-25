module Oasis.Runner.PartialMode
  ( PartialModeResult
  , runPartialMode
  , runPartialModeDetailed
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI
import Oasis.Model (resolveModelId)
import Oasis.Client.OpenAI.Param (ChatParams, applyChatParams)
import Oasis.Client.OpenAI.Context (extractAssistantContent)
import Oasis.Runner.Result (encodeRequestJson, buildRequestResponse)

type PartialModeResult = RequestResponse ChatCompletionResponse

runPartialMode :: Provider -> Text -> Maybe Text -> ChatParams -> Bool -> IO (Either Text ())
runPartialMode provider apiKey modelOverride params useBeta = do
  detailed <- runPartialModeDetailed provider apiKey modelOverride params useBeta
  case detailed of
    Left err -> pure (Left err)
    Right RequestResponse{response} ->
      case response >>= extractAssistantContent of
        Nothing -> pure (Left "No assistant message returned.")
        Just content -> do
          putTextLn content
          pure (Right ())

runPartialModeDetailed :: Provider -> Text -> Maybe Text -> ChatParams -> Bool -> IO (Either Text PartialModeResult)
runPartialModeDetailed provider apiKey modelOverride params useBeta = do
  let modelId = resolveModelId provider modelOverride
      messages =
        [ Message "user" (ContentText "请对“春天来了，大地”这句话进行续写，来表达春天的美好和作者的喜悦之情") Nothing Nothing Nothing Nothing
        , Message "assistant" (ContentText "春天来了，大地") Nothing Nothing (Just True) (Just True)
        ]
      reqBase = defaultChatRequest modelId messages
      reqBody = applyChatParams params reqBase
      reqJsonText = encodeRequestJson reqBody
  result <- sendChatCompletionRawWithHooks emptyClientHooks provider apiKey reqBody useBeta
  pure (buildRequestResponse reqJsonText result)
