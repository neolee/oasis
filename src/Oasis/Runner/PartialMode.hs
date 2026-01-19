module Oasis.Runner.PartialMode
  ( runPartialMode
  ) where

import Relude
import Oasis.Types
import Oasis.Client.OpenAI
import Oasis.Runner.Common (resolveModelId, ChatParams, applyChatParams, extractAssistantContent)
import Oasis.Runner.Result (parseRawResponseStrict)

runPartialMode :: Provider -> Text -> Maybe Text -> ChatParams -> Bool -> IO (Either Text ())
runPartialMode provider apiKey modelOverride params useBeta = do
  let modelId = resolveModelId provider modelOverride
      messages =
        [ Message "user" (ContentText "请对“春天来了，大地”这句话进行续写，来表达春天的美好和作者的喜悦之情") Nothing Nothing Nothing Nothing
        , Message "assistant" (ContentText "春天来了，大地") Nothing Nothing (Just True) (Just True)
        ]
      -- Use beta URL for this feature
      reqBase = defaultChatRequest modelId messages
      reqBody = applyChatParams params reqBase
  
  result <- sendChatCompletionRawWithHooks emptyClientHooks provider apiKey reqBody useBeta
  case parseRawResponseStrict result of
    Left err -> pure (Left err)
    Right (_, response) ->
      case extractAssistantContent response of
        Nothing -> pure (Left "No assistant message returned.")
        Just content -> do
          putTextLn content
          pure (Right ())
