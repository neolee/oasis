module Oasis.Demo.Completions
  ( partialModeMessages
  , prefixCompletionMessages
  , fimCompletionRequest
  ) where

import Relude
import Oasis.Client.OpenAI.Types (CompletionRequest(..))
import Oasis.Types (Message(..), MessageContent(..))

partialModeMessages :: [Message]
partialModeMessages =
  [ Message "user" (ContentText "请对“春天来了，大地”这句话进行续写，来表达春天的美好和作者的喜悦之情") Nothing Nothing Nothing Nothing Nothing
  , Message "assistant" (ContentText "春天来了，大地") Nothing Nothing Nothing (Just True) (Just True)
  ]

prefixCompletionMessages :: [Message]
prefixCompletionMessages =
  [ Message "user" (ContentText "Please write quick sort code") Nothing Nothing Nothing Nothing Nothing
  , Message "assistant" (ContentText "```python\n") Nothing Nothing Nothing (Just True) (Just True)
  ]

fimCompletionRequest :: CompletionRequest
fimCompletionRequest =
  CompletionRequest
    { model = "demo"
    , prompt = "def fib(a):"
    , suffix = Just "    return fib(a-1) + fib(a-2)"
    , max_tokens = Just 128
    , temperature = Nothing
    , top_p = Nothing
    , stream = False
    , stop = Nothing
    , echo = Nothing
    , logprobs = Nothing
    }
