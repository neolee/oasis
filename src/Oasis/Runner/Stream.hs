module Oasis.Runner.Stream
  ( forEachDelta
  , forEachDeltaContent
  , forEachDeltaReasoning
  , deltaReasoningText
  ) where

import Relude
import Oasis.Client.OpenAI.Types (ChatCompletionStreamChunk(..), StreamChoice(..), StreamDelta(..))

forEachDelta :: ChatCompletionStreamChunk -> (StreamDelta -> IO ()) -> IO ()
forEachDelta ChatCompletionStreamChunk{choices} f =
  forM_ choices $ \StreamChoice{delta} ->
    forM_ delta f

forEachDeltaContent :: ChatCompletionStreamChunk -> (Text -> IO ()) -> IO ()
forEachDeltaContent chunk f =
  forEachDelta chunk $ \StreamDelta{content} ->
    forM_ content f

forEachDeltaReasoning :: ChatCompletionStreamChunk -> (Text -> IO ()) -> IO ()
forEachDeltaReasoning chunk f =
  forEachDelta chunk $ \delta ->
    forM_ (deltaReasoningText delta) f

deltaReasoningText :: StreamDelta -> Maybe Text
deltaReasoningText StreamDelta{reasoning, thinking, reasoning_content} =
  reasoning <|> thinking <|> reasoning_content
