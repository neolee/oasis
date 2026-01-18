{-# LANGUAGE StrictData #-}

module Oasis.Client.OpenAI.Stream
  ( streamSseData
  ) where

import Relude
import Oasis.Client.OpenAI.Types (ClientError(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client (BodyReader, brRead)

data SseEvent = SseEvent
  { eventType :: Maybe BS.ByteString
  , eventId   :: Maybe BS.ByteString
  , dataLines :: [BS.ByteString]
  }

streamSseData :: BodyReader -> (BS.ByteString -> IO (Either ClientError ())) -> IO (Either ClientError ())
streamSseData reader onData = loop BS.empty emptyEvent
  where
    emptyEvent = SseEvent Nothing Nothing []

    loop buffer event = do
      chunk <- brRead reader
      if BS.null chunk
        then flushEvent event
        else do
          let combined = buffer <> chunk
              (lines, rest) = splitLines combined
          result <- processLines lines event
          case result of
            Left err -> pure (Left err)
            Right (done, event') ->
              if done
                then pure (Right ())
                else loop rest event'

    splitLines bs
      | BS.null bs = ([], BS.empty)
      | BS8.last bs == '\n' = (BS8.split '\n' bs, BS.empty)
      | otherwise =
          let parts = BS8.split '\n' bs
          in case reverse parts of
               [] -> ([], BS.empty)
               (lastPart:revInit) -> (reverse revInit, lastPart)

    processLines [] event = pure (Right (False, event))
    processLines (l:ls) event = do
      let line = stripCr l
      if BS.null line
        then do
          result <- dispatchEvent event
          case result of
            Left err -> pure (Left err)
            Right done ->
              if done
                then pure (Right (True, emptyEvent))
                else processLines ls emptyEvent
        else if BS8.isPrefixOf ":" line
          then processLines ls event
          else do
            let (field, value) = splitField line
            let event' = applyField field value event
            processLines ls event'

    stripCr bs
      | BS.null bs = bs
      | BS8.last bs == '\r' = BS8.init bs
      | otherwise = bs

    splitField line =
      let (field, rest) = BS8.break (== ':') line
      in if BS.null rest
           then (field, BS.empty)
           else
             let value = BS8.drop 1 rest
             in (field, if BS8.isPrefixOf " " value then BS8.drop 1 value else value)

    applyField field value event
      | field == "data" = event { dataLines = dataLines event <> [value] }
      | field == "event" = event { eventType = Just value }
      | field == "id" = event { eventId = Just value }
      | otherwise = event

    dispatchEvent event =
      case dataLines event of
        [] -> pure (Right False)
        lines' ->
          let payload = BS8.intercalate "\n" lines'
          in if payload == "[DONE]"
               then pure (Right True)
               else do
                 result <- onData payload
                 case result of
                   Left err -> pure (Left err)
                   Right _ -> pure (Right False)

    flushEvent event = do
      result <- dispatchEvent event
      case result of
        Left err -> pure (Left err)
        Right _ -> pure (Right ())
