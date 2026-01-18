module Main where

import Relude
import Oasis.Config
import Oasis.Types
import Oasis.Runner.Basic
import Oasis.Runner.Chat
import Oasis.Runner.Common (resolveModelId)
import qualified Data.Text as T
import qualified Data.List as L

main :: IO ()
main = do
  args <- getArgs
  case args of
    (aliasArg:modelArg:runnerArg:runnerArgs) -> do
      let alias = toText aliasArg
          modelText = toText modelArg
          modelOverride = normalizeModel modelText
          runnerName = T.toLower (toText runnerArg)
      mPath <- findConfig
      case mPath of
        Nothing -> do
          putTextLn "Error: providers.toml not found in current directory, config/, or local/."
          exitFailure
        Just path -> do
          cfgResult <- loadConfig path
          case cfgResult of
            Left err -> do
              putTextLn $ "Error parsing config: " <> err
              exitFailure
            Right cfg -> do
              res <- resolveProvider cfg alias
              case res of
                Nothing -> do
                  putTextLn $ "Provider not found for alias: " <> alias
                  exitFailure
                Just (p, key) -> dispatchRunner alias p key modelOverride runnerName runnerArgs
    _ -> do
      putTextLn "Usage: oasis-cli <provider> <model|default|-> <runner> [runner args...]"
      putTextLn "Runners: basic, chat"
      putTextLn "Chat runner args: [--no-stream] [--hide-thinking] [initial prompt...]"
      putTextLn "Basic runner args: <prompt...>"
      exitFailure

normalizeModel :: Text -> Maybe Text
normalizeModel t
  | T.toLower (T.strip t) `elem` ["default", "-"] = Nothing
  | T.null (T.strip t) = Nothing
  | otherwise = Just t

dispatchRunner :: Text -> Provider -> Text -> Maybe Text -> Text -> [String] -> IO ()
dispatchRunner alias provider apiKey modelOverride runnerName runnerArgs =
  case runnerName of
    "basic" -> do
      putTextLn $ "Loading config for alias: " <> alias
      putTextLn "--- Resolved Provider ---"
      print provider
      if apiKey /= ""
        then putTextLn "API Key: Found (hidden)"
        else putTextLn "API Key: NOT FOUND (Check environment variables)"
      let prompt = T.unwords (map toText runnerArgs)
      if T.null (T.strip prompt)
        then do
          putTextLn "Basic runner requires a prompt."
          exitFailure
        else do
          putTextLn $ "Using model: " <> resolveModelId provider modelOverride
          result <- runBasic provider apiKey modelOverride prompt
          case result of
            Left err -> do
              putTextLn $ "Request failed: " <> err
              exitFailure
            Right BasicResult{requestJson, responseJson, response} -> do
              putTextLn "--- Request JSON ---"
              putTextLn requestJson
              putTextLn "--- Response JSON ---"
              putTextLn responseJson
              when (isNothing response) $
                putTextLn "Warning: response JSON could not be decoded."
    "chat" -> do
      let (flags, rest) = L.partition (L.isPrefixOf "--") runnerArgs
          useStream = "--no-stream" `notElem` flags
          showThinking = "--hide-thinking" `notElem` flags
          initialPrompt = if null rest then Nothing else Just (T.unwords (map toText rest))
      putTextLn $ "Using model: " <> resolveModelId provider modelOverride
      result <- runChat provider apiKey modelOverride (ChatOptions useStream showThinking) initialPrompt
      case result of
        Left err -> do
          putTextLn $ "Request failed: " <> err
          exitFailure
        Right _ -> pure ()
    _ -> do
      putTextLn $ "Unknown runner: " <> runnerName
      putTextLn "Runners: basic, chat"
      exitFailure
