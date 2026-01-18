module Main where

import Relude
import Oasis.Config
import Oasis.Types
import Oasis.Runner.Basic
import Oasis.Runner.Chat
import Oasis.Runner.GetModels
import Oasis.Runner.StructuredOutput
import Oasis.Runner.Embeddings
import Oasis.Runner.HooksDemo
import Oasis.Runner.ToolCalling
import Oasis.Runner.Common (resolveModelId, parseChatParams)
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (eitherDecode)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

main :: IO ()
main = do
  setLocaleEncoding utf8
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
      putTextLn "Runners: basic, chat, models, structured-json, structured-schema, tool-calling, embeddings, hooks-demo"
      putTextLn "Chat runner args: [--no-stream] [--hide-thinking] [--extra-args <json>] [initial prompt...]"
      putTextLn "Basic runner args: [--extra-args <json>] [--raw <json>] <prompt...>"
      putTextLn "Structured runner args: [--extra-args <json>]"
      putTextLn "Tool calling runner args: [--extra-args <json>]"
      putTextLn "Embeddings runner args: [--extra-args <json>] <text...>"
      putTextLn "Hooks demo runner args: [--extra-args <json>] <prompt...>"
      exitFailure

normalizeModel :: Text -> Maybe Text
normalizeModel t
  | T.toLower (T.strip t) `elem` ["default", "-"] = Nothing
  | T.null (T.strip t) = Nothing
  | otherwise = Just t

extractExtraArgs :: [String] -> Either Text (Maybe Text, [String])
extractExtraArgs = go Nothing []
  where
    go found acc = \case
      [] -> Right (found, reverse acc)
      (x:xs)
        | x == "--extra-args" ->
            case xs of
              [] -> Left "Missing value for --extra-args"
              (v:rest) ->
                if isJust found
                  then Left "--extra-args specified more than once"
                  else go (Just (toText v)) acc rest
        | "--extra-args=" `L.isPrefixOf` x ->
            let prefix = "--extra-args=" :: String
                v = drop (length prefix) x
            in if isJust found
              then Left "--extra-args specified more than once"
              else go (Just (toText v)) acc xs
        | otherwise -> go found (x:acc) xs

extractRawArgs :: [String] -> Either Text (Maybe Text, [String])
extractRawArgs = go Nothing []
  where
    go found acc = \case
      [] -> Right (found, reverse acc)
      (x:xs)
        | x == "--raw" ->
            case xs of
              [] -> Left "Missing value for --raw"
              (v:rest) ->
                if isJust found
                  then Left "--raw specified more than once"
                  else go (Just (toText v)) acc rest
        | "--raw=" `L.isPrefixOf` x ->
            let prefix = "--raw=" :: String
                v = drop (length prefix) x
            in if isJust found
              then Left "--raw specified more than once"
              else go (Just (toText v)) acc xs
        | otherwise -> go found (x:acc) xs

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
      case extractExtraArgs runnerArgs of
        Left err -> do
          putTextLn err
          exitFailure
        Right (extraArgsText, restArgs) -> do
          params <- case parseChatParams extraArgsText of
            Left err -> do
              putTextLn err
              exitFailure
            Right p -> pure p
          case extractRawArgs restArgs of
            Left err -> do
              putTextLn err
              exitFailure
            Right (rawText, restArgs2) -> do
              case rawText of
                Just rawJson -> do
                  unless (null restArgs2) $ do
                    putTextLn "Basic runner with --raw does not accept positional prompt."
                    exitFailure
                  messages <- case eitherDecode (BL.fromStrict (TE.encodeUtf8 rawJson)) of
                    Left err -> do
                      putTextLn $ "Invalid --raw JSON: " <> toText err
                      exitFailure
                    Right msgs -> pure msgs
                  putTextLn $ "Using model: " <> resolveModelId provider modelOverride
                  result <- runBasicRaw provider apiKey modelOverride params messages
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
                Nothing -> do
                  let prompt = T.unwords (map toText restArgs2)
                  if T.null (T.strip prompt)
                    then do
                      putTextLn "Basic runner requires a prompt."
                      exitFailure
                    else do
                      putTextLn $ "Using model: " <> resolveModelId provider modelOverride
                      result <- runBasic provider apiKey modelOverride params prompt
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
      case extractExtraArgs runnerArgs of
        Left err -> do
          putTextLn err
          exitFailure
        Right (extraArgsText, restArgs) -> do
          params <- case parseChatParams extraArgsText of
            Left err -> do
              putTextLn err
              exitFailure
            Right p -> pure p
          let (flags, rest) = L.partition (L.isPrefixOf "--") restArgs
              useStream = "--no-stream" `notElem` flags
              showThinking = "--hide-thinking" `notElem` flags
              initialPrompt = if null rest then Nothing else Just (T.unwords (map toText rest))
          putTextLn $ "Using model: " <> resolveModelId provider modelOverride
          result <- runChat provider apiKey modelOverride params (ChatOptions useStream showThinking) initialPrompt
          case result of
            Left err -> do
              putTextLn $ "Request failed: " <> err
              exitFailure
            Right _ -> pure ()
    "models" -> do
      putTextLn $ "Loading config for alias: " <> alias
      putTextLn "--- Resolved Provider ---"
      print provider
      if apiKey /= ""
        then putTextLn "API Key: Found (hidden)"
        else putTextLn "API Key: NOT FOUND (Check environment variables)"
      result <- runGetModels provider apiKey
      case result of
        Left err -> do
          putTextLn $ "Request failed: " <> err
          exitFailure
        Right GetModelsResult{responseJson, response} -> do
          putTextLn "--- Response JSON ---"
          putTextLn responseJson
          when (isNothing response) $
            putTextLn "Warning: response JSON could not be decoded."
    "structured-json" -> do
      case extractExtraArgs runnerArgs of
        Left err -> do
          putTextLn err
          exitFailure
        Right (extraArgsText, restArgs) -> do
          unless (null restArgs) $ do
            putTextLn "Structured runner does not accept positional args."
            exitFailure
          params <- case parseChatParams extraArgsText of
            Left err -> do
              putTextLn err
              exitFailure
            Right p -> pure p
          putTextLn $ "Using model: " <> resolveModelId provider modelOverride
          result <- runStructuredOutput provider apiKey modelOverride params JSONObject
          case result of
            Left err -> do
              putTextLn $ "Request failed: " <> err
              exitFailure
            Right _ -> pure ()
    "structured-schema" -> do
      case extractExtraArgs runnerArgs of
        Left err -> do
          putTextLn err
          exitFailure
        Right (extraArgsText, restArgs) -> do
          unless (null restArgs) $ do
            putTextLn "Structured runner does not accept positional args."
            exitFailure
          params <- case parseChatParams extraArgsText of
            Left err -> do
              putTextLn err
              exitFailure
            Right p -> pure p
          putTextLn $ "Using model: " <> resolveModelId provider modelOverride
          result <- runStructuredOutput provider apiKey modelOverride params JSONSchema
          case result of
            Left err -> do
              putTextLn $ "Request failed: " <> err
              exitFailure
            Right _ -> pure ()
    "tool-calling" -> do
      case extractExtraArgs runnerArgs of
        Left err -> do
          putTextLn err
          exitFailure
        Right (extraArgsText, restArgs) -> do
          unless (null restArgs) $ do
            putTextLn "Tool calling runner does not accept positional args."
            exitFailure
          params <- case parseChatParams extraArgsText of
            Left err -> do
              putTextLn err
              exitFailure
            Right p -> pure p
          putTextLn $ "Using model: " <> resolveModelId provider modelOverride
          result <- runToolCalling provider apiKey modelOverride params
          case result of
            Left err -> do
              putTextLn $ "Request failed: " <> err
              exitFailure
            Right _ -> pure ()
    "embeddings" -> do
      case extractExtraArgs runnerArgs of
        Left err -> do
          putTextLn err
          exitFailure
        Right (extraArgsText, restArgs) -> do
          params <- case parseEmbeddingParams extraArgsText of
            Left err -> do
              putTextLn err
              exitFailure
            Right p -> pure p
          let inputText = T.unwords (map toText restArgs)
          if T.null (T.strip inputText)
            then do
              putTextLn "Embeddings runner requires input text."
              exitFailure
            else do
              putTextLn $ "Using model: " <> resolveModelId provider modelOverride
              result <- runEmbeddings provider apiKey modelOverride params inputText
              case result of
                Left err -> do
                  putTextLn $ "Request failed: " <> err
                  exitFailure
                Right EmbeddingResult{requestJson, responseJson, response} -> do
                  putTextLn "--- Request JSON ---"
                  putTextLn requestJson
                  putTextLn "--- Response JSON ---"
                  putTextLn responseJson
                  when (isNothing response) $
                    putTextLn "Warning: response JSON could not be decoded."
    "hooks-demo" -> do
      case extractExtraArgs runnerArgs of
        Left err -> do
          putTextLn err
          exitFailure
        Right (extraArgsText, restArgs) -> do
          params <- case parseChatParams extraArgsText of
            Left err -> do
              putTextLn err
              exitFailure
            Right p -> pure p
          let prompt = T.unwords (map toText restArgs)
          if T.null (T.strip prompt)
            then do
              putTextLn "Hooks demo runner requires a prompt."
              exitFailure
            else do
              putTextLn $ "Using model: " <> resolveModelId provider modelOverride
              result <- runHooksDemo provider apiKey modelOverride params prompt
              case result of
                Left err -> do
                  putTextLn $ "Request failed: " <> err
                  exitFailure
                Right _ -> pure ()
    _ -> do
      putTextLn $ "Unknown runner: " <> runnerName
      putTextLn "Runners: basic, chat, models, structured-json, structured-schema, tool-calling"
      exitFailure
