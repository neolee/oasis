module Main where

import Relude
import Oasis.Config
import Oasis.Types
import Oasis.Runner.Basic
import Oasis.CLI.Chat (runChatInteractive)
import Oasis.CLI.ToolCalling (executeDemoToolCall)
import Oasis.Demo.StructuredOutput
  ( structuredMessages
  , jsonObjectFormat
  , jsonSchemaFormat
  )
import Oasis.Demo.ToolCalling
  ( toolCallingMessages
  , toolCallingTools
  )
import Oasis.Demo.Completions
  ( partialModeMessages
  , prefixCompletionMessages
  , fimCompletionRequest
  )
import Oasis.Runner.GetModels
import Oasis.Runner.StructuredOutput
import Oasis.Runner.Embeddings
import Oasis.Runner.Hooks
import Oasis.Runner.Responses
import Oasis.Runner.ToolCalling
import Oasis.Runner.PartialMode
import Oasis.Runner.PrefixCompletion
import Oasis.Runner.FIMCompletion
import Oasis.Model (resolveModelId, resolveEmbeddingModelId, selectBaseUrl)
import Oasis.Client.OpenAI.Param
  ( ChatParams(..)
  , parseChatParams
  , decodeExtraBodyValue
  , extraBodyFromEnableThinking
  , mergeExtraBodyList
  )
import Oasis.CLI.Render.Text
  ( renderSectionsText
  , requestSections
  , responseSections
  , textSection
  , jsonSection
  , warningSection
  )
import Oasis.Output.Common (extractAssistantContent, extractResponsesAssistantContent)
import qualified Oasis.Output.Types as Output
import Oasis.Output.Types (OutputSectionKind(..))
import Oasis.Client.OpenAI
  ( buildChatUrl
  , buildModelsUrl
  , buildEmbeddingsUrl
  , buildResponsesUrl
  , buildCompletionsUrl
  )
import Oasis.Client.OpenAI.Types
  ( CompletionResponse(..)
  , CompletionChoice(..)
  , EmbeddingResponse(..)
  , EmbeddingData(..)
  , ResponsesResponse(..)
  )
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (eitherDecode)
import qualified Data.Aeson as Aeson
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
      putTextLn "Common options: [--beta] [--extra-args <json>] [--extra-body <json>] [--enable-thinking]"
      putTextLn "Runners: basic, chat, models, structured-json, structured-schema, tool-calling, embeddings, hooks, responses, partial-mode, prefix-completion, fim-completion"
      putTextLn ""
      putTextLn "Runner specific args:"
      putTextLn "  basic [--raw <json>] <prompt...>"
      putTextLn "  chat [--no-stream] [--hide-thinking] [initial prompt...]"
      putTextLn "  structured-json / structured-schema"
      putTextLn "  tool-calling"
      putTextLn "  embeddings <text...>"
      putTextLn "  hooks <prompt...>"
      putTextLn "  responses <input...>"
      putTextLn "  partial-mode"
      putTextLn "  prefix-completion"
      putTextLn "  fim-completion"
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

extractBetaFlag :: [String] -> (Bool, [String])
extractBetaFlag = go False []
  where
    go found acc = \case
      [] -> (found, reverse acc)
      (x:xs)
        | x == "--beta" -> go True acc xs
        | otherwise -> go found (x:acc) xs

extractExtraBodyArgs :: [String] -> Either Text (Maybe Aeson.Value, [String])
extractExtraBodyArgs = go [] []
  where
    enableThinkingValue = maybe [] pure (extraBodyFromEnableThinking True)
    go extras acc = \case
      [] -> Right (mergeExtraBodyList extras, reverse acc)
      (x:xs)
        | x == "--enable-thinking" -> go (extras <> enableThinkingValue) acc xs
        | x == "--extra-body" ->
            case xs of
              [] -> Left "Missing value for --extra-body"
              (v:rest) ->
                case decodeExtraBodyValue (toText v) of
                  Left err -> Left err
                  Right val -> go (extras <> [val]) acc rest
        | "--extra-body=" `L.isPrefixOf` x ->
            let prefix = "--extra-body=" :: String
                v = drop (length prefix) x
            in case decodeExtraBodyValue (toText v) of
              Left err -> Left err
              Right val -> go (extras <> [val]) acc xs
        | otherwise -> go extras (x:acc) xs

applyExtraBodyToParams :: Maybe Aeson.Value -> ChatParams -> ChatParams
applyExtraBodyToParams extra params =
  let merged = mergeExtraBodyList (catMaybes [paramExtraBody params, extra])
  in params { paramExtraBody = merged }

dispatchRunner :: Text -> Provider -> Text -> Maybe Text -> Text -> [String] -> IO ()
dispatchRunner alias provider apiKey modelOverride runnerName runnerArgs =
  let (useBeta, runnerArgs') = extractBetaFlag runnerArgs
  in
  case runnerName of
    "basic" -> do
      putTextLn $ "Loading config for alias: " <> alias
      putTextLn "--- Resolved Provider ---"
      print provider
      if apiKey /= ""
        then putTextLn "API Key: Found (hidden)"
        else putTextLn "API Key: NOT FOUND (Check environment variables)"
      case extractExtraArgs runnerArgs' of
        Left err -> do
          putTextLn err
          exitFailure
        Right (extraArgsText, restArgs) -> do
          params0 <- case parseChatParams extraArgsText of
            Left err -> do
              putTextLn err
              exitFailure
            Right p -> pure p
          case extractExtraBodyArgs restArgs of
            Left err -> do
              putTextLn err
              exitFailure
            Right (extraBody, restArgs1) -> do
              let params = applyExtraBodyToParams extraBody params0
              case extractRawArgs restArgs1 of
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
                      result <- runBasicRaw provider apiKey modelOverride params messages useBeta
                      case result of
                        Left err -> do
                          putTextLn $ "Request failed: " <> err
                          exitFailure
                        Right result -> do
                          let baseUrl = selectBaseUrl provider useBeta
                              ctx = Output.RequestContext (buildChatUrl baseUrl) (requestJson result)
                              assistantSection = case response result >>= extractAssistantContent of
                                Nothing -> []
                                Just content -> [textSection SectionAssistant "Assistant" content]
                              sections = requestSections ctx <> responseSections result <> assistantSection
                          putTextLn (renderSectionsText sections)
                    Nothing -> do
                      let prompt = T.unwords (map toText restArgs2)
                      if T.null (T.strip prompt)
                        then do
                          putTextLn "Basic runner requires a prompt."
                          exitFailure
                        else do
                          putTextLn $ "Using model: " <> resolveModelId provider modelOverride
                          result <- runBasic provider apiKey modelOverride params prompt useBeta
                          case result of
                            Left err -> do
                              putTextLn $ "Request failed: " <> err
                              exitFailure
                            Right result -> do
                              let baseUrl = selectBaseUrl provider useBeta
                                  ctx = Output.RequestContext (buildChatUrl baseUrl) (requestJson result)
                                  assistantSection = case response result >>= extractAssistantContent of
                                    Nothing -> []
                                    Just content -> [textSection SectionAssistant "Assistant" content]
                                  sections = requestSections ctx <> responseSections result <> assistantSection
                              putTextLn (renderSectionsText sections)
    "chat" -> do
      case extractExtraArgs runnerArgs' of
        Left err -> do
          putTextLn err
          exitFailure
        Right (extraArgsText, restArgs) -> do
          params0 <- case parseChatParams extraArgsText of
            Left err -> do
              putTextLn err
              exitFailure
            Right p -> pure p
          case extractExtraBodyArgs restArgs of
            Left err -> do
              putTextLn err
              exitFailure
            Right (extraBody, restArgs1) -> do
              let params = applyExtraBodyToParams extraBody params0
                  (flags, rest) = L.partition (L.isPrefixOf "--") restArgs1
                  useStream = "--no-stream" `notElem` flags
                  showThinking = "--hide-thinking" `notElem` flags
                  initialPrompt = if null rest then Nothing else Just (T.unwords (map toText rest))
              putTextLn $ "Using model: " <> resolveModelId provider modelOverride
              result <- runChatInteractive provider apiKey modelOverride params useStream showThinking useBeta initialPrompt
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
      result <- runGetModels provider apiKey useBeta
      case result of
        Left err -> do
          putTextLn $ "Request failed: " <> err
          exitFailure
        Right result -> do
          let baseUrl = selectBaseUrl provider useBeta
              ctx = Output.RequestContext (buildModelsUrl baseUrl) (requestJson result)
              sections = requestSections ctx <> responseSections result
          putTextLn (renderSectionsText sections)
    "structured-json" -> do
      case extractExtraArgs runnerArgs' of
        Left err -> do
          putTextLn err
          exitFailure
        Right (extraArgsText, restArgs) -> do
          params0 <- case parseChatParams extraArgsText of
            Left err -> do
              putTextLn err
              exitFailure
            Right p -> pure p
          case extractExtraBodyArgs restArgs of
            Left err -> do
              putTextLn err
              exitFailure
            Right (extraBody, restArgs1) -> do
              unless (null restArgs1) $ do
                putTextLn "Structured runner does not accept positional args."
                exitFailure
              let params = applyExtraBodyToParams extraBody params0
              putTextLn $ "Using model: " <> resolveModelId provider modelOverride
              let messages = structuredMessages
                  responseFormat = jsonObjectFormat
              result <- runStructuredOutputDetailed provider apiKey modelOverride params messages responseFormat useBeta
              case result of
                Left err -> do
                  putTextLn $ "Request failed: " <> err
                  exitFailure
                Right structured -> do
                  let rawSection = textSection SectionResponse "Raw Stream" (rawText structured)
                      parsedSection = case parsedJson structured of
                        Left perr -> textSection SectionError "Parsed JSON" ("Invalid JSON: " <> perr)
                        Right pretty -> jsonSection SectionResponse "Parsed JSON" pretty
                  putTextLn (renderSectionsText [rawSection, parsedSection])
    "structured-schema" -> do
      case extractExtraArgs runnerArgs' of
        Left err -> do
          putTextLn err
          exitFailure
        Right (extraArgsText, restArgs) -> do
          params0 <- case parseChatParams extraArgsText of
            Left err -> do
              putTextLn err
              exitFailure
            Right p -> pure p
          case extractExtraBodyArgs restArgs of
            Left err -> do
              putTextLn err
              exitFailure
            Right (extraBody, restArgs1) -> do
              unless (null restArgs1) $ do
                putTextLn "Structured runner does not accept positional args."
                exitFailure
              let params = applyExtraBodyToParams extraBody params0
              putTextLn $ "Using model: " <> resolveModelId provider modelOverride
              let messages = structuredMessages
                  responseFormat = jsonSchemaFormat
              result <- runStructuredOutputDetailed provider apiKey modelOverride params messages responseFormat useBeta
              case result of
                Left err -> do
                  putTextLn $ "Request failed: " <> err
                  exitFailure
                Right structured -> do
                  let rawSection = textSection SectionResponse "Raw Stream" (rawText structured)
                      parsedSection = case parsedJson structured of
                        Left perr -> textSection SectionError "Parsed JSON" ("Invalid JSON: " <> perr)
                        Right pretty -> jsonSection SectionResponse "Parsed JSON" pretty
                  putTextLn (renderSectionsText [rawSection, parsedSection])
    "tool-calling" -> do
      case extractExtraArgs runnerArgs' of
        Left err -> do
          putTextLn err
          exitFailure
        Right (extraArgsText, restArgs) -> do
          params0 <- case parseChatParams extraArgsText of
            Left err -> do
              putTextLn err
              exitFailure
            Right p -> pure p
          case extractExtraBodyArgs restArgs of
            Left err -> do
              putTextLn err
              exitFailure
            Right (extraBody, restArgs1) -> do
              unless (null restArgs1) $ do
                putTextLn "Tool calling runner does not accept positional args."
                exitFailure
              let params = applyExtraBodyToParams extraBody params0
              putTextLn $ "Using model: " <> resolveModelId provider modelOverride
              let input = ToolCallingInput
                    { toolMessages = toolCallingMessages
                    , toolDefs = toolCallingTools
                    , toolParallelCalls = Just True
                    }
              result <- runToolCallingDetailed provider apiKey modelOverride params input executeDemoToolCall useBeta
              case result of
                Left err -> do
                  let sections =
                        [ textSection SectionResponse "First Response" ("Error: " <> err)
                        , textSection SectionToolResult "Tool Result" "Skipped due to request error."
                        , textSection SectionAssistant "Final Assistant" "Skipped due to request error."
                        ]
                  putTextLn (renderSectionsText sections)
                Right outcome -> do
                  let sections = case outcome of
                        ToolCallingNoToolCall content ->
                          [ textSection SectionResponse "First Response" content
                          , textSection SectionToolResult "Tool Result" "No tool call returned."
                          , textSection SectionAssistant "Final Assistant" "Skipped because no tool call was returned."
                          ]
                        ToolCallingToolError toolCallJson terr ->
                          [ jsonSection SectionResponse "First Response (Tool Call)" toolCallJson
                          , textSection SectionToolResult "Tool Result" ("Error: " <> terr)
                          , textSection SectionAssistant "Final Assistant" "Skipped due to tool error."
                          ]
                        ToolCallingSecondError toolCallJson toolResult err ->
                          [ jsonSection SectionResponse "First Response (Tool Call)" toolCallJson
                          , textSection SectionToolResult "Tool Result" toolResult
                          , textSection SectionAssistant "Final Assistant" ("Error: " <> err)
                          ]
                        ToolCallingSuccess toolCallJson toolResult finalText ->
                          [ jsonSection SectionResponse "First Response (Tool Call)" toolCallJson
                          , textSection SectionToolResult "Tool Result" toolResult
                          , textSection SectionAssistant "Final Assistant" finalText
                          ]
                  putTextLn (renderSectionsText sections)
    "embeddings" -> do
      case extractExtraArgs runnerArgs' of
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
              putTextLn $ "Using model: " <> resolveEmbeddingModelId provider modelOverride
              result <- runEmbeddings provider apiKey modelOverride params inputText useBeta
              case result of
                Left err -> do
                  putTextLn $ "Request failed: " <> err
                  exitFailure
                Right result -> do
                  let baseUrl = selectBaseUrl provider useBeta
                      ctx = Output.RequestContext (buildEmbeddingsUrl baseUrl) (requestJson result)
                      summarySection = case response result of
                        Nothing -> []
                        Just EmbeddingResponse{data_} ->
                          let count = length data_
                              firstEmbedding = listToMaybe data_ >>= \EmbeddingData{embedding} -> Just embedding
                              dim = maybe 0 length firstEmbedding
                              preview = maybe "[]" (formatPreview 6) firstEmbedding
                              summary = T.unlines
                                [ "Vectors: " <> show count
                                , "Dimensions: " <> show dim
                                , "Head: " <> preview
                                ]
                          in [textSection SectionResponse "Summary" summary]
                      sections = requestSections ctx <> responseSections result <> summarySection
                  putTextLn (renderSectionsText sections)
    "hooks" -> do
      case extractExtraArgs runnerArgs' of
        Left err -> do
          putTextLn err
          exitFailure
        Right (extraArgsText, restArgs) -> do
          params0 <- case parseChatParams extraArgsText of
            Left err -> do
              putTextLn err
              exitFailure
            Right p -> pure p
          case extractExtraBodyArgs restArgs of
            Left err -> do
              putTextLn err
              exitFailure
            Right (extraBody, restArgs1) -> do
              let params = applyExtraBodyToParams extraBody params0
                  prompt = T.unwords (map toText restArgs1)
              if T.null (T.strip prompt)
                then do
                  putTextLn "Hooks runner requires a prompt."
                  exitFailure
                else do
                  putTextLn $ "Using model: " <> resolveModelId provider modelOverride
                  result <- runHooksDetailed provider apiKey modelOverride params prompt useBeta
                  case result of
                    Left err -> do
                      putTextLn $ "Request failed: " <> err
                      exitFailure
                    Right HooksResult{hookLogText, responseJsonText, requestJsonText} -> do
                      let baseUrl = selectBaseUrl provider useBeta
                          ctx = Output.RequestContext (buildChatUrl baseUrl) requestJsonText
                          sections =
                            requestSections ctx
                            <> [ textSection SectionLog "Hook Log" hookLogText
                               , textSection SectionResponse "Response JSON (truncated)" responseJsonText
                               ]
                      putTextLn (renderSectionsText sections)
    "responses" -> do
      case extractExtraArgs runnerArgs' of
        Left err -> do
          putTextLn err
          exitFailure
        Right (extraArgsText, restArgs) -> do
          params <- case parseResponsesParams extraArgsText of
            Left err -> do
              putTextLn err
              exitFailure
            Right p -> pure p
          let inputText = T.unwords (map toText restArgs)
          if T.null (T.strip inputText)
            then do
              putTextLn "Responses runner requires input text."
              exitFailure
            else do
              putTextLn $ "Using model: " <> resolveModelId provider modelOverride
              result <- runResponses provider apiKey modelOverride params inputText useBeta
              case result of
                Left err -> do
                  putTextLn $ "Request failed: " <> err
                  exitFailure
                Right result -> do
                  let baseUrl = selectBaseUrl provider useBeta
                      ctx = Output.RequestContext (buildResponsesUrl baseUrl) (requestJson result)
                      assistantSection = case response result >>= extractResponsesAssistantContent of
                        Nothing -> []
                        Just content -> [textSection SectionAssistant "Assistant" content]
                      sections = requestSections ctx <> responseSections result <> assistantSection
                  putTextLn (renderSectionsText sections)
    "partial-mode" -> do
      case extractExtraArgs runnerArgs' of
        Left err -> do
          putTextLn err
          exitFailure
        Right (extraArgsText, restArgs) -> do
          params0 <- case parseChatParams extraArgsText of
            Left err -> do
              putTextLn err
              exitFailure
            Right p -> pure p
          case extractExtraBodyArgs restArgs of
            Left err -> do
              putTextLn err
              exitFailure
            Right (extraBody, restArgs1) -> do
              unless (null restArgs1) $ do
                putTextLn "Partial mode runner does not accept positional args."
                exitFailure
              let params = applyExtraBodyToParams extraBody params0
              putTextLn $ "Using model: " <> resolveModelId provider modelOverride
              let messages = partialModeMessages
              result <- runPartialModeDetailed provider apiKey modelOverride params messages useBeta
              case result of
                Left err -> do
                  putTextLn $ "Request failed: " <> err
                  exitFailure
                Right result -> do
                  let baseUrl = selectBaseUrl provider useBeta
                      ctx = Output.RequestContext (buildChatUrl baseUrl) (requestJson result)
                      assistantSection = case response result >>= extractAssistantContent of
                        Nothing -> []
                        Just content -> [textSection SectionAssistant "Assistant" content]
                      sections = requestSections ctx <> responseSections result <> assistantSection
                  putTextLn (renderSectionsText sections)
    "prefix-completion" -> do
      case extractExtraArgs runnerArgs' of
        Left err -> do
          putTextLn err
          exitFailure
        Right (extraArgsText, restArgs) -> do
          params0 <- case parseChatParams extraArgsText of
            Left err -> do
              putTextLn err
              exitFailure
            Right p -> pure p
          case extractExtraBodyArgs restArgs of
            Left err -> do
              putTextLn err
              exitFailure
            Right (extraBody, restArgs1) -> do
              unless (null restArgs1) $ do
                putTextLn "Prefix completion runner does not accept positional args."
                exitFailure
              let params = applyExtraBodyToParams extraBody params0
              putTextLn $ "Using model: " <> resolveModelId provider modelOverride
              let messages = prefixCompletionMessages
              result <- runPrefixCompletionDetailed provider apiKey modelOverride params messages useBeta
              case result of
                Left err -> do
                  putTextLn $ "Request failed: " <> err
                  exitFailure
                Right result -> do
                  let baseUrl = selectBaseUrl provider useBeta
                      ctx = Output.RequestContext (buildChatUrl baseUrl) (requestJson result)
                      assistantSection = case response result >>= extractAssistantContent of
                        Nothing -> []
                        Just content -> [textSection SectionAssistant "Assistant" content]
                      sections = requestSections ctx <> responseSections result <> assistantSection
                  putTextLn (renderSectionsText sections)
    "fim-completion" -> do
      putTextLn $ "Using model: " <> resolveModelId provider modelOverride
      result <- runFIMCompletionDetailed provider apiKey modelOverride fimCompletionRequest useBeta
      case result of
        Left err -> do
          putTextLn $ "Request failed: " <> err
          exitFailure
        Right result -> do
          let baseUrl = selectBaseUrl provider useBeta
              ctx = Output.RequestContext (buildCompletionsUrl baseUrl) (requestJson result)
              completionSection = case response result of
                Just CompletionResponse{choices = (CompletionChoice{text}:_)} ->
                  [textSection SectionCompletion "Completion" text]
                _ -> [warningSection "Completion" "No completion choices returned."]
              sections = requestSections ctx <> responseSections result <> completionSection
          putTextLn (renderSectionsText sections)
    _ -> do
      putTextLn $ "Unknown runner: " <> runnerName
      putTextLn "Runners: basic, chat, models, structured-json, structured-schema, tool-calling, embeddings, hooks, responses, partial-mode, prefix-completion, fim-completion"
      exitFailure

formatPreview :: Int -> [Double] -> Text
formatPreview n xs =
  let items = map (T.pack . show) (take n xs)
  in "[" <> T.intercalate ", " items <> if length xs > n then ", ...]" else "]"
