# Oasis: Haskell LLM Integration Library

Oasis is a Haskell library and set of apps that provide a unified interface for different LLM providers. It focuses on configuration-driven provider selection, unified type-safe request and response handling, and advanced context management with hook mechanisms.

---

## The Library

The library (`./src`) is layered to keep protocol logic pure and keep UI/printing in apps.

### Public API

**Core**

- `Oasis.Types`: protocol-level types (messages, tools, usage, errors).
- `Oasis.Config`: TOML configuration parsing and defaults.
- `Oasis.Model`: provider/model resolution and related utilities.

**OpenAI-Compatible Client**

- `Oasis.Client.OpenAI.Types`: OpenAI-compatible protocol types and helpers.
- `Oasis.Client.OpenAI`: main entry point, OpenAI-compatible API request and response handling.
- `Oasis.Client.OpenAI.Param`: request parameter helpers.
- `Oasis.Client.OpenAI.Hooks`: request hooks (`RequestHook`) and transport hooks (`ClientHooks`).

**Chat Session Support**

- `Oasis.Chat.Message`: message constructors and helpers.
- `Oasis.Chat.History`: conversation history and editing.

**Runners (LLM Capabilities)**

- `Oasis.Runner.GetModels`: fetch available models from provider.
- `Oasis.Runner.Basic`: single-turn, non-streaming, JSON-in/JSON-out.
- `Oasis.Runner.Chat`: multi-turn chat with streaming toggle.
- `Oasis.Runner.Embeddings`: embeddings.
- `Oasis.Runner.StructuredOutput`: structured JSON or JSON schema output.
- `Oasis.Runner.PartialMode`, `Oasis.Runner.PrefixCompletion`, `Oasis.Runner.FIMCompletion`: partial and prefix completion modes.
- `Oasis.Runner.ToolCalling`: tool calling.
- `Oasis.Runner.Responses`: OpenAI Responses API.
- `Oasis.Runner.Hooks`: request/response interception hooks.

**Service Layer**

- `Oasis.Service.Amap`: external service integration (used in tool calling runner).

**Demos**

- `Oasis.Demo.StructuredOutput`: structured output message/format examples.
- `Oasis.Demo.ToolCalling`: tool calling message/tool examples.
- `Oasis.Demo.Completions`: partial/prefix/FIM completion examples.

**Typical use**: load config, resolve provider/model, then call a client or runner using those settings.

### Usage Scenarios

#### 1. Resolve a provider by alias and list preset models

```hs
import Oasis.Config (findConfig, loadConfig, resolveProvider)
import Oasis.Types (Config(..), Provider(..))
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  -- Load config and resolve alias
  Just path <- findConfig
  Right cfg <- loadConfig path
  Just (provider, apiKey) <- resolveProvider cfg "deepseek"

  -- Available provider names (before alias expansion)
  let providerNames = M.keys (providers cfg)

  -- Preset model list (from providers.toml)
  let presetModels = [chat_model_id provider, coder_model_id provider, reasoner_model_id provider]
  pure ()
```

#### 2. Fetch available models (remote API)

```hs
import Oasis.Runner.GetModels (runGetModels)
import Oasis.Types (RequestResponse)
import Data.Aeson (Value)

result <- runGetModels provider apiKey False
-- result :: Either Text (RequestResponse Value)
```

Implementation references: [src/Oasis/Runner/GetModels.hs](src/Oasis/Runner/GetModels.hs) and CLI dispatch in [cli/Main.hs](cli/Main.hs).

#### 3. Build a request from prompt and call LLM (non-streaming)

```hs
import Oasis.Runner.Basic (runBasic)
import Oasis.Client.OpenAI.Param (emptyChatParams)

let modelOverride = Nothing
result <- runBasic provider apiKey modelOverride emptyChatParams "Hi, how are you?" False
```

#### 4. Same as above, but streaming (SSE)

```hs
import Oasis.Runner.Chat (streamChatOnce, ChatStreamEvent(..))
import Oasis.Chat.Message (userMessage)
import Oasis.Client.OpenAI.Param (emptyChatParams)

let messages = [userMessage "Explain hash tables in three points" ]
let onEvent ev = case ev of
  ChatThinking t -> putStrLn ("[thinking] " <> toString t)
  ChatAnswer t -> putStrLn (toString t)

let modelOverride = Nothing
_ <- streamChatOnce provider apiKey modelOverride emptyChatParams messages onEvent False
```

Full implementation: [src/Oasis/Runner/Chat.hs](src/Oasis/Runner/Chat.hs).

#### 5. Get embeddings from input text

```hs
import Oasis.Runner.Embeddings (runEmbeddings, emptyEmbeddingParams)

let modelOverride = "text-embedding-v3"
result <- runEmbeddings provider apiKey modelOverride emptyEmbeddingParams "Hello world"
```

#### 6. Structured output with a schema

```hs
import Oasis.Runner.StructuredOutput (runStructuredOutput)
import Oasis.Client.OpenAI.Param (emptyChatParams)
import Oasis.Chat.Message (systemMessage, userMessage)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson

let messages =
  [ systemMessage "Parse the input into JSON with fields question and answer"
  , userMessage "Which is the highest mountain in the world? Mount Everest."
  ]
let responseFormat = Aeson.object
  [ "type" .= ("json_schema" :: Text)
  , "json_schema" .= Aeson.object
      [ "name" .= ("qa" :: Text)
      , "schema" .= Aeson.object
      [ "type" .= ("object" :: Text)
      , "properties" .= Aeson.object
      [ "question" .= Aeson.object ["type" .= ("string" :: Text)]
      , "answer" .= Aeson.object ["type" .= ("string" :: Text)]
      ]
      , "required" .= (["question", "answer"] :: [Text])
      ]
      ]
  ]

let modelOverride = Nothing
result <- runStructuredOutput provider apiKey modelOverride emptyChatParams messages responseFormat False
```

Reference example: [src/Oasis/Demo/StructuredOutput.hs](src/Oasis/Demo/StructuredOutput.hs).

#### 7. Tool Calling

```hs
import Oasis.Runner.ToolCalling
import Oasis.Client.OpenAI.Param (emptyChatParams)
import Oasis.Demo.ToolCalling (toolCallingMessages, toolCallingTools)

let input = ToolCallingInput
  { toolMessages = toolCallingMessages
  , toolDefs = toolCallingTools
  , toolParallelCalls = Just False
  }
let executeToolCall tc = do
  -- Call your real tool here
  pure (Right "tool result")

let modelOverride = Nothing
result <- runToolCalling provider apiKey modelOverride emptyChatParams input executeToolCall False
```

Tool and message examples: [src/Oasis/Demo/ToolCalling.hs](src/Oasis/Demo/ToolCalling.hs).

#### 8. Partial mode / Prefix completion / FIM completion

```hs
import Oasis.Runner.PartialMode (runPartialMode)
import Oasis.Runner.PrefixCompletion (runPrefixCompletion)
import Oasis.Runner.FIMCompletion (runFIMCompletion)
import Oasis.Client.OpenAI.Param (emptyChatParams)
import Oasis.Demo.Completions (partialModeMessages, prefixCompletionMessages, fimCompletionRequest)

let modelOverride = Nothing
_ <- runPartialMode provider apiKey modelOverride emptyChatParams partialModeMessages False
_ <- runPrefixCompletion provider apiKey modelOverride emptyChatParams prefixCompletionMessages False
_ <- runFIMCompletion provider apiKey modelOverride fimCompletionRequest False
```

Reference examples: [src/Oasis/Demo/Completions.hs](src/Oasis/Demo/Completions.hs).

#### 9. Use hooks to intercept LLM requests (log request/response)

```hs
import Oasis.Runner.Hooks (runHooks, HooksResult(..))
import Oasis.Client.OpenAI.Param (emptyChatParams)

result <- runHooks provider apiKey modelOverride emptyChatParams "Hello hooks" False
case result of
  Left err -> putStrLn (toString err)
  Right HooksResult{..} -> do
    putStrLn ("--- log ---\n" <> toString hookLogText)
    putStrLn ("--- request ---\n" <> toString requestJsonText)
    putStrLn ("--- response ---\n" <> toString responseJsonText)
```

Reference implementation: [src/Oasis/Runner/Hooks.hs](src/Oasis/Runner/Hooks.hs).

### Internal Modules

Internal implementation modules that may change without notice:

- `Oasis.Client.OpenAI.Http`
- `Oasis.Client.OpenAI.Request`
- `Oasis.Client.OpenAI.Stream`
- `Oasis.Client.OpenAI.Context`
- `Oasis.Runner.Result`
- `Oasis.Runner.Stream`

---

## CLI Application

The CLI exposes the library via a single command with runner selection. It is designed for quick experiments and reproducible JSON-based testing.

### Usage

```
oasis-cli <provider> <model|default|-> <runner> [runner args...]
```

**Positional arguments**

- `provider`: provider alias in providers.toml (e.g., `deepseek`, `qwen`).
- `model|default|-`:
  - `default` or `-` means “use provider default.”
  - Any other value is used as the explicit model ID.
- `runner`: one of `basic`, `chat`, `models`, `structured-json`, `structured-schema`, `tool-calling`, `embeddings`, `hooks`, `responses`, `partial-mode`, `prefix-completion`, `fim-completion`.

**Common options**

- `--beta`: enable beta features/APIs.
- `--params <json>`: pass OpenAI-compatible parameters (snake_case).
- `--extra-body <json>`: merge an extra JSON object into the request body.
- `--enable-thinking`: convenience flag for `extra_body.enable_thinking = true`.

> The `--params` vs `--extra-body` options are different.
>
> - `--params` is parsed into structured parameters and mapped onto the request fields (e.g., `temperature`, `top_p`, `max_completion_tokens`, `stop`, etc.).
> - `--extra-body` is a raw JSON object that is merged into the final request body (useful for provider-specific or undocumented fields).
> - If both are provided, the final request uses `--params` for known fields, and `--extra-body` for extra fields; overlapping keys are resolved by the merge logic in `extra_body` (later inputs win).
>
> The following fields are supported in `--params`:
>
> - `temperature`
> - `top_p`
> - `max_completion_tokens`
> - `stop` (string or list of strings)
> - `presence_penalty`
> - `frequency_penalty`
> - `seed`
> - `logit_bias`
> - `user`
> - `service_tier`
> - `reasoning_effort`
> - `stream_options`
>

**Runner-specific notes**

- `basic [--raw <json>] <prompt...>`: single-turn, non-streaming. With `--raw`, input is a JSON `messages` array.
- `chat [--no-stream] [--hide-thinking] [initial prompt...]`: multi-turn chat. Default is streaming with thinking visible.
- `structured-json` / `structured-schema`: structured output modes.
- `tool-calling`: tool calling demo.
- `embeddings <text...>`: generate embeddings.
- `responses <input...>`: OpenAI Responses API with plain text input.

### Examples

Basic runner with params:

```
oasis-cli deepseek - basic --params '{"temperature":0.2,"top_p":0.9,"max_completion_tokens":64}' 你好
```

Basic runner with extra_body:

```
oasis-cli qwen - basic --extra-body '{"enable_thinking":true}' 你好
```

Basic runner with params and extra_body:

```
oasis-cli -- moonshot - basic 你好 --params '{"max_completion_tokens":64}' --extra-body '{"thinking": {"type": "disabled"}}'
```

Basic runner with raw messages:

```
oasis-cli qwen - basic --raw '[{"role":"user","content":[{"type":"text","text":"Explain siphoning in one sentence."}]}]'
```

Embeddings:

```
oasis-cli qwen text-embedding-v3 embeddings "Hello embeddings"
```

Hooks:

```
oasis-cli qwen - hooks "Hello"
```

Responses:

```
oasis-cli qwen - responses "Introduce yourself" --enable-thinking
```

---

## TUI Application

The TUI provides an interactive chat-focused UI with multi-pane views and streaming output. It is designed to keep the UI responsive during network requests.

### Core Usage

- **Provider and model selection**: choose a provider alias and model configured in providers.toml.
- **Runner selection**: choose a runner (basic, chat, tool calling, structured output, tool_calling etc) to run, check LLM's specific capabilities.
- **Chat client**: choose `chat` runner for multi-turn conversations, with streaming output by default.

### Message List Editing (Verbose History)

Press `h` to toggle Message History. This opens a message list editor that lets you inspect and modify the full message list before sending.

- **Focus list**: press `l`, then use ↑/↓ to move.
- **Append**: press `a` to append a new message.
- **Insert**: press `i` to insert before the current selection.
- **Edit**: press `e` to edit the selected message content.
- **Delete**: press `del` or `backspace` to remove the selected message.

This is the recommended way to edit system/user/assistant/tool messages precisely.

### Debug Mode (Request Preview)

Press `d` to toggle Debug Mode. When enabled, each request opens a debug dialog showing provider/model, endpoint, headers, and the JSON payload. You can:

- Edit the request body before sending.
- Submit or cancel the request.
- Restore the original payload or copy the current payload.

Use Debug Mode to verify request shape, test parameters, and reproduce issues.

### Chat Params

Press `a` in the main pane to open the Chat Params dialog. Besides temperature/top_p/stop, it includes:

- **enable_thinking**: toggles `extra_body.enable_thinking = true`.
- **extra_body (json)**: an object merged into the request body. It must be a JSON object.

If `extra_body` already contains `enable_thinking`, its value must match the checkbox; otherwise the dialog reports a conflict.

---

## Setup and Run Applications

### Prerequisites

- GHC 9.10.3
- Stack 3.9.1
- A configured providers.toml with API keys via environment variables

### Build

```
stack build
```

### Run CLI

```
stack exec oasis-cli -- <provider> <model|default|-> <runner> [runner args...]
```

### Run TUI

```
stack exec oasis-tui
```

### Emoji Width Fix for TUI (Recommended)

Some terminals render emoji with different widths than Vty’s default table. Use the width-table tool to generate a terminal-specific width map and load it via Vty’s user config:

```
stack run vty-width-table -- -u
```

This command:

- Measures emoji widths in your current terminal.
- Writes a width table to your Vty config directory.
- Updates `~/.vty/config` to point to the new width table.

After generating the table, **restart the TUI** to apply the new width map.
