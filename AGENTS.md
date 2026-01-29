# Project Strategy: Oasis

This document tracks the project goals, technical specifications, and critical information for coding agents working on the Oasis project.

## Core Motivation

To leverage Haskell's powerful type system for building a reliable, unified LLM abstraction layer, migrating from an existing Python implementation (`ref/` directory).

## Project Structure

```
.
├── src/                    # Core library code
│   └── Oasis/
│       ├── Types.hs        # Protocol-level types
│       ├── Config.hs       # TOML configuration parsing
│       ├── Model.hs        # Provider/model resolution
│       ├── Chat/           # Chat session support
│       │   ├── Message.hs  # Message constructors
│       │   ├── History.hs  # Conversation history
│       │   └── Prompt.hs   # Prompt composition
│       ├── Client/         # OpenAI-compatible client
│       │   └── OpenAI/
│       ├── Demo/           # Usage examples
│       ├── Runner/         # LLM capability modules
│       ├── Output/         # Output formatting
│       └── Service/        # External service integrations
├── cli/                    # CLI application
├── tui/                    # TUI application
├── tools/                  # Development tools
│   └── VtyWidthTable/      # Vty emoji width table generator
├── package.yaml            # Package dependencies
├── stack.yaml              # Stack configuration
└── providers.toml          # Provider configurations
```

## Project Goals (Priority Order)

1.  **Configuration-Driven Provider Switching**:
    - Implement a Model Abstraction Layer (MAL) that resolves providers and models based on `providers.toml`.
    - Support environment-based API key management.
2.  **Robust Chat Interface**:
    - Support multi-turn conversations.
    - Implement both streaming and non-streaming completion modes.
    - Handle "Reasoning" tokens for models like DeepSeek-R1.
3.  **Pluggable Test Runners**:
    - Port the "Runner" architecture allowing different testing scenarios (Chat, Structured Output, Tool Calling).
4.  **OpenAI-Compatible Generic Client**:
    - Develop a high-quality, reusable OpenAI-compatible HTTP client in Haskell that can target different base URLs.

## Technical Specifications

-   **GHC Version**: 9.10.3
-   **Build Tool**: Stack 3.9.1 (Standard for this project)
-   **Prelude**: `relude` (Modern, safe alternative to base Prelude)
-   **Configuration**: `toml-parser`
-   **JSON**: `aeson`, `aeson-pretty`
-   **Networking**: `http-conduit` & `http-client-tls`
-   **Streaming**: `conduit` (for composable streaming operations)
-   **Concurrency**: `async`, `stm`
-   **TUI Framework**: `brick`, `brick-panes`, `brick-skylighting`, `vty`, `vty-crossplatform`
-   **Utilities**: `microlens`, `skylighting`, `ansi-terminal`, `case-insensitive`

## Non-Functional Requirements & Best Practices

1.  **Haskell Idiomatic Design**: Fully embrace the type system (ADTs, GADTs if necessary, Type Classes) and pure functions. Prefer `ReaderT` patterns or effect systems (like `polysemy` or simple `mtl`) over global state.
2.  **Observability Ready**: Architect the request pipeline to allow for easy insertion of logging, tracing, and metric collection for debugging and profiling.
3.  **Context Middleware**: Design the `Model` abstraction to support interceptors that can monitor or modify the message context (history) before it reaches the provider.
4.  **Verifiable Iterations**: Each development step must produce a human-verifiable result (e.g., a CLI output or a passing test suite).
5.  **TUI Concurrency**: Use `BChan` + `forkIO` to keep the UI responsive while running LLM requests; UI updates must be delivered via async events.

## Layered Architecture (L1-L4)

### L1: Protocol (OpenAI-compatible core)

**Public API**:

-   `Oasis.Types`: protocol-level types (messages, tools, usage, errors).
-   `Oasis.Config`: TOML configuration parsing and defaults.
-   `Oasis.Model`: provider/model resolution and related utilities.
-   `Oasis.Client.OpenAI.Types`: OpenAI-compatible protocol types and helpers.
-   `Oasis.Client.OpenAI`: main entry point, OpenAI-compatible API request and response handling.
-   `Oasis.Client.OpenAI.Param`: request parameter helpers.
-   `Oasis.Client.OpenAI.Hooks`: request hooks (`RequestHook`) and transport hooks (`ClientHooks`).

**Internal Modules** (may change without notice):

-   `Oasis.Client.OpenAI.Http`
-   `Oasis.Client.OpenAI.Request`
-   `Oasis.Client.OpenAI.Stream`
-   `Oasis.Client.OpenAI.Context`

### L2: Session & Prompt Engineering

-   `Oasis.Chat.Message`: message constructors and helpers.
-   `Oasis.Chat.History`: conversation history and editing.
-   `Oasis.Chat.Prompt`: prompt composition/templating.

### L3: Runners (LLM capability modules)

All runners are implemented and available in both CLI and TUI:

-   `Oasis.Runner.GetModels`: fetch available models from provider.
-   `Oasis.Runner.Basic`: single-turn, non-streaming, JSON-in/JSON-out.
-   `Oasis.Runner.Chat`: multi-turn chat with streaming toggle.
-   `Oasis.Runner.Embeddings`: embeddings generation.
-   `Oasis.Runner.StructuredOutput`: structured JSON or JSON schema output.
-   `Oasis.Runner.PartialMode`: partial response completion.
-   `Oasis.Runner.PrefixCompletion`: prefix-based completion.
-   `Oasis.Runner.FIMCompletion`: FIM (Fill-In-Middle) completion.
-   `Oasis.Runner.ToolCalling`: tool calling with execution.
-   `Oasis.Runner.Responses`: OpenAI Responses API.
-   `Oasis.Runner.Hooks`: request/response interception hooks.

**Internal Modules**:

-   `Oasis.Runner.Result`
-   `Oasis.Runner.Stream`

### L4: CLI/TUI Applications

Three executables are built:

-   `oasis-cli`: Command-line interface for quick experiments and reproducible JSON-based testing.
-   `oasis-tui`: Terminal user interface with interactive chat, multi-pane views, and streaming output.
-   `vty-width-table`: Development tool to generate terminal-specific emoji width tables.

## OpenAI-Compatible Client Interface Design

The `Oasis.Client.OpenAI` module exposes a layered, reusable interface:

1. **Core Types**
    - `ClientConfig`: base URL, API key, timeouts, proxies, default headers, user agent.
    - `ChatCompletionRequest` / `ChatCompletionResponse`: full OpenAI-compatible fields (model, messages, temperature, top_p, max_completion_tokens, stop, presence_penalty, frequency_penalty, seed, logit_bias, user, service_tier, reasoning_effort, stream, stream_options, tools, tool_choice, response_format).
    - `Message`, `Tool`, `ToolCall`, `Usage`, `ErrorResponse`.
    - `ChatCompletionStreamChunk` with `delta` content for streaming.

2. **Low-Level Request Construction**
    - `buildChatUrl`, `buildRequest`, `withAuth` for custom headers and instrumentation.

3. **Synchronous API**
    - `sendChatCompletion`, `sendEmbedding`, `sendModels`.

4. **Streaming API (SSE)**
    - `streamChatCompletion`: parse SSE lines and emit `delta` tokens incrementally.

5. **Convenience Helpers**
    - `simpleChat`, `chatWithSystem`, `chatWithParams` for common usage.

6. **Errors & Observability**
    - Structured `ClientError` (HTTP status, error code, raw body).
    - Hooks/middleware for logging and tracing.

## Interface Design Principles (Client Purity)

- **Protocol-only client**: `Oasis.Client.OpenAI` should only handle request/response encoding, HTTP, and SSE parsing.
- **No UI/printing in client**: formatting and display (CLI/GUI/Web) must live in Runner or application layers.
- **Stream as data**: streaming APIs should emit structured chunks/deltas, allowing callers to decide how to render or store them.
- **Composable handlers**: callers can route tokens to stdout, UI widgets, logs, or buffers without modifying the client.

## TUI Text Wrapping (Pane-Accurate, CJK-Safe)

- **Pane width must be resolved at render time**: use viewport size (not estimated terminal width). This is done by reading `vpSize` from the viewport in the render phase. See [tui/Oasis/Tui/Render/Markdown.hs](tui/Oasis/Tui/Render/Markdown.hs).
- **Custom wrap with terminal column widths**: wrap lines by accumulating display columns using Vty width functions (`safeWcwidth` / `safeWctwidth`) to correctly handle CJK and mixed text. See [tui/Oasis/Tui/Render/Markdown.hs](tui/Oasis/Tui/Render/Markdown.hs).
- **Preserve explicit newlines and blank lines**:
    - Split on `\n`, wrap each segment independently.
    - Render empty lines as a single space to ensure they occupy height (Brick treats `txt ""` as zero height).
    - Parse and retain consecutive blank lines as a separate block to avoid collapsing `\n\n`.
    See [tui/Oasis/Tui/Render/Markdown.hs](tui/Oasis/Tui/Render/Markdown.hs).

## Known Issues

- **Inline Markdown formatting deferred**: Inline bold/italic, inline code, links and code blocks can be detected but are not formatted properly due to rendering/layout complications. This remains unresolved.

## Emoji Width Handling (Vty Custom Width Table)

- **Problem**: Vty's built-in width table disagrees with terminal rendering for many emoji, causing border gaps. ZWJ sequences remain problematic, but most non-ZWJ emoji can be corrected.
- **Solution**: Generate a terminal-specific width table using the Vty width-table tool and load it via Vty user config at startup.
    - Tool entry point: [tools/VtyWidthTable/Main.hs](tools/VtyWidthTable/Main.hs) (invokes `defaultMain` with terminal cursor measurement).
    - Run: `stack run vty-width-table -- -u` (writes the table and updates `~/.vty/config` with a `widthMap`).
    - The tool reports the output path (e.g., `~/.vty/width_table_<TERM>.dat`).
- **Load at runtime**: TUI startup uses Vty user config to ensure widthMap is applied. See [tui/Main.hs](tui/Main.hs).
- **Notes**: Custom width table is loaded once per process by Vty; restart the TUI after generating a new table.

## Build & Verification

Each iteration must pass the following baseline checks before moving forward:

1. **Type Check (HLS diagnostics)**
    - `haskell-language-server-wrapper typecheck .`

2. **Build**
    - `stack build`

## Running Applications

### Build
```bash
stack build
```

### Run CLI
```bash
stack exec oasis-cli -- <provider> <model|default|-> <runner> [runner args...]
```

Example:
```bash
stack exec oasis-cli -- deepseek - basic "Hello, world!"
```

### Run TUI
```bash
stack exec oasis-tui
```

### Generate Emoji Width Table (Recommended before TUI)
```bash
stack run vty-width-table -- -u
```

## providers.toml Configuration

The configuration file defines providers and their settings:

```toml
description = "Large Language Model As A Service Providers"

[providers.deepseek]
description = "DeepSeek"
api_key_name = "DEEPSEEK_API_KEY"
base_url = "https://api.deepseek.com"
beta_base_url = "https://api.deepseek.com/beta"
chat_model_id = "deepseek-chat"
coder_model_id = "deepseek-chat"
reasoner_model_id = "deepseek-reasoner"

[defaults]
provider = "aliyun_qwen"
model_type = "chat"

[aliases]
qwen = "aliyun_qwen"
```

Each provider can specify:
- `api_key_name`: Environment variable name for the API key
- `base_url` / `beta_base_url`: API endpoints
- `chat_model_id`, `coder_model_id`, `reasoner_model_id`, `embedding_model_id`: Preset models
- `default_model_type`: Default model type for the provider

## CLI Runner Reference

Available runners in `oasis-cli`:

| Runner | Description |
|--------|-------------|
| `basic` | Single-turn, non-streaming |
| `chat` | Multi-turn chat (streaming by default) |
| `models` | Fetch available models from provider |
| `structured-json` / `structured-schema` | Structured output modes |
| `tool-calling` | Tool calling demo |
| `embeddings` | Generate embeddings |
| `hooks` | Request/response hook demo |
| `responses` | OpenAI Responses API |
| `partial-mode` | Partial response mode |
| `prefix-completion` | Prefix-based completion |
| `fim-completion` | FIM (Fill-In-Middle) completion |

Common CLI options:
- `--beta`: Enable beta features/APIs
- `--params <json>`: Pass OpenAI-compatible parameters (snake_case)
- `--extra-body <json>`: Merge extra JSON into request body
- `--enable-thinking`: Set `extra_body.enable_thinking = true`

## TUI Features

- **Provider/Model selection**: Choose from configured providers
- **Runner selection**: Test different LLM capabilities
- **Chat mode**: Multi-turn conversations with streaming
- **Message History editing** (`h`): Inspect and modify full message list
- **Debug Mode** (`d`): Preview and edit request JSON before sending
- **Chat Params** (`a`): Configure temperature, top_p, enable_thinking, extra_body

## Service Layer

- `Oasis.Service.Amap`: External service integration (used in tool calling runner for geocoding/reverse geocoding).
