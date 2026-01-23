# Project Strategy: Oasis

This document tracks the project goals, technical specifications, and critical information for coding agents working on the Oasis project.

## Core Motivation
To leverage Haskell's powerful type system for building a reliable, unified LLM abstraction layer, migrating from an existing Python implementation (`ref/` directory).

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
-   **JSON**: `aeson`
-   **Networking**: `http-conduit` & `http-client-tls`
-   **Streaming**: `conduit` (for composable streaming operations)
-   **Concurrency**: `async`, `stm`

## Non-Functional Requirements & Best Practices

1.  **Haskell Idiomatic Design**: Fully embrace the type system (ADTs, GADTs if necessary, Type Classes) and pure functions. Prefer `ReaderT` patterns or effect systems (like `polysemy` or simple `mtl`) over global state.
2.  **Observability Ready**: Architect the request pipeline to allow for easy insertion of logging, tracing, and metric collection for debugging and profiling.
3.  **Context Middleware**: Design the `Model` abstraction to support interceptors that can monitor or modify the message context (history) before it reaches the provider.
4.  **Verifiable Iterations**: Each development step must produce a human-verifiable result (e.g., a CLI output or a passing test suite).
5.  **TUI Concurrency**: Use `BChan` + `forkIO` to keep the UI responsive while running LLM requests; UI updates must be delivered via async events.

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

- **Problem**: Vty’s built-in width table disagrees with terminal rendering for many emoji, causing border gaps. ZWJ sequences remain problematic, but most non-ZWJ emoji can be corrected.
- **Solution**: Generate a terminal-specific width table using the Vty width-table tool and load it via Vty user config at startup.
    - Tool entry point: [tools/VtyWidthTable/Main.hs](tools/VtyWidthTable/Main.hs) (invokes `defaultMain` with terminal cursor measurement).
    - Run: `stack run vty-width-table -- -u` (writes the table and updates `~/.vty/config` with a `widthMap`).
    - The tool reports the output path (e.g., `~/.vty/width_table_<TERM>.dat`).
- **Load at runtime**: TUI startup uses Vty user config to ensure widthMap is applied. See [tui/Main.hs](tui/Main.hs).
- **Notes**: Custom width table is loaded once per process by Vty; restart the TUI after generating a new table.

## OpenAI-Compatible Client Interface Design

The `Oasis.Client.OpenAI` module should expose a layered, reusable interface:

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

## Layered Architecture (L1-L4)

**L1: Protocol (OpenAI-compatible core)**
- `Oasis.Client.OpenAI` for request/response/SSE parsing.
- `Oasis.Types` for protocol-level types.
- `Oasis.Config` for provider config parsing and defaults.

**L2: Session & Prompt Engineering**
- `Oasis.Chat.History` for editable conversation history.
- `Oasis.Chat.Prompt` for prompt composition/templating (future).
- `Oasis.Chat.Log` for session logging (future).

**L3: Runners (LLM capability modules)**
- `Oasis.Runner.Chat` (multi-turn + streaming toggle).
- `Oasis.Runner.Basic` (single-turn, non-streaming; full JSON input/output for low-level verification).
- `Oasis.Runner.Structured`, `Oasis.Runner.ToolCalling`, etc.

**L4: CLI Entry**
- Menu + selection + rendering only; no protocol or session logic.

## Build & Verification

Each iteration must pass the following baseline checks before moving forward:

1. **Type Check (HLS diagnostics)**
    - `haskell-language-server-wrapper typecheck .`

2. **Build**
    - `stack build`
