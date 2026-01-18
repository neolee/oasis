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

-   **GHC Version**: 9.6.7
-   **Build Tool**: Stack 3.7.1 (Standard for this project)
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

## OpenAI-Compatible Client Interface Design

The `Oasis.Client.OpenAI` module should expose a layered, reusable interface:

1. **Core Types**
    - `ClientConfig`: base URL, API key, timeouts, proxies, default headers, user agent.
    - `ChatCompletionRequest` / `ChatCompletionResponse`: full OpenAI-compatible fields (model, messages, temperature, top_p, max_tokens, stop, stream, tools, tool_choice, response_format, seed, logprobs, metadata).
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
- `Oasis.Runner.Structured`, `Oasis.Runner.ToolCalling` (future).

**L4: CLI Entry**
- Menu + selection + rendering only; no protocol or session logic.

## Build & Verification

Each iteration must pass the following baseline checks before moving forward:

1. **Type Check (HLS diagnostics)**
    - `haskell-language-server-wrapper typecheck .`

2. **Build**
    - `stack build`
