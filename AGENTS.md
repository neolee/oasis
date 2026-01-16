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

## Build & Verification (Every Iteration)

Each iteration must pass the following baseline checks before moving forward:

1. **Build**
    - `stack build`

2. **Phase 1 Verification (Config Resolution)**
    - `stack exec oasis-cli -- deepseek`
    - Expected: prints the resolved `Provider` and API key presence status.

## Implementation Phases

### Phase 1: Infrastructure & MAL (Verifiable: Config Printing)
- Define core types for `Provider`, `Model`, `Message`, and `Completion`.
- Implement TOML parser for `providers.toml`.
- API Key management from environment variables.
- **Verification**: A small CLI tool that loads `providers.toml` and prints a resolved `Provider` object based on an alias.

### Phase 2: Non-Streaming Client (Verifiable: Non-Streaming Chat)
- Implement basic OpenAI-compatible HTTP client.
- Handle authentication and error responses.
- **Verification**: A runner that sends a single question to an LLM and prints the full JSON-decoded response.

### Phase 3: Streaming & Conduit (Verifiable: Real-time Output)
- Implement streaming support using `conduit`.
- Parse Server-Sent Events (SSE) into incremental chunks.
- **Verification**: A terminal output that prints tokens as they are received from the API.

### Phase 4: Interactive Chat & History (Verifiable: Multi-turn Chat)
- Implement `ChatRunner` with multi-turn conversation logic.
- Handle "Reasoning" tokens for models like DeepSeek-R1.
- **Verification**: An interactive terminal loop where the user can carry out a full conversation.

### Phase 5: Extensions & Interceptors
- Implement Structured Output and Tool Calling.
- Add logging and context modification interceptors.

## Agent Context & Instructions

-   **Source Material**: Reference implementation is in `/ref/`. 
-   **MAL Logic**: The Python `mal/` module provides a good reference for how providers are indexed and selected.
-   **Type Safety**: Prioritize strong types over raw strings (e.g., use an `Enum` or sum type for `ModelType` instead of raw strings like `"chat"`).
-   **Error Handling**: Use explicit error types rather than throwing exceptions where possible.
