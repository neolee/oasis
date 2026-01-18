# Oasis: Haskell LLM Integration Library

Oasis is a high-quality Haskell library providing a unified interface for multiple Large Language Model (LLM) providers, inspired by the goals of [LiteLLM](https://www.litellm.ai/).

## Purpose

The project aims to simplify the integration of various LLMs into Haskell applications by providing:
- **Unified API**: A consistent interface for Chat, Structured Output, and Tool Calling across different providers (OpenAI, DeepSeek, Anthropic, etc.).
- **Configuration-Driven**: Easy switching between providers and models via a simple TOML configuration.
- **Reliability**: Leveraging Haskell's robust type system to ensure correctness in LLM interactions and data handling.

## Key Features

- **Multi-Provider Support**: Seamlessly switch between official APIs and local instances (Ollama, LM Studio).
- **Streaming Support**: Native support for streaming responses with incremental updates.
- **Pluggable Runners**: A flexible "Runner" architecture to implement and test different LLM use cases.
- **Type-Safe Models**: Clear data structures for messages, tools, and model configurations.

## Getting Started

*(Instructions for building with Stack/Cabal will be added as the implementation progresses.)*

## CLI Parameters

The CLI supports a small set of flags and positional arguments:

- `--interactive`: Start interactive multi-turn chat mode.
	- Module: [app/Main.hs](app/Main.hs), [src/Oasis/Runner/Interactive.hs](src/Oasis/Runner/Interactive.hs)
- `--stream`: Stream tokens instead of waiting for a full response (single-turn mode).
	- Module: [app/Main.hs](app/Main.hs), [src/Oasis/Runner/Chat.hs](src/Oasis/Runner/Chat.hs)
- `--show-thinking`: When streaming in interactive mode, display thinking tokens if provided by the model.
	- Module: [app/Main.hs](app/Main.hs), [src/Oasis/Runner/Interactive.hs](src/Oasis/Runner/Interactive.hs)

Positional arguments:

- `alias`: Provider alias (e.g., `deepseek`, `qwen`). Defaults to `deepseek`.
	- Module: [app/Main.hs](app/Main.hs), [src/Oasis/Config.hs](src/Oasis/Config.hs)
- `prompt`: Single-turn prompt text (only when not using `--interactive`).
	- Module: [app/Main.hs](app/Main.hs), [src/Oasis/Runner/Chat.hs](src/Oasis/Runner/Chat.hs)
