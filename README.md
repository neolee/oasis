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
