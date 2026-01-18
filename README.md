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

## CLI Parameters

Usage:

```
oasis-cli <provider> <model|default|-> <runner> [runner args...]
```

Positional arguments:

- `provider`: Provider alias (e.g., `deepseek`, `qwen`).
- `model|default|-`:
	- `default` or `-` means “do not override,” use the provider’s configured default model.
	- Any other value is used as the explicit model ID.
- `runner`: `basic` or `chat`.

Runner args:

- `basic [--extra-args <json>] <prompt...>`: Single-turn, non-streaming. Prints raw request/response JSON for debugging.
- `chat [--no-stream] [--hide-thinking] [--extra-args <json>] [initial prompt...]`: Multi-turn chat. Defaults to streaming and showing thinking.
- `structured-json [--extra-args <json>]`: Structured output (JSON object).
- `structured-schema [--extra-args <json>]`: Structured output (JSON schema).
- `tool-calling [--extra-args <json>]`: Tool calling demo.

### Extra Args (Model Parameters)

Use `--extra-args` to pass OpenAI-compatible request parameters as JSON. Field names should follow the OpenAI API naming (snake_case). Example parameters supported:

- `temperature`
- `top_p`
- `max_completion_tokens`
- `stop` (string or array)
- `presence_penalty`
- `frequency_penalty`
- `seed`
- `logit_bias` (object)
- `user`
- `service_tier`
- `reasoning_effort`
- `stream_options` (object)

Example (basic runner prints request JSON):

```
oasis-cli deepseek - basic --extra-args '{"temperature":0.2,"top_p":0.9,"max_completion_tokens":64,"stop":["\n\n"],"logit_bias":{"123":-2},"user":"u1","service_tier":"default","reasoning_effort":"low","stream_options":{"include_usage":true}}' 你好
```
