# Oasis: Haskell LLM Integration Library

Oasis is a Haskell library and set of apps that provide a unified interface for multiple LLM providers. It focuses on type safety, configuration-driven provider selection, and streaming support.

## Overview

- **Unified API** for Chat, Structured Output, Tool Calling, Embeddings, and Completions.
- **Configuration-driven** provider/model selection via TOML.
- **Streaming-first** design with structured chunks.
- **Pluggable runners** for different LLM workflows.

---

## 1) Library: src/

The library is layered to keep protocol logic pure and keep UI/printing in apps.

### Public API (Exposed Modules)

**Core**

- `Oasis.Types`: protocol-level types (messages, tools, usage, errors).
- `Oasis.Model`: model/provider resolution and high-level model abstractions.
- `Oasis.Config`: TOML configuration parsing and defaults.

**OpenAI-Compatible Client**

- `Oasis.Client.OpenAI`: main entry point for OpenAI-compatible requests.
- `Oasis.Client.OpenAI.Param`: request parameter helpers.

**Chat Session Support**

- `Oasis.Chat.History`: conversation history and editing.
- `Oasis.Chat.Log`: session logging helpers.
- `Oasis.Chat.Message`: message constructors and helpers.
- `Oasis.Chat.Prompt`: prompt composition (extensible).

**Runners (LLM Capabilities)**

- `Oasis.Runner.Chat`: multi-turn chat with streaming toggle.
- `Oasis.Runner.Basic`: single-turn, non-streaming, JSON-in/JSON-out.
- `Oasis.Runner.StructuredOutput`: structured JSON or JSON schema output.
- `Oasis.Runner.ToolCalling`: tool calling.
- `Oasis.Runner.Embeddings`: embeddings.
- `Oasis.Runner.PrefixCompletion`, `Oasis.Runner.FIMCompletion`: completion modes.
- `Oasis.Runner.PartialMode`, `Oasis.Runner.Responses`, `Oasis.Runner.Hooks`, `Oasis.Runner.GetModels`.

**Service Layer**

- `Oasis.Service.Amap`: external service integration (geo/demo usage).

**Typical use**: load config, resolve provider/model, then call a client or runner using those settings.

### Internal Modules (Not Exposed)

Modules under `Oasis.Client.OpenAI.*` (HTTP, request building, streaming, and internal types) and `Oasis.Runner.*` internal helpers are implementation details and may change without notice.

---

## 2) CLI Application

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
- `--extra-args <json>`: pass OpenAI-compatible parameters (snake_case).

**Runner-specific notes**

- `basic [--raw <json>] <prompt...>`: single-turn, non-streaming. With `--raw`, input is a JSON `messages` array.
- `chat [--no-stream] [--hide-thinking] [initial prompt...]`: multi-turn chat. Default is streaming with thinking visible.
- `structured-json` / `structured-schema`: structured output modes.
- `tool-calling`: tool calling demo.
- `embeddings <text...>`: generate embeddings.
- `responses <input...>`: OpenAI Responses API with plain text input.

### Examples

Basic runner with extra args:

```
oasis-cli deepseek - basic --extra-args '{"temperature":0.2,"top_p":0.9,"max_completion_tokens":64}' 你好
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
oasis-cli qwen - hooks "Hello hooks"
```

Responses:

```
oasis-cli qwen - responses "Hello responses"
```

---

## 3) TUI Application

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

---

## Installation, Setup, and Run Guide

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
