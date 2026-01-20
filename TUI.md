下一步我们准备在现有能力（L1、L2及L3的一部分）基础上创建一个 TUI 应用，以提供更多高级功能。

## 项目目标与依赖

将目前项目 component 之一的 `oasis-cli` 的 `source-dirs` 从 `app` 改为 `cli`；增加一个 `executable` 命名为 `oasis-tui`，其源目录设为 `tui`，TUI 源代码置于此。

从一般角度考虑，Haskell 最主要的 TUI 库 [brick](https://github.com/jtdaugherty/brick) 及其第三方扩展 [brick-panes](https://github.com/kquick/brick-panes)、[brick-skylighting](https://github.com/jtdaugherty/brick-skylighting)，以及语法高亮库 [skylighting](https://github.com/jgm/skylighting) 是需要重点考虑的核心依赖。

## 设计理念：TUI 与 CLI 的核心差异

TUI 不是 CLI 的简单包装，而是在交互体验上的全面升级：

1. **显示更漂亮清晰**：语法高亮、结构化显示（JSON/Markdown）、消息历史可视化、流式输出实时渲染。
2. **输入体验更好**：多行编辑、方向键移动、拷贝粘贴、对话框参数输入、自动补全。
3. **更多高级功能**：Verbose Mode（消息历史编辑）、Advanced Mode（请求前确认）、右侧边栏高级面板。

TUI 保持与 CLI 相同的分层架构（L1-L3），仅作为 L4 表示层提供更优秀的用户体验。

## 非功能需求

- TUI 应划分为几个主要窗体 *pane*，可以用快捷键（通常是一个数字或字母按键）切换到不同区域。
- 使用方向键导航，使用 Enter 键选择或确认，使用 ESC 键取消或放弃。
- 需要用户输入文本时要暂停处理界面快捷键处理，所以要有进入退出编辑状态的方法；应提供方便易用的输入体验如方向键移动、拷贝粘贴、多行文本输入等。
- 对需要显示 Markdown、JSON 等文本或数据或源代码的窗口，应该提供语法高亮和尽可能美观易读的显示格式。
- 尽可能兼容宽度 80 字符以上、高度 40 字符以上的终端窗口。

## 功能需求

- 大语言模型（LLM）选择：通过 `provider` 和 `model` 选择模型，支持列表选择和手工输入，兼顾方便和灵活。
- 可以从列表中选择一个 *runner* 来执行；可以方便地设置参数；最好有个方便的方法，以后增加了新的 *runner* 也能自动加载到可选列表。
- 高级开关：对任何 *runner* 都有效：
  - Verbose Mode：开启后在右侧边栏显示会话中消息历史 *message list*，可以选择任意一项查看、修改、删除，也可以手工增加 *message* 项，类似 CLI app 中 Chat runner 支持的 `/show` `/system` `/insert` `/update` `/delete` 等命令的功能。
  - Advanced Mode：开启后每次发送请求到 LLM 之前暂停，在右侧边栏允许用户查看和修改传送的 *message list* 和其他参数。

## 实现方案

### 项目结构调整
1. **目录调整**：
   - 将 `oasis-cli` 的 `source-dirs` 从 `app` 改为 `cli`
   - 创建 `tui` 目录作为 `oasis-tui` 可执行文件的源目录
   - 保持现有的 `src` 库结构不变

2. **依赖添加**（`package.yaml`）：
   ```yaml
   dependencies:
     - brick
     - brick-panes
     - brick-skylighting
     - skylighting
     - vty
   ```

3. **可执行文件定义**：
   ```yaml
   executables:
     oasis-cli:
       main:                Main.hs
       source-dirs:         cli
       ...
     oasis-tui:
       main:                Main.hs
       source-dirs:         tui
       dependencies:
         - oasis
         - brick
         - brick-panes
         - brick-skylighting
         - skylighting
         - vty
   ```

### 总体架构设计
**分层原则**：TUI 作为 L4（表示层），对标原 CLI 应用，仅负责用户交互和渲染，不侵入 L1-L3 的功能分层。所有 LLM 协议、会话管理、runner 逻辑均通过现有库接口调用。

**模块划分**：
- `TUI.Main`：Brick 应用入口，初始化、事件循环
- `TUI.State`：应用状态类型定义
- `TUI.UI`：各窗格的渲染函数
- `TUI.Handlers`：键盘事件处理
- `TUI.Dialogs`：模态对话框系统（用于不常用的临时输入）
- `TUI.RunnerIntegration`：调用现有 runner 模块的适配层
- `TUI.Utils`：辅助函数（焦点管理、文本处理等）

**数据流**：
```
Brick 事件循环 → Handlers → 更新 State → UI 渲染 → VTY 输出
                    ↓
            RunnerIntegration → Oasis 库
```

### 布局设计
采用 **左右边栏加中央主区域加底部状态栏**的布局，右侧边栏用于高级模式（稍后实现），对话框仅用于不常用的临时输入：

```
┌─────┬────────────────────────────┬─────┐
│     │                            │     │
│  左 │      中央主区域            │  右 │
│  侧 │    (核心交互/显示)         │  侧 │
│  边 │                            │  边 │
│  栏 │                            │  栏 │
│     │                            │     │
├─────┴────────────────────────────┴─────┤
│  底部状态栏/日志                       │
└────────────────────────────────────────┘
```

**区域划分**：
1. **左侧边栏**（宽度 20%，固定）
   - Provider 列表（从 `providers.toml` 加载）
   - Model 列表（基于所选 provider 的 `model_ids`，含"手动输入…"选项）
   - Runner 列表（动态发现所有 `Oasis.Runner.*` 模块）

2. **中央主区域**（宽度 60%，动态）
   - **非 Chat Runner**：输出显示区域（支持语法高亮的 JSON/Markdown）
   - **Chat Runner**：
     ```
     ┌─────────────────────────────────────┐
     │ 消息历史 (可滚动 viewport)          │
     │  [user] Hello...                    │
     │  [assistant] Hi...                  │
     ├─────────────────────────────────────┤
     │ 输入区域 (多行编辑框)               │
     │ >>>                                 │
     └─────────────────────────────────────┘
     ```

3. **右侧边栏**（宽度 20%，初始隐藏或留空）
   - 包含两个可切换/并排显示的窗体：
     1. **消息历史列表**（Verbose Mode）：显示当前会话的所有消息，支持选择、编辑、删除
     2. **消息详情/参数确认**（Advanced Mode）：显示选中消息的详细内容，或发送请求前的参数确认面板
   - 默认可能折叠或显示辅助信息，高级模式启用时自动显示对应窗体

4. **底部状态栏**（高度 10-15%）
   - 状态信息：当前 provider/model/runner/API 状态
   - 操作反馈：请求进度、错误信息
   - 快捷键提示：当前可用操作

### 快捷键设计
**窗体直接激活键**（全局生效，直接切换到对应窗体）：
- `P`：跳转到 Provider 列表（左侧边栏）
- `M`：跳转到 Model 列表（左侧边栏）
- `R`：跳转到 Runner 列表（左侧边栏）
- `V`：跳转到输出显示区域（中央主区域）
- `E`：跳转到输入区域（中央主区域，Chat runner 专用）
- `L`：跳转到消息历史列表（右侧边栏，Verbose Mode）
- `D`：跳转到消息详情/参数确认（右侧边栏，Advanced Mode）
- `ESC`：取消当前操作/请求，关闭对话框
- `F1`：显示帮助页面
- `F2`：切换 Verbose Mode
- `F3`：切换 Advanced Mode
- `Q`：退出应用

**选择浏览窗体操作**（适用于列表类窗体：Provider/Model/Runner/消息历史列表）：
- `↑/↓`：上下移动选择项
- `Enter`：确认选择（如选择 provider 后加载 models）
- `ESC`：退出当前窗体或取消选择
- `字母/数字`：快速跳转到匹配项
- `空格`：切换选中状态（如多选场景）
- `Tab`/`Shift+Tab`：在列表项内字段间切换（如有子字段）

**输入窗体操作**（适用于文本输入类窗体：输入区域、对话框输入框）：
- 尽量遵循默认控件的行为标准，下列仅供参考。
- 标准文本输入：字母、数字、符号
- `←`/`→`：光标左右移动
- `↑`/`↓`：光标上下移动（多行时）
- `Home`/`End`：行首/行尾
- `Ctrl+A`/`Ctrl+E`：行首/行尾（Emacs 风格）
- `Backspace`/`Delete`：删除字符
- `Ctrl+W`：删除前一个单词
- `Ctrl+U`：删除到行首
- `Ctrl+K`：删除到行尾
- `Ctrl+J`：插入新行（代替回车）
- `Enter`：完成输入/发送消息（根据上下文）
- `ESC`：取消输入/退出编辑模式
- `Tab`：缩进或跳转到下一个字段

**输出显示区域操作**（只读显示）：
- `↑`/`↓`：滚动查看历史输出
- `PgUp`/`PgDn`：翻页
- `Home`/`End`：滚动到顶部/底部
- `Ctrl+L`：清空输出区域
- `ESC`：退出滚动模式（如有）

**全局操作**（任何焦点下生效）：
- `ESC`：通用取消/返回
- `Q`：退出应用
- `F1`：显示帮助
- `F2`/`F3`：切换高级模式

### 状态数据结构
```haskell
data FocusArea 
  = ProviderList      -- 左侧：Provider 列表
  | ModelList         -- 左侧：Model 列表
  | RunnerList        -- 左侧：Runner 列表
  | OutputDisplay     -- 中央：输出显示区域
  | InputArea         -- 中央：输入区域（Chat runner）
  | MessageList       -- 右侧：消息历史列表（Verbose Mode）
  | MessageDetail     -- 右侧：消息详情/参数确认（Advanced Mode）
  | DialogArea        -- 模态对话框
  | BottomPanel       -- 底部状态栏（只读）

data DialogType 
  = ModelInputDialog           -- 手动输入模型名
  | RunnerParamsDialog         -- runner 参数设置
  | MessageEditDialog Int      -- 编辑消息历史项
  | HelpDialog                 -- 帮助页面

data RunnerUI = ChatRunnerUI | BasicRunnerUI | EmbeddingsRunnerUI | ...

data AppState = AppState
  { focusRing     :: FocusRing FocusArea
  , leftPanel     :: LeftPanelState
  , centralPanel  :: CentralPanelState
  , bottomLog     :: [Text]
  , currentDialog :: Maybe DialogType
  , dialogState   :: Maybe DialogData
  , config        :: Config
  , provider      :: Maybe Provider
  , modelOverride :: Maybe Text
  , selectedRunner:: Text
  , runnerParams  :: Aeson.Value           -- 当前 runner 参数
  , chatHistory   :: History               -- 仅 Chat runner 使用
  , flags         :: Flags                 -- {verbose, advanced}
  , lastResult    :: Maybe RunnerResult
  , requestInProgress :: Bool              -- 当前是否有请求进行中
  }
```

### 对话框系统设计
**设计原则**：对话框仅用于不常用的临时输入（如手动输入模型名、一次性参数调整）。常用操作应通过主界面直接完成。

**实现原则**：
1. 通过 `currentDialog :: Maybe DialogType` 控制对话框显示
2. 对话框渲染时叠加在基础 UI 之上
3. 对话框内焦点独立管理
4. 支持确认/取消操作

**对话框类型**：
1. **模型手动输入**：单行文本输入，验证模型名有效性
2. **Runner 参数设置**：基于 runner 类型的动态表单
   - Basic/Chat：temperature, max_tokens, streaming 等
   - Embeddings：dimensions, encoding_format 等
   - 其他 runner 特定参数
3. **消息历史编辑**（Verbose Mode）：
   - 列表显示所有消息
   - 支持增删改查，角色选择
4. **帮助页面**：显示所有快捷键说明

### Chat Runner 特殊处理
**多轮对话管理**：
- 维护独立的 `History` 状态
- 消息历史显示为可滚动列表，每条消息显示角色和内容摘要
- 输入框支持多行编辑，`Ctrl+J` 换行，`Enter` 发送

**Verbose Mode**：
- 激活时右侧边栏显示消息历史列表，可直接查看/编辑
- 支持 CLI 中所有 `/show` `/insert` `/update` `/delete` 操作的 GUI 版本

**流式输出渲染**：
- 实时显示 thinking tokens（如 DeepSeek-R1）
- 保持滚动位置，新内容自动显示在可视区域
- 支持中途取消（`ESC`）

### Runner 参数动态发现
**自动加载机制**：
```haskell
-- 通过 Template Haskell 或运行时反射发现所有 runner
discoverRunners :: IO [(Text, RunnerInfo)]
discoverRunners = do
  let runners = 
        [ ("basic", basicRunnerInfo)
        , ("chat", chatRunnerInfo)
        , ("embeddings", embeddingsRunnerInfo)
        -- ... 其他 runner
        ]
  pure runners

data RunnerInfo = RunnerInfo
  { displayName   :: Text
  , needsPrompt   :: Bool          -- 是否需要文本输入
  , paramSchema   :: Aeson.Value   -- 参数 JSON Schema
  , executeFunc   :: RunnerFunc    -- 执行函数
  }
```

**参数表单生成**：
- 基于 `paramSchema` 动态生成输入字段
- 支持常见类型：数字、布尔、字符串、枚举
- 提供合理的默认值

### 实施计划

#### 阶段 1：基础框架
1. 修改 `package.yaml` 调整 `source-dirs`，添加 TUI 依赖
2. 迁移 CLI 代码：将 `app/Main.hs` 移动到 `cli/Main.hs`
3. 创建 `tui/Main.hs` Brick 骨架，实现基本布局渲染
4. 集成 `Oasis.Config` 加载 providers，显示在左侧列表

**交付物**：可运行的空 TUI，显示基本布局和 provider 列表

#### 阶段 2：核心交互与 Basic Runner
1. 实现左侧面板完整功能：provider/model/runner 列表交互
2. 实现中央区域动态内容切换，非 Chat runner 输出显示区域（支持语法高亮）
3. 集成 Basic runner 执行，显示结果，**把显示效果部分做好**
4. 实现对话框系统基础框架，支持弹出对话框输入参数（如 temperature、max_tokens 等）

**交付物**：可选择 provider/model/runner、执行 basic runner 并查看美观输出、可通过对话框调整参数的 TUI

#### 阶段 3：Chat Runner 与高级显示
1. 完善 Chat runner 界面：消息历史显示、输入框多行编辑
2. 实现流式输出渲染，支持 thinking tokens
3. **所有显示和输入体验在这里做好**
4. 添加 Verbose Mode 对话框（消息历史编辑）
5. 实现 Advanced Mode 暂停确认功能

**交付物**：功能完整的 Chat runner TUI，支持多轮对话、流式输出和高级模式

#### 阶段 4：其他 Runner 与高级功能
1. 实现其他 runner（Embeddings、Structured、ToolCalling 等）
2. 完善 runner 参数对话框系统（基于 JSON Schema 的动态表单）
3. 添加语法高亮（JSON/Markdown）
4. 错误处理和状态反馈优化
5. 终端兼容性测试（80×40 最小尺寸）
6. **实现高级功能**：右侧边栏的 Verbose Mode 和 Advanced Mode 面板

**交付物**：生产可用的 TUI，覆盖所有需求，支持所有 runner 和高级功能

### 技术风险与缓解
1. **Brick 性能**：流式输出时避免频繁全屏重绘，使用增量更新
2. **依赖兼容性**：确保 brick、skylighting 与 GHC 9.10.3 兼容
3. **焦点管理**：复杂对话框场景下焦点环可能混乱，设计清晰的焦点切换逻辑
4. **异步请求**：LLM 请求可能阻塞 UI，考虑使用 `async` 和状态标志

### 验证标准
每个阶段完成后应通过以下验证：
1. **编译通过**：`stack build` 无错误
2. **类型检查**：HLS 诊断无问题
3. **基本功能**：阶段目标功能可交互操作
4. **错误处理**：API 错误、网络问题等有合理反馈
5. **快捷键**：定义的快捷键工作正常

### 后续扩展方向
1. **右侧高级面板**：显示 Verbose Mode 消息列表或请求详情
2. **主题支持**：颜色方案切换
3. **会话管理**：保存/加载对话历史
4. **插件系统**：第三方 runner 动态注册
5. **批量操作**：同时向多个模型发送请求比较结果
