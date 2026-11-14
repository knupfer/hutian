# Changelog

All notable changes to Hutian are documented here. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Added

- A shared strict command-line parser in `Hutian.Cli` for `hutian-shell` and
  `hutian-llm-relay`. Unknown or bare arguments, missing values, and repeated
  options are now rejected with a usage error instead of being ignored.
- A pure `Hutian.Tool.planTool` step that turns the configured shell policy and
  a model tool call into run, approve, or refuse. Main performs only the IO that
  the plan requests, so policy decisions are covered without a terminal.
- Tests for strict option parsing, tool-call planning, socket framing helpers,
  and LLM error responses.
- Shell syntax highlighting with Skylighting for term-mode input and
  model-requested `tool shell:` commands.
- Mandatory `sandbox` and `model` option groups. `sandbox.packages` selects the
  sandbox's package closures. `sandbox.internet = false`
  forces gVisor's `none` network mode and cannot be overridden with
  `GVISOR_NETWORK`. `model.shell` accepts `"deny"`, `"ask"`, or `"allow"`.
  `"deny"` disables only the model's `shell` tool; term mode and `Ctrl+X`
  remain available. `"ask"` advertises the tool but requires interactive
  approval for each command; non-interactive runs deny tool calls. The approval
  prompt colours `approve:` with the user's pink, shows `[y] Yes` and `[n] No`
  on separate lines while the cursor is hidden, and collapses them into a
  single `Yes` or `No` line afterwards. Keys other than `y`/`n` are ignored.
  Ctrl+C aborts the current LLM turn instead of denying the command. The tool is
  omitted from the request in deny mode, and tool calls are refused when they
  arrive anyway.
- A capability matrix in the README documenting how `sandbox.readOnly`,
  `sandbox.nix`, `sandbox.internet`, `model.shell`, and the relay compose,
  including the fact that `sandbox.internet = false` covers direct sandbox
  networking but not host Nix fetches or the fixed relay upstream.
- Unknown flake options are rejected. The helpers list the offending paths and
  the known option names, so a misspelled key such as `model.foo` fails instead
  of being ignored.
- Host-side API key handling. `model.apiKeyFile` and `model.apiKeyCommand` start
  `hutian-llm-relay`, which keeps the key on the host and listens on a private
  per-session Unix socket. The sandbox only receives `--llama-socket
  /run/hutian/llm.sock`, so the key never appears in argv, environment
  variables, `/source`, `/tmp`, or the Nix store.
- `--llama-socket` support in `hutian-shell`, with a minimal body/status
  client for Unix domain sockets and streaming SSE responses.
- Relay tests covering key injection, fixed-upstream forwarding, fixed
  headers, body limits, redaction, error forwarding, and the full
  shell-to-relay-to-upstream path.
- HTTPS support for OpenAI-compatible model endpoints. The HTTP client now uses
  TLS, and the sandbox ships the nixpkgs CA bundle together with
  `SYSTEM_CERTIFICATE_PATH` so `https://` values for `model.url` are verified.

### Changed

- The built-in defaults are now safe-by-default: `sandbox.internet = false`,
  `model.shell = "ask"`, and the system prompt is `You are a coding agent.`.
- `sandbox.internet = true` now selects gVisor's `host` network mode explicitly.
  Rootless gVisor does not support `sandbox` networking and silently falls back
  to `host`, so the launcher no longer requests the unsupported mode.
  `GVISOR_NETWORK` can still narrow the mode to `none`.
- Direct TCP LLM requests no longer follow redirects, and non-success responses
  are reported the same way whether they arrive directly or through the relay.
  Error bodies and relay keys are read with explicit size limits.
- Sandbox and model settings are now grouped. `sandbox` contains `packages`,
  `readOnly`, `nix`, and `internet`; `model` contains `name`, `url`,
  `systemPrompt`, `shell`, and the optional `apiKeyFile`/`apiKeyCommand`. The
  internal launcher arguments stay flat.
- The `shell` flake option and `--allow-shell` launcher flag were replaced by
  `model.shell` and `--model-shell deny|ask|allow`. The new path and name
  reflect that the option controls the model's shell tool, not the user's term
  mode.
- The relay is always used for model traffic. Without an API key it starts with
  `--no-auth`, does not read standard input, and forwards without an
  `Authorization` header. The sandbox still receives only the relay socket and
  the logical `http://hutian-relay/...` URL.
- When a key is configured, the relay reads it from standard input. The
  launcher pipes either `model.apiKeyFile` or `model.apiKeyCommand` into it, so
  the key never appears in the relay's argument list or environment.
- The relay now uses a private body/status protocol instead of HTTP/1.1. The
  sandbox sends only a request body; the host relay builds the single fixed
  upstream request, so there is no client-supplied request line or header block
  to parse, validate, strip, or redirect. The response is a four-byte status
  code followed by the upstream body.
- The sandbox network now follows the mandatory `sandbox.internet` option
  instead of the presence of an API key. With `sandbox.internet = true`,
  `GVISOR_NETWORK` may still narrow the gVisor mode; with
  `sandbox.internet = false`, networking is always `none`.
- `model.apiKeyFile` must not point into the Nix store or the sandbox working
  directory, and `model.apiKeyFile` and `model.apiKeyCommand` are mutually
  exclusive.
- Terminal colours, styles, and cursor movement are now generated with
  `ansi-terminal` from typed RGB values instead of hand-written escape codes,
  so palettes can be darkened before they are encoded.
- Secondary labels such as `output:`, `tool output:`, and `think:` are dimmed
  to 60 percent, matching the dimmed highlighting in thinking blocks.
- Terminal IO now goes through a single `Hutian.Console` boundary. Prompt
  policy is a pure reducer in `Hutian.Prompt`, and `Hutian.Shell` returns
  captured command output as data instead of printing it.

### Fixed

- Interrupting a model reply with `Ctrl+C` no longer kills the host relay. The
  relay ignores the terminal's `SIGINT`, so its socket stays available for
  follow-up prompts; the launcher still stops it with `SIGTERM` on exit.
- `model.apiKeyCommand` now runs in the foreground, so interactive prompts such as
  `passage` can wait for input and be cancelled with `Ctrl+C`. The launcher no
  longer reports a missing relay socket while the command is still running.
- The launcher restores the terminal settings on every exit path, including an
  interrupted `model.apiKeyCommand`, so echo and the cursor are not left disabled.
- Pasting large blocks of text no longer redraws the prompt once per character;
  a queued burst of input is rendered as a single frame.
- The cursor stays blinking teal for the whole model turn, including shell tool
  calls, instead of switching back to pink while a tool command runs.

## [0.4.0.0] - 2026-11-11

### Added

- The terminal cursor is pink while the user is typing and blinks teal while
  the model streams a reply, even as new output arrives.
- LLM-mode input is highlighted as Markdown while typing, including inline
  emphasis, links, lists, and fenced code blocks.
- LLM mode shows a bold pink `ask:` line before the input, and its replies are
  labelled with bold teal `think:` and `answer:` lines.
- Shell output is preceded by an `output:` line and tool output by a
  `tool output:` line, each in a light version of the command's role colour.

### Changed

- Text typed while the model is responding is no longer echoed into the
  streamed output; it appears in the next prompt instead.
- Term-mode and tool commands now always run in a fresh `/bin/sh` in the
  sandbox working directory. Shell state such as `cd` and exported variables
  no longer persists between commands.
- `Ctrl+D` on empty input now exits the shell in both llm and term mode.
- Term mode uses a fixed `shell:` label instead of the working directory,
  since commands do not share a shell.
- Every prompt label occupies its own line above the input: `ask:`, `shell:`,
  and `tool shell:`.
- Tool calls are shown with a bold teal `tool shell:` line.
- The `run_shell_command` tool has been renamed to `shell`.
- Removed all background colouring. Modes are distinguished by their prompt
  prefixes and foreground colours instead.
- Refined the Markdown palette into one consistent scheme: a blue-to-rose
  heading ramp, warm yellow emphasis, green code and fence markers, underlined
  teal links, peach list markers, and muted grey quotes and rules.
- Markdown highlight colours are scaled down in thinking blocks, including
  syntax-highlighted code, so reasoning stays visually subordinate.
- `mkHutianShellFlake` now requires every option: `packages`, `systemPrompt`,
  `model`, `llamaUrl`, `readOnly`, and `nix`. The `src`, `systems`, and
  `pkgsFor` options were removed from the helper. Missing options are reported
  by name, with the full required set, instead of Nix's low-level argument
  error.
- Llama connection settings are passed to `hutian-shell` as `--llama-url`,
  `--model`, and `--system-prompt` arguments instead of `HUTIAN_*` environment
  variables.
- The launcher starts the sandbox with `runsc -cwd /source`, sets
  `TERM=xterm-256color` and `LANG=C.UTF-8` directly, and no longer reads the
  host's `TERM`, `LANG`, or `NIX_CONFIG`.

### Removed

- The term-mode `cd`, `pwd`, `llm`, `exit`, and `quit` builtins. `help` is the
  only remaining builtin.
- The `HUTIAN_LLAMA_URL`, `HUTIAN_LLAMA_MODEL`, `HUTIAN_SYSTEM_PROMPT`, and
  `HUTIAN_WORKDIR` environment variables, the `GVISOR_PATH` variable, the
  `nix/required-option.nix` helper, and the `required-options` check.

### Fixed

- Large package closures no longer make the sandbox fail at startup with an
  EOF error. The launcher bind-mounts closure paths into the rootfs instead of
  passing one gVisor volume per path, which exceeded gVisor's reserved stdio
  file descriptors at around 250 store paths.
- Hutian now starts when the working directory is itself a mountpoint with
  nested mounts, such as `/nix`. The working directory is recursively bound
  into the rootfs instead of being passed as a gVisor volume, which failed with
  EINVAL and surfaced as the same EOF error.
- LLM-mode input redraws as a single frame, so highlighted input no longer
  flickers while typing.
- `Ctrl+C` now interrupts the running term-mode or tool command without
  terminating hutian. The interrupt handler is installed once for the process
  instead of being torn down while the child shell is still winding down.

## [0.3.0.0] - 2026-11-10

### Added

- A required `readOnly` option. When true, the host working directory is
  bind-mounted read-only at `/source`, so the sandbox cannot modify it.
- LLM tool calls. The model can request shell commands through the
  `run_shell_command` tool, which runs `/bin/sh -c` in the current directory
  and feeds the output back into the conversation.
- Streaming tool-call parsing and tool-call/tool-result conversation messages.
- A tool-call prompt style with a dark red background and a bright red path.
- `Hutian.Command`, a pure parser for term-mode builtins.
- `Hutian.Completion`, a pure completion selector shared by commands and paths.
- `Hutian.Render`, a pure ANSI prompt renderer.
- `ChatRole` for typed OpenAI-compatible chat messages.
- Pure history encoding/decoding and input routing.
- Pure terminal-input classification, shell directory resolution, and SSE
  stream parsing.
- LiquidHaskell invariants for completion selection, common-prefix bounds, and
  completion path lengths.
- Shared `TestSupport` helpers and additional unit and property tests for
  command parsing, path completion, prompt layout, and completion targets.
- MIT `LICENSE` and package metadata for the changelog and README.

### Changed

- Input lines are no longer padded with spaces, so copied prompt text stays
  minimal. The mode background still extends to the end of each line.
- Missing required `mkHutianShell`/`mkHutianShellFlake` options now report the
  option name and an example value instead of a missing flake attribute.
- Term-mode command output is now gray on a slightly darker blue background,
  and tool-call output is gray on a slightly darker red background.
- `mkHutianShell` and `mkHutianShellFlake` now require a `readOnly` argument.
- Simplified the flake outputs and package definition, and factored the lint
  and LiquidHaskell checks into a shared `mkCheck` helper.
- Replaced stringly-typed chat roles with `ChatRole`.
- Bundled the main loop and terminal prompt loop into records.
- Made `completionTarget` return a non-optional target.
- Moved path completion helpers to `Hutian.Paths` and prompt-prefix layout to
  `Hutian.Editor`.
- Simplified history navigation, Markdown prefix matching, and streaming
  reasoning output.
- Removed the remaining partial functions from application code; `NonEmpty.head`
  is now the only `head` use and is total.
- Made prompt layout total for non-positive widths and hid the `Row`,
  `Column`, and `GoalColumn` constructors.
- Reduced the IO surface by moving history codecs, completion selection,
  prompt rendering, shell resolution, and SSE parsing into pure modules.
- Added pure `PromptAction` key routing so the terminal loop only performs IO.
- Made `cd` and `llm` command payloads optional strings, removing empty-list
  handling from the interpreter.
- Read and wrote history files strictly to avoid lazy IO errors.
- Simplified LLM choice/delta traversal and Markdown chunk accumulation.
- Switched the default language to `GHC2021`.
- Fixed `displayPath` incorrectly abbreviating sibling directories such as
  `/home/user2` for a home of `/home/user`.

## [0.2.0.0] - 2026-11-09

### Added

- Two-mode interactive prompt. The app starts in LLM mode, and `Ctrl+X`
  switches to term mode.
- Independent, persistent input buffers for LLM and term modes.
- Persistent command and prompt histories.
- LLM conversation context containing prior prompts, assistant replies, and
  terminal commands.
- Configurable system prompt, model name, and llama.cpp endpoint.
- Streaming Markdown highlighting, including language-specific syntax
  highlighting for fenced code blocks.
- Grey reasoning/thinking output with Markdown highlighting.
- Term-mode path prompt, command execution, tab completion, history recall,
  and backslash/quote continuation.
- HUnit, QuickCheck, integration, filesystem, and shell tests.
- HLint, Statix, and DeadNix checks integrated into the build.
- LiquidHaskell verification of the pure path, conversation, editor, and
  history modules, also run as part of the package build.
- `nixfmt-tree` formatting through `nix fmt`.
- Strict GHC warnings with `-Werror`.
- Type-safety improvements: `Row`/`Column`/`GoalColumn` newtypes, private
  `Prompt` construction, `NonEmpty` prompt lines, and safe indexing helpers.
- Required Nix options and an example coding-agent system prompt.
- Flake checks for linter and required-option validation.

### Changed

- Consolidated lint and LiquidHaskell checks into `nix/lint.nix`, shared by
  the package build and flake checks.
- Moved the Haskell project to the repository root.
- LLM and term histories are stored separately.
- Input rendering now redraws relative to the current cursor.
- Refined the term and LLM background colors.
- Fenced code blocks now use their tagged language's lexer.
- The check phase is bounded and no longer susceptible to pty test hangs.

### Fixed

- Input no longer transfers between modes when toggling.
- LLM drafts survive terminal command execution.
- Newly created input lines receive the correct background color.
- LLM cursor navigation no longer causes full-prompt flicker.
- Tab-completion suggestions no longer inherit the mode background.
- Thinking output is retained and displayed.
- `Ctrl+D` no longer submits terminal-mode input.
- `Ctrl+C` clears the active input.
