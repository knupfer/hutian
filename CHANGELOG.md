# Changelog

All notable changes to Hutian are documented here. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Added

- A `tasty-bench` suite for the paths a session exercises: a full model reply
  streamed through the Markdown/ANSI presenter (including a 2000-chunk fast
  decode), a 1 MiB shell result through the captured and streamed output paths,
  highlighting, SSE parsing, editor and prompt reducers, redraw layout, and the
  startup PATH scan (`bench/`, `nix build .#checks.<system>.benchmark`).
- `scripts/bench.sh` drives the interactive TUI through a pty with `expect` for
  startup-to-prompt and keystroke-echo latency.
- The executables now accept RTS options, so `+RTS -s` prints allocation and GC
  statistics.
- Batch mode. When stdin or stdout is not a terminal, or a prompt argument is
  given, Hutian runs one quiet turn: the positional arguments are the first user
  message (or stdin is read to EOF), `"ask"` tools are denied, and only the main
  agent's final answer is printed to stdout.
- Opt-in session transcripts with the mandatory `output.transcript` option.
  Each session gets a fresh `$XDG_STATE_HOME/hutian/transcripts/<timestamp>/`
  directory, reserved atomically so existing transcripts are never overwritten.
  The launcher stages the files in a writable private directory and copies them
  out on exit, so it works even when `/source` is read-only and never writes
  transcripts into the project. A JSON Lines file records prompts, reasoning,
  assistant text, tool requests, approvals, results, subagent prompts, and
  diagnostics, plus `conversation.snapshot` events after each completed turn
  that preserve the exact conversation for a later session. A Markdown sibling
  gives every prompt and block label a level-one heading, demotes headings
  inside those blocks by one, and does not add synthetic code fences.

### Changed

- **Breaking:** positional arguments after the options are now the first user
  message for batch mode, not a command run in the sandbox. Use the model's
  shell tool, or an interactive term-mode command, to run a command. A bare
  `--` ends option parsing so a prompt may start with a dash.
- Tool commands, tool labels, subagent prompts, and handler blocks are dimmed;
  only the user's prompt and the top-level agent answer keep normal emphasis.
- Agent input prompts use the same `<path>.ask` label as the top-level user
  prompt, so a subagent's task is `foo.ask` and a handler's input is
  `mcp.<server>.<tool>.handler.ask`.
- Handler input is shown only at high verbosity. Normal and low print the
  handler label without the raw tool result.
- Captured shell output stays as bytes for display and is decoded to text once
  for the model, with no `String` round trip. A 1 MiB tool result takes about
  7 ms instead of about 100 ms, and output is still not capped.
- MCP result handlers now nest under the tool that produced the result: the
  block is labeled `mcp.<server>.<tool>.handler` and its prompts extend that
  path, instead of `mcp.<server>.handlers.<tool>`.

### Fixed

- An MCP server that closes an idle session no longer makes every later tool
  call fail with `HTTP 404: Not Found: Session not found`. Hutian starts a new
  session and retries the call once, then reuses that session for later calls.

## [0.11.0.0] - 2026-11-24

### Added

- An API reference for `hutian.lib` (`mkFlake`, `mkApp`, and `mkPackage`) and a
  documented, mandatory configuration schema.

### Changed

- **Breaking:** every MCP server must state `url`, `policy`, and `handlers`.
  Missing keys fail at evaluation time instead of silently defaulting.
- Tool documents are validated as strictly as the flake schema. A hand-written
  `--tools-file`, subagent document, or handler document now rejects missing
  fields and unknown keys instead of ignoring them.
- `job_kill` now asks for approval in `"ask"` mode. Listing jobs and reading a
  log stay unapproved.
- A model endpoint with one trailing slash is still treated as the chat
  completion API.
- MCP prompt labels keep the server's real names, while the model-facing
  function name stays API-safe. Tools that collide after sanitizing are
  reported, and only the first is advertised.
- MCP tool descriptions no longer add an `MCP server: <name>.` line. A result
  handler's description is appended after a single newline, without wrapper
  text, and the handler receives only the raw tool result as its prompt.

### Fixed

- MCP tool arguments are rendered as UTF-8 text, so accented characters no
  longer appear as mojibake in the tool-input block.
- An unreadable job log is reported as an error instead of appearing empty.
- JSON tool-call indices and MCP response ids reject fractional and
  out-of-range numbers.

## [0.10.0.0] - 2026-11-23

### Added

- MCP tool result handlers. `agent.tools.mcp.<server>.handlers.<tool>` takes a
  subagent configuration (`model`, `description`, `systemPrompt`, `tools`) for
  a concrete MCP tool. The handler gets the raw tool result, runs in its own
  context with its own tools, and returns only its final answer to the agent.
  Its description is appended to the tool description and its terminal label is
  `mcp.<server>.handlers.<tool>`. A handler's MCP tool results can have their own
  handlers. Built-in tools return their results directly.
- MCP tool arguments are shown with dim JSON syntax highlighting, and error
  lines are bold red when standard error is a terminal.
- A result handler whose tool name is not exposed by its MCP server is reported
  after discovery and ignored.

### Changed

- MCP prompt labels use `mcp.<server>.<tool>`, while the model-facing function
  name stays API-safe as `mcp__<server>__<tool>`.
- Prompt labels use dotted agent paths. The main agent prints `think`,
  `answer`, `output`, tool names, `system`, and `tools`. A subagent `foo`
  prefixes its prompts with `foo.`, so its reasoning is `foo.think`, its answer
  `foo.answer`, its tool input `foo.shell`, and its output `foo.shell.output`.
  Nested agents extend the path, for example `foo.bar.think`. The old
  `subagent <name>:`, `tool <tool>:`, `think:`, `answer:`, `output:`,
  `tool output:`, `system:`, and `tools:` labels are gone. The `ask`, `shell`,
  and `approve` labels also drop their trailing colon.
- Tool output labels include the path of the tool that produced them, for
  example `shell.output` or `mcp.<server>.<tool>.output`. When an MCP tool has
  a result handler, neither `<tool>.output` nor the raw result is printed:
  the raw result is the body of the handler's prompt instead. That prompt is
  rendered like tool output, with a dim label and a gray body, and follows
  tool-output visibility, so low verbosity hides the body.
- The flake configuration is split into `sandbox`, `endpoint`, `output`, and
  `agent` groups. `model.name` is now `agent.model`, matching subagents, which
  already use `<name>.model`. The shared host-side settings moved out of the
  agent: `model.url` is now `endpoint.url`, `model.apiKeyCommand` is
  `endpoint.apiKeyCommand`, and `model.verbosity` is `output.verbosity`.
  `agent` now has the same shape as a subagent, except for the `description`
  that only selectable subagents need. The old `model` group is rejected as an
  unknown option.
- Hutian's executables are now linked statically against their Haskell
  libraries, so the sandbox `/nix/store` only carries their runtime
  dependencies. The Haskell library, GHC package database, propagated build
  inputs, and dependency documentation are no longer mounted; an empty
  `sandbox.packages` gives a closure of under 30 store paths instead of roughly
  300.

### Removed

- The `websearch` tool and its `model.tools.websearch` configuration. Use an
  MCP search server instead; `webfetch` remains for direct page fetches.
- File-backed term and LLM prompt history. Both histories are now kept only in
  memory for the lifetime of the shell, so `/tmp/hutian-shell-history` and
  `/tmp/hutian-llm-history` are no longer written.

### Fixed

- Launchers no longer fail shellcheck when the model name, system prompt, tool
  configuration, endpoint URL, or API key command contains backticks or `$`.
  These values are now installed from store files instead of being embedded in
  the generated launcher script.

## [0.9.0.0] - 2026-11-22

### Added

- A mandatory `model.verbosity` option (`"low"`, `"normal"`, or `"high"`).
  Low shows tool calls and main-turn answers while printing only prompts for
  other blocks, including subagent answers. Normal also shows main-agent
  thinking, presents subagents like low, and streams each subagent's final answer
  in dim colors. High keeps the previous full output, with subagent answers
  using the same dim styling. It prints the exact system message and, per tool,
  a `# name(...)` heading followed by its complete description rendered as dim
  Markdown before the first main `ask:` and before every subagent block, using
  `system:`/`tools:` or `system <name>:`/`tools <name>:` with dim text.
- `model.tools` now configures every model tool. It has `shell`, `webfetch`,
  `subagent`, and `mcp` keys, each validated recursively for subagents.
- A `webfetch` tool with `deny`, `ask`, and `allow` policies. It runs in the
  sandbox, returns a bounded response body, and reports non-success statuses.
- A serial `subagent` tool. `model.tools.subagent` maps selector names to
  subagents. Each selector has an upstream `model`, a `description`, a
  `systemPrompt`, and its own tools; the selector list and descriptions appear
  in the tool schema so the main model can decide when to delegate. A subagent
  shares the main API and tool loop and returns only its final answer. Its work
  starts under a blue `subagent <name>:` line, followed by the submitted task.
  Reasoning, answers, tool prompts, and output labels inside a subagent use the
  dim subagent blue; tool input text is dim gray, and the cursor blinks blue
  while it runs.
- Remote MCP servers through `model.tools.mcp`. Hutian performs the Streamable
  HTTP handshake at startup, discovers each server's tools, advertises them as
  `mcp__<server>__<tool>`, and calls them with the server's session id.
- `--tools-file` carries the complete tool JSON to the shell, alongside
  `--model-webfetch` and `--subagent-file` for direct use.

### Fixed

- The LLM relay sends `Connection: close` upstream and retries once when a
  server closes before sending response headers. This avoids `IncompleteHeaders`
  failures caused by stale keep-alive connections or an upstream that drops the
  first request.

### Changed

- Replaced the mandatory top-level `model.shell` option with `model.tools.shell`.
  Tool configuration is written to `/run/hutian-config/tools` and removed by the
  shell before it accepts input.
- `Hutian.Policy` now exposes one shared `ToolPolicy` (`ToolDeny`, `ToolAsk`,
  `ToolAllow`) with JSON instances, and `Hutian.Tool.Config` owns the JSON shape
  and safe defaults for the whole tool tree. `Hutian.Tool.Catalog` keeps the
  runtime MCP sessions and discovered tools separate from that configuration.
- `Hutian.Llm.Loop` can return the final model result for subagents while the
  existing conversation-only loop remains available.

## [0.8.0.0] - 2026-11-19

### Fixed

- A final streamed response line that arrives without a trailing newline is no
  longer dropped, so an answer or tool call is not lost when the server closes
  the connection immediately after it.
- Reasoning that resumes after an answer is labelled again with `think:`, and a
  flushed reasoning block no longer leaves a stray terminal reset at the end of
  the response.
- Tool command output terminates stdout and stderr separately, so stderr-only
  output ends on its own line and the two streams cannot run together.

### Changed

- Tool-call abort bookkeeping moved into pure `Hutian.Conversation`, so
  `Hutian.Llm.Loop` no longer depends on the IO executor for it.
- The README project layout now lists every source module and `nix/sandbox.nix`.

## [0.7.0.0] - 2026-11-18

### Added

- `lib.mkApp` builds a single-system Hutian app, so an existing flake can add
  `apps.<system>.hutian` without replacing its own default app.

### Changed

- `lib.mkPackage` and `lib.mkApp` now take `pkgs` as a separate first argument,
  and `sandbox.packages` is a `pkgs` selector for both builders. This lets one
  configuration be shared by `mkPackage`, `mkApp`, and `mkFlake`.
- `lib.mkFlake` now exposes `apps.<system>.hutian` and
  `packages.<system>.hutian` instead of `default`. Run dedicated Hutian flakes
  with `nix run .#hutian`.
- Renamed the public helpers from `mkHutianShellFlake`, `mkHutianShell`, and
  `mkHutianApp` to `mkFlake`, `mkPackage`, and `mkApp` under the `hutian.lib`
  namespace.
- `model.apiKeyCommand` is now required. Set it to `null` when no key is used.
- Removed `model.apiKeyFile`. Use `model.apiKeyCommand = "cat /path"` to
  read a key from a file.
- LiquidHaskell verifies the pure shell and relay modules as well, with tighter
  refinements for message counts, editor layout, job output bounds, and relay
  frame lengths. The check disables DEFAULT case expansion, so unexpanded
  matches no longer emit warnings.

## [0.6.0.0] - 2026-11-17

### Changed

- Prompt state now separates input ownership (`LlmInput`/`TermInput`) from
  render-only prompt and output kinds, so mode confusion is a type error rather
  than a runtime convention.
- `Hutian.Output` was folded into `Hutian.Render`, and `Hutian.Syntax` into
  `Hutian.Highlight`. Prompt-to-ANSI rendering, output labels, and pure approval
  decisions now share one module.
- `Hutian.Tool.Exec` owns terminal-side tool execution, keeping `Hutian.Tool`
  and its plans pure.
- `Hutian.Command` now only classifies submitted term-mode text; the unused
  command parser and its shell-builtin completion entries were removed.
- Job status is explicit: `JobRunning`, `JobExited`, or `JobSignalled`, instead
  of a negative `ExitFailure` used to represent signals.
- Job output line counts are clamped into a bounded `LineCount`, and jobs are
  stored by `JobId`.
- `Hutian.Llm` writes through a small `LlmOutput` sink instead of importing
  `Hutian.Console`, so the LLM layer no longer depends on the terminal.
- Model URLs are validated for scheme, host, and path when settings are parsed,
  and the API path matches `/v1/chat/completions` exactly after stripping any
  query or fragment.
- Relay authentication is a sum type, `RelayAuthNone` or
  `RelayAuthBearer key`, with a redacted `Show` instance. The relay socket is
  restricted to `0600` before it starts listening.
- The package builds a `hutian-shell` library shared by the shell and relay
  executables, and `Hutian.Llm.Loop` owns the model/tool exchange with its
  executor and continuation injected.
- LLM requests are split into `Hutian.Llm.Settings`, `Request`, `Stream`, and
  `Presentation`, and the interactive prompt loop moved to `Hutian.Session`, so
  prompting, history, and input execution are explicit effects.

### Fixed

- Interrupting a stream drops partial tool calls and leaves the conversation
  valid, and a request blocked on the network is stopped instead of left
  running.
- Typed-ahead input buffered during an approval prompt can no longer approve a
  model command, and control characters in command, job, and tool output are
  neutralized before rendering.
- The test suite defers its report and runs one test at a time, so Tasty output
  and other tests' writes cannot leak into process-wide stdout/stderr captures.
- Denied shell policy is refused before tool arguments are inspected.
- Relay sockets and job logs are no longer exposed through permissive file
  modes.
- `sandbox.nixDaemon = true` now supplies a single local flake registry instead
  of downloading the global one or exposing the host's registries. Its only
  entry pins `nixpkgs` to the nixpkgs input of the flake that configured
  Hutian. This makes `nix run nixpkgs#...` and locked local builds work offline
  when their store paths are present.
- The sandbox now disables the gVisor dentry cache in every mode. Store paths
  built or substituted by the host daemon after startup stay visible even if
  the sandbox has already listed `/nix/store`, and files created or changed on
  the host appear in `/source`.
- The interactive shell no longer keeps its own working directory inside
  `/source`. gVisor pins a directory listing while a process is in it, so the
  shell now enters `/source` once per command instead of for the whole session.

### Removed

- Stale `help`, `jobs`, and `job` shell completion entries.
- The `PromptMode`, `ToolMode`, and `SyntaxSpec` types and modules superseded by
  the refactor above.

## [0.5.0.0] - 2026-11-15

### Added

- A shared strict command-line parser in `Hutian.Cli` for `hutian-shell` and
  `hutian-llm-relay`. Unknown or bare arguments, missing values, and repeated
  options are now rejected with a usage error instead of being ignored.
- A pure `Hutian.Tool.planTool` step that turns the configured shell policy and
  a model tool call into run, approve, or refuse. Main performs only the IO that
  the plan requests, so policy decisions are covered without a terminal.
- A small first-class background-job API. The model gets `shell_background`,
  `jobs`, `job_output`, and `job_kill`. Jobs run in a new process
  group with stdin closed, and stdout and stderr are appended to
  `/tmp/hutian-jobs/<id>.log`. Up to eight run at once; finished jobs stay
  listed until the shell exits, and exiting stops the rest. Model job tools use
  the same `tool <name>:` and `tool output:` layout as the foreground shell
  tool, with gray results. Completion is not announced; `jobs` and `job_output`
  report the current status on request.
  `model.shell = "deny"` refuses every job tool; `"ask"` approves commands but
  not job inspection.
- Tests for the pure job model, job-tool planning, and the runtime's output
  capture, bounded tails, and process-group stop.
- Tests for strict option parsing, tool-call planning, socket framing helpers,
  and LLM error responses.
- Shell syntax highlighting with Skylighting for term-mode input and
  model-requested `tool shell:` commands.
- Mandatory `sandbox` and `model` option groups. `sandbox.packages` selects the
  sandbox's package closures. `sandbox.network = false`
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
- A capability matrix in the README documenting how `sandbox.writable`,
  `sandbox.nixDaemon`, `sandbox.network`, `model.shell`, and the relay compose,
  including the fact that `sandbox.network = false` covers direct sandbox
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

- The built-in defaults are now safe-by-default: `sandbox.network = false`,
  `sandbox.writable = false`, `model.shell = "ask"`, and the system prompt is
  `You are a coding agent.`.
- `sandbox.readOnly` was renamed to `sandbox.writable`, so every sandbox flag
  now grants a capability when it is `true`. `writable = true` allows writes to
  `/source`; the default `false` mounts it read-only.
- `sandbox.internet` was renamed to `sandbox.network`, and `sandbox.nix` was
  renamed to `sandbox.nixDaemon`, so each name describes the capability it
  grants. Both still default to `false`.
- `sandbox.network = true` now selects gVisor's `host` network mode explicitly.
  Rootless gVisor does not support `sandbox` networking and silently falls back
  to `host`, so the launcher no longer requests the unsupported mode.
  `GVISOR_NETWORK` can still narrow the mode to `none`.
- Direct TCP LLM requests no longer follow redirects, and non-success responses
  are reported the same way whether they arrive directly or through the relay.
  Error bodies and relay keys are read with explicit size limits.
- Sandbox and model settings are now grouped. `sandbox` contains `packages`,
  `writable`, `nixDaemon`, and `network`; `model` contains `name`, `url`,
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
- The logical sandbox URL now preserves only the upstream path. Query and
  fragment parameters from `model.url` stay on the host.
- The relay directory is bind-mounted read-only. Sandboxed commands can still
  connect to the relay socket, but cannot unlink or replace it.
- `model.name` and `model.systemPrompt` are written to short-lived files that
  the shell reads and removes before accepting input, so they no longer appear
  in the sandbox process listing.
- The sandboxed shell binary is explicitly kept stripped, and a check fails if a
  symbol or debug section reappears.
- When a key is configured, the relay reads it from standard input. The
  launcher pipes either `model.apiKeyFile` or `model.apiKeyCommand` into it, so
  the key never appears in the relay's argument list or environment.
- The relay now uses a private body/status protocol instead of HTTP/1.1. The
  sandbox sends only a request body; the host relay builds the single fixed
  upstream request, so there is no client-supplied request line or header block
  to parse, validate, strip, or redirect. The response is a four-byte status
  code followed by the upstream body.
- The sandbox network now follows the mandatory `sandbox.network` option
  instead of the presence of an API key. With `sandbox.network = true`,
  `GVISOR_NETWORK` may still narrow the gVisor mode; with
  `sandbox.network = false`, networking is always `none`.
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

### Removed

- The term-mode `help`, `jobs`, `job`, `job start`, and `job kill` builtins.
  Background jobs are controlled only through the model's job tools.

### Fixed

- Background jobs that outlive the shell that started them are no longer left as
  zombies for the rest of the session. A static `dumb-init` now runs as PID 1 in
  the sandbox and reaps orphaned processes, while the user shell runs as its
  child. It uses single-child mode because its default `setsid` mode detaches
  from the controlling terminal that gVisor does not reattach, which broke
  interactive programs such as `emacs` with `Could not open file: /dev/tty`.
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
