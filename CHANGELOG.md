# Changelog

All notable changes to Hutian are documented here. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Added

- LLM-mode input is highlighted as Markdown while typing, including inline
  emphasis, links, lists, and fenced code blocks.
- LLM mode shows a bold pink `ask:` line before the input, and its replies are
  labelled with bold teal `think:` and `answer:` lines.
- Shell output is preceded by an `output:` line and tool output by a
  `tool output:` line, each in a light version of the command's role colour.

### Changed

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

- LLM-mode input redraws as a single frame, so highlighted input no longer
  flickers while typing.

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
