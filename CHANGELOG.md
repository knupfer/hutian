# Changelog

All notable changes to Hutian are documented here. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

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
