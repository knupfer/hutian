#!/usr/bin/env bash
#
# Measure the real launcher and interactive shell.
#
# The binary is built first so Nix evaluation and compilation are excluded from
# the measurements. This script needs gVisor and unprivileged user namespaces,
# because every measured run starts the real sandbox.
#
#   * hyperfine times launcher + sandbox startup and one-shot commands;
#   * expect drives the interactive TUI through a pty and reports
#     startup-to-prompt and keystroke-echo latency.
set -euo pipefail

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd "$script_dir/.." && pwd)
script_path="$script_dir/$(basename "${BASH_SOURCE[0]}")"
cd "$repo_root"

if ! command -v hyperfine >/dev/null 2>&1; then
  if command -v nix >/dev/null 2>&1; then
    exec nix shell nixpkgs#hyperfine --command "$script_path" "$@"
  fi
  echo "hyperfine is required; install it or run this script through Nix" >&2
  exit 1
fi

echo "Building .#hutian ..."
nix build .#hutian

binary="$repo_root/result/bin/hutian"

if ! "$binary" true >/dev/null 2>&1; then
  echo "hutian could not run; check gVisor and unprivileged user namespaces" >&2
  exit 1
fi

echo
echo "Launcher startup and one-shot commands (real gVisor sandbox)."
echo "A one-shot command bypasses hutian-shell, so this excludes its startup."
hyperfine \
  --warmup 3 \
  --command-name 'startup' "$binary true" \
  --command-name 'echo hello' "$binary echo hello" \
  --command-name 'shell redirect' "$binary /bin/sh -c 'echo hello >/dev/null'"

echo
echo "Interactive TUI through a pty ..."
tui_script="$repo_root/scripts/tui-bench.exp"
tui_runs=${HUTIAN_TUI_RUNS:-3}
if command -v expect >/dev/null 2>&1; then
  expect "$tui_script" "$binary" "$tui_runs"
elif command -v nix >/dev/null 2>&1; then
  nix shell nixpkgs#expect --command expect "$tui_script" "$binary" "$tui_runs"
else
  echo "expect is required for the pty measurement; skipping" >&2
fi

echo
echo "For the pure and mock-model benchmarks, run:"
echo "  nix build .#checks.<system>.benchmark --print-build-logs"
echo "or, in a Cabal environment:"
echo "  cabal bench"
