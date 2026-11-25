#!/usr/bin/env bash
#
# Measure the real launcher and interactive shell through a pty.
#
# The binary is built first so Nix evaluation and compilation are excluded from
# the measurements. This script needs gVisor and unprivileged user namespaces,
# because every measured run starts the real sandbox.
#
# The Expect script reports startup-to-prompt and keystroke-echo latency.
set -euo pipefail

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd "$script_dir/.." && pwd)

cd "$repo_root"

echo "Building .#hutian ..."
nix build .#hutian

binary="$repo_root/result/bin/hutian"

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
