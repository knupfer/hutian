# Hutian

Hutian is a Nix flake that drops you into a rootless
[gVisor](https://gvisor.dev/) sandbox with a small Haskell shell.

The sandbox sees only:

- the current working directory, mounted at `/source` read-write by default or
  read-only when `readOnly = true`;
- the runtime closures of the packages you configure, mounted under
  `/nix/store`;
- optionally, when `nix = true`, the host Nix store and Nix daemon socket.

Everything else on the host is hidden. The root filesystem is built from Nix's
static BusyBox sandbox shell plus the Haskell `hutian-shell` binary.

The interactive prompt is a multiline editor with two modes. It starts in LLM
mode with a dark magenta background. Press `Ctrl+X` to switch to term mode,
where the input lines have a dark blue background and submitted text runs
through BusyBox `ash`, so shell builtins and redirection work as expected.

In LLM mode the model can also call a shell tool. Commands requested by the
model run through the same `/bin/sh -c` execution path as term mode, and the
input lines used to show them have a dark red background with a bright red
path.

## Quick start

```console
$ cd /some/project
$ nix run /path/to/this/flake
<Ctrl+X>          # switch from llm mode to term mode
pwd
<Enter>
/source
```

Run a single command without starting the interactive shell:

```console
$ nix run /path/to/this/flake -- echo hello
$ nix run /path/to/this/flake -- /bin/sh -c 'echo hello > result.txt'
```

Files written under `/source` appear directly in the host working directory
unless `readOnly = true`. `/tmp` is private to the sandbox, and the sandbox root
is read-only.

## Input modes

The prompt starts in llm mode. `Ctrl+X` switches between llm mode and term
mode. Each mode keeps its own input buffer: switching saves the current text
and restores the text of the mode being entered. Submitted text is sent to
llama.cpp in llm mode and executed with `/bin/sh -c` in term mode.

The common editing keys are:

- `Enter` starts a new line in llm mode and executes the command in term mode.
  In term mode, a trailing `\` or an unclosed single/double quote continues the
  command on the next line, just like bash.
- `Up`/`Down` recall previous input history. In llm mode they first move
  through the current lines, then browse earlier prompts at the first or last
  line.
- `Backspace` and `Delete` edit the text at the cursor; deleting a newline
  joins the two lines.
- The arrow keys move the cursor within the message, including between lines,
  and `Home`/`End` jump to the start or end of the current line.
- `Tab` completes command names from `PATH` and filesystem paths in term
  mode. Ambiguous matches are listed below the input.
- `Ctrl+D` submits the accumulated text in llm mode. In term mode, use `Enter`
  to run the command.
- `Ctrl+C` clears the current input and keeps editing.

### LLM mode

LLM mode uses a dark magenta background. This is the initial mode. Replies are
syntax-highlighted as Markdown, including headings, emphasis, links, lists, and
fenced code blocks. Fenced blocks tagged with a language use that language's
syntax highlighter. Reasoning/thinking tokens are shown in grey with the same
Markdown highlighting.

Every LLM request includes the current session's prior conversation: earlier
LLM prompts, assistant replies, commands submitted in term mode, and tool
calls with their results.

The model can request shell commands through the `run_shell_command` tool.
Hutian runs each command with `/bin/sh -c` in the current directory, streams
its output to the terminal in gray, and sends the output back to the model. The
requested command is shown on input lines with a dark red background and a
bright red path, distinguishing it from commands typed by the user.

```console
Explain what a syscall is.
<Ctrl+D>
```

### Term mode

Term mode uses a dark blue background, shows the current path in vibrant teal
before the input, and prints command output in gray. It provides these builtins:

| Command | Description |
| --- | --- |
| `cd [dir]` | Change directory, defaulting to `$HOME`. |
| `pwd` | Print the current directory. |
| `llm [prompt]` | Send a one-off prompt to llama.cpp and stream the response. |
| `help` | Show the builtin help. |
| `exit`, `quit` | Leave the shell. |

Everything else is executed with `/bin/sh -c` inside the sandbox. `Ctrl+C`
cancels the current input, command, or LLM request without terminating the
shell. Term command history is stored in `/tmp/hutian-shell-history`, and LLM
prompt history in `/tmp/hutian-llm-history`.

The flake requires explicit `llamaUrl` and `readOnly` options. Hutian expects
an OpenAI-compatible llama.cpp chat endpoint; a native `/completion` URL is
also supported.

```text
http://127.0.0.1:8080/v1/chat/completions
```

Configure it with:

| Variable | Default | Description |
| --- | --- | --- |
| `HUTIAN_LLAMA_URL` | set by `llamaUrl` | llama.cpp endpoint. A native `/completion` URL is also supported. |
| `HUTIAN_LLAMA_MODEL` | set by `model` | Model name sent to llama.cpp. |
| `HUTIAN_SYSTEM_PROMPT` | set by `systemPrompt` | System message sent to llama.cpp. |

The sandbox uses gVisor's `sandbox` network mode by default, which can reach a
server bound to the host's loopback interface.

## Adding packages

`lib.mkHutianShellFlake` builds Hutian for all supported systems from a single
package selector. Each selected package's runtime closure is mounted under
`/nix/store`, and its `bin` directory is added to `PATH`.

The required `systemPrompt`, `model`, `llamaUrl`, and `readOnly` arguments
configure the system message, model name, llama.cpp endpoint, and whether the
host working directory is writable:

```nix
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.hutian.url = "path:/path/to/this/flake";

  outputs = { nixpkgs, hutian, ... }:
    hutian.lib.mkHutianShellFlake nixpkgs {
      packages = pkgs: with pkgs; [ coreutils git ];
      model = "your-model";
      llamaUrl = "http://127.0.0.1:8080/v1/chat/completions";
      readOnly = false;
      systemPrompt = ''
        You are a careful coding assistant working in a sandboxed shell.
      '';
    };
}
```

Set `readOnly = true` to mount `/source` read-only. The sandbox can still read
the working directory, but any attempt to write to it fails with a read-only
filesystem error. This is useful when the model should only inspect a project.

Then run `nix run .` from that project. Package versions come from the consumer
flake's lock file. Omitting a required argument fails with an error that names
the missing option and shows an example value.

To use a custom package set, such as one with overlays, pass `pkgsFor`:

```nix
pkgsFor = system:
  nixpkgs.legacyPackages.${system}.extend (final: prev: {
    # your overlay
  });
```

`pkgsFor` defaults to `system: nixpkgs.legacyPackages.${system}`.

For a single system or an existing `pkgs` instance, use the lower-level builder:

```nix
hutian.lib.mkHutianShell {
  inherit pkgs;
  packages = [ pkgs.git pkgs.jq ];
  model = "your-model";
  llamaUrl = "http://127.0.0.1:8080/v1/chat/completions";
  readOnly = false;
  systemPrompt = "You are a careful coding assistant working in a sandboxed shell.";
}
```

A complete consumer example is available in [`example.nix`](./example.nix).

## Using Nix inside the sandbox

Pass `nix = true` to include Nix and connect it to the host's Nix daemon:

```nix
outputs = { nixpkgs, hutian, ... }:
  hutian.lib.mkHutianShellFlake nixpkgs {
    packages = pkgs: with pkgs; [ coreutils git ];
    model = "your-model";
    llamaUrl = "http://127.0.0.1:8080/v1/chat/completions";
    readOnly = false;
    systemPrompt = "You are a careful coding assistant working in a sandboxed shell.";
    nix = true;
  };
```

The lower-level `lib.mkHutianShell` builder accepts the same option.

In this mode the host's Nix store and Nix daemon socket are exposed to the
sandbox. Commands such as `nix build`, `nix shell`, `nix run`, and
`nix develop` act as clients of the host daemon, so builds and substitutions run
on the host. Flakes and the `nix` command are enabled automatically, and a CA
bundle is provided for client-side HTTPS fetches.

The sandbox still uses gVisor's network mode, but the host Nix store is visible
in full while `nix = true`. Leave this option off when you only need the
configured package closures.

## Configuration

| Variable | Default | Description |
| --- | --- | --- |
| `GVISOR_PLATFORM` | `systrap` | gVisor platform. Use `ptrace` if your host requires it. |
| `GVISOR_NETWORK` | `sandbox` | gVisor network mode: `sandbox`, `host`, or `none`. |
| `HUTIAN_LLAMA_URL` | set by `llamaUrl` | llama.cpp endpoint. |
| `HUTIAN_LLAMA_MODEL` | set by `model` | llama.cpp model name. |
| `HUTIAN_SYSTEM_PROMPT` | set by `systemPrompt` | System message sent to llama.cpp. |

## Security model

- `runsc --rootless` runs the sandbox as your unprivileged host user inside a
  user namespace. The sandbox process is UID 0 inside that namespace, mapped to
  your host UID; it gains no host privileges.
- With the default `sandbox` network mode, the sandbox can reach the host's
  loopback interface. Set `GVISOR_NETWORK=none` to disable networking.
- Writes outside `/source`, `/tmp`, and the mounted package store are refused.
  Set `readOnly = true` to refuse writes to `/source` as well.
- With `nix = true`, the full host `/nix/store` and the Nix daemon socket are
  visible. Other host paths remain hidden.
- The sandbox uses the `systrap` platform by default. Set
  `GVISOR_PLATFORM=ptrace` if your host needs the `ptrace` platform.

## Architecture

The code keeps pure logic separate from IO:

- `Hutian.Command` parses term-mode input into a typed `Command` and classifies
  single-line versus multi-line input.
- `Hutian.Editor` owns prompt state, key semantics, layout, and the pure
  `PromptAction` routing used by the terminal loop.
- `Hutian.Completion` selects command and path completions.
- `Hutian.Render` turns a prompt into ANSI output and cursor movement, including
  the dark red tool-call style.
- `Hutian.History`, `Hutian.Conversation`, and `Hutian.Paths` are pure models;
  `Hutian.Conversation` also represents tool calls and tool results.
- `Hutian.History` owns the pure on-disk history codec.
- `Hutian.Markdown` is a pure streaming renderer for Markdown and ANSI styles.
- `Hutian.Llm` keeps SSE parsing, tool-call accumulation, and request building
  pure, with IO limited to HTTP and terminal output.
- `Hutian.Terminal`, `Hutian.Shell`, and `Hutian.Interrupt` are thin IO adapters
  over those pure cores.
- `Main` is a small interpreter over `Command`, `TerminalInput`, and
  `ShellState`, and drives the tool-call loop.

The package uses `GHC2021` and compiles with strict warnings and `-Werror`.

## Project layout

```text
flake.nix                 Flake outputs and consumer helpers
nix/hutian.nix            Sandbox rootfs, launcher, and wrapper
nix/hutian-shell.nix      Haskell package definition
nix/lint.nix              Shared lint and LiquidHaskell checks
app/                      Haskell shell source
app/Hutian/Command.hs     Typed term-mode command parsing
app/Hutian/Completion.hs  Pure completion selection
app/Hutian/Render.hs      Pure ANSI prompt rendering
test/                     Unit and property tests
test/TestSupport.hs       Shared test helpers
hutian-shell.cabal        Haskell package manifest
example.nix               Minimal consumer flake
LICENSE                   MIT license
CHANGELOG.md              Release notes
```

## Testing

The test suite combines HUnit tests with QuickCheck properties for the editor
state machine, prompt layout and rendering, command parsing, completion
selection, history codecs, conversation history, LLM requests, streaming
tool calls, Markdown rendering, code highlighting, path and shell helpers, and
interrupt handling. It also includes integration tests for:

- streaming LLM responses through a local HTTP server
- history file persistence
- shell command execution and command discovery

Run it through Nix:

```console
$ nix build .#packages.x86_64-linux.default
```

The package build runs the suite as part of the standard build. With a local
Cabal setup, `cabal test` runs the same tests directly.

The flake also exposes a check that verifies the required Nix options are
propagated:

```console
$ nix build .#checks.x86_64-linux.required-options
```

## Linting

Haskell and Nix lint checks are available through the flake:

```console
$ nix build .#checks.x86_64-linux.haskell-lint
$ nix build .#checks.x86_64-linux.nix-lint
```

`haskell-lint` runs HLint over `app/` and `test/`. `nix-lint` runs Statix and
DeadNix over the flake and Nix files. These linters also run automatically in
the package check phase, so a normal `nix build` fails on lint errors.
`nix flake check` runs the explicit checks along with the package build.

LiquidHaskell verifies the pure `Hutian.Completion`, `Hutian.Paths`,
`Hutian.Conversation`, `Hutian.Editor`, and `Hutian.History` modules:

```console
$ nix build .#checks.x86_64-linux.liquid
```

This verification also runs automatically as a package-build dependency, so a
normal `nix build` fails if the LiquidHaskell check fails.

Format all Nix files with the configured `nixfmt-tree` formatter:

```console
$ nix fmt
```

## Requirements

- Linux with unprivileged user namespaces enabled.
- x86_64 or aarch64.

If user namespaces are disabled, `runsc` reports an error. On systems with the
Debian-style sysctl this is enabled with:

```console
$ sudo sysctl kernel.unprivileged_userns_clone=1
```
