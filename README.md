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
mode, where the input is preceded by a bold pink `ask:` line. Press `Ctrl+X` to
switch to term mode, where the input has a bold pink `shell:` line above it and
submitted text runs through BusyBox `ash`, so shell builtins and redirection
work as expected.

In LLM mode the model can also call the `shell` tool. Commands requested by the
model run through the same `/bin/sh -c` execution path as term mode, and are
shown with a bold teal `tool shell:` line. Shell output is preceded by a
dimmed pink `output:` line and tool output by a dimmed teal `tool output:` line.

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
- `Ctrl+D` on an empty input exits the program in either mode. In llm mode it
  also submits non-empty input.
- `Ctrl+C` clears the current input and keeps editing.

### LLM mode

LLM mode is the initial mode. Input is preceded by a bold pink `ask:` line and
is syntax-highlighted as Markdown while you type, including headings, emphasis,
links, lists, and fenced code blocks. Replies use the same highlighting.
Fenced blocks tagged with a language use that language's syntax highlighter.
Reasoning/thinking tokens are shown in grey under a dimmed teal `think:` line,
with dimmed Markdown and code highlighting, and the reply is shown under a bold
teal `answer:` line.

Every LLM request includes the current session's prior conversation: earlier
LLM prompts, assistant replies, commands submitted in term mode, and tool
calls with their results.

The model can request shell commands through the `shell` tool. Hutian runs each
command with `/bin/sh -c` in the sandbox working directory, streams its output
to the terminal in gray under a dimmed teal `tool output:` line, and sends the
output back to the model. Every call runs in a fresh shell, so `cd`, exported
variables, and other shell state do not persist between calls. The requested
command is shown under a bold teal `tool shell:` line, distinguishing it from
commands typed by the user.

```console
Explain what a syscall is.
<Ctrl+D>
```

### Term mode

Term mode shows a bold pink `shell:` line above the input and prints command
output in gray under a dimmed pink `output:` line. `help` shows the builtin
help; every other line is executed with `/bin/sh -c` in the sandbox working
directory.

Each term-mode command runs in a fresh shell, so `cd`, exported variables, and
other shell state do not carry over between commands. Use absolute paths or
combine steps in one command, for example `cd sub && make`.

`Ctrl+C` cancels the current input or command without terminating the shell.
Term command history is stored in `/tmp/hutian-shell-history`, and LLM prompt
history in `/tmp/hutian-llm-history`.

The flake requires explicit `packages`, `systemPrompt`, `model`, `llamaUrl`,
`readOnly`, and `nix` options. Hutian expects an OpenAI-compatible llama.cpp
chat endpoint; a native `/completion` URL is also supported. Both `http://` and
`https://` URLs work, and HTTPS endpoints are verified against the CA bundle
from nixpkgs:

```text
https://llama.example.test/v1/chat/completions
```

The launcher passes `llamaUrl`, `model`, and `systemPrompt` to the shell as
`--llama-url`, `--model`, and `--system-prompt` arguments. The shell reads no
Hutian-specific environment variables. The sandbox receives the CA bundle path
as `SYSTEM_CERTIFICATE_PATH` so the Haskell HTTP client can verify TLS
certificates.

The sandbox uses gVisor's `sandbox` network mode by default, which can reach a
server bound to the host's loopback interface. When an API key is configured,
the default becomes `none` instead; see [API keys](#api-keys).

## API keys

Hutian can keep an API key on the host and give the sandbox only a private
relay socket. Pass `apiKeyFile` or `apiKeyCommand` to `mkHutianShellFlake` or
`mkHutianShell`:

```nix
hutian.lib.mkHutianShellFlake nixpkgs {
  # ...
  apiKeyFile = "/run/secrets/hutian-key";
}
```

The launcher starts `hutian-llm-relay` on the host. The relay reads the key from
standard input; the launcher pipes either `apiKeyFile` or the standard output of
`apiKeyCommand` into it. The relay listens on a per-session Unix socket under
`$XDG_RUNTIME_DIR` (falling back to `/tmp`). The socket directory is mode `0700`
and is bind-mounted at `/run/hutian`, so the sandbox can connect to
`/run/hutian/llm.sock`; the directory holds no key material.

The relay accepts only an HTTP/1.1 `POST` to the exact path and query of
`llamaUrl`. It discards all client headers, including `Authorization` and
`Cookie`, and adds its own `Authorization: Bearer <key>` before forwarding the
body to the fixed upstream. Response bodies, including streaming SSE, are
relayed back. The relay does not follow upstream redirects, so a response
cannot move the key to another host. The sandbox therefore cannot redirect the
key, cannot use the relay as a general proxy, and never receives the key in
argv, environment variables, files, or the Nix store.

The shell is launched with `--llama-socket /run/hutian/llm.sock` and the
logical URL `http://hutian-relay/...`, which preserves the chat or completion
API shape without exposing the real upstream host or any credentials embedded
in `llamaUrl`.

When a key is configured, `GVISOR_NETWORK` defaults to `none`. Set it to
`sandbox` or `host` to restore networking for other tools. `apiKeyFile` and
`apiKeyCommand` are mutually exclusive. `apiKeyFile` must not point into the
Nix store or the host working directory, both of which the sandbox can read;
the launcher rejects such paths at startup. Keep `apiKeyCommand` as a command
that fetches the key, for example from `pass` or `sops`, rather than embedding
the key in the command string, because option values live in the Nix store.

For example, with `pass`:

```nix
apiKeyCommand = "pass show llama/api-key";
```

## Adding packages

`lib.mkHutianShellFlake` builds Hutian for all supported systems from a single
package selector. Each selected package's runtime closure is mounted under
`/nix/store`, and its `bin` directory is added to `PATH`. Closures of any size
are supported; Hutian bind-mounts the closure into the root filesystem instead
of passing one sandbox volume per store path.

The `systemPrompt`, `model`, `llamaUrl`, `readOnly`, and `nix` arguments are
all required and configure the system message, model name, llama.cpp endpoint,
whether the host working directory is writable, and whether Nix is available:

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
      nix = false;
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
flake's lock file. Every option is mandatory, and omitting one fails with an
error that lists the missing options.

For a single system or an existing `pkgs` instance, use the lower-level builder:

```nix
hutian.lib.mkHutianShell {
  inherit pkgs;
  packages = [ pkgs.git pkgs.jq ];
  model = "your-model";
  llamaUrl = "http://127.0.0.1:8080/v1/chat/completions";
  readOnly = false;
  nix = false;
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
in full while `nix = true`. Set it to `false` when you only need the configured
package closures.

## Configuration

The sandbox receives a fixed `TERM=xterm-256color` and `LANG=C.UTF-8`. Only the
gVisor runtime settings can be overridden from the host:

| Variable | Default | Description |
| --- | --- | --- |
| `GVISOR_PLATFORM` | `systrap` | gVisor platform. Use `ptrace` if your host requires it. |
| `GVISOR_NETWORK` | `sandbox` | gVisor network mode: `sandbox`, `host`, or `none`. |

## Security model

- `runsc --rootless` runs the sandbox as your unprivileged host user inside a
  user namespace. The sandbox process is UID 0 inside that namespace, mapped to
  your host UID; it gains no host privileges.
- With the default `sandbox` network mode, the sandbox can reach the host's
  loopback interface. Set `GVISOR_NETWORK=none` to disable networking. When an
  API key is configured, the default is already `none` and model traffic uses
  the host relay socket instead.
- With `apiKeyFile` or `apiKeyCommand`, the key stays on the host. The sandbox
  gets only the relay socket at `/run/hutian/llm.sock`, which accepts a single
  fixed request target and cannot be used as a general authenticated proxy.
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
- `Hutian.Editor` owns prompt state, key semantics, and layout.
- `Hutian.Prompt` is the pure prompt reducer: it turns a key and the current
  state into a step such as redraw, move cursor, complete, or finish.
- `Hutian.Completion` selects command and path completions.
- `Hutian.Ansi` represents RGB colours and text styles as data and encodes them
  with `ansi-terminal`.
- `Hutian.Render` turns a prompt into ANSI output and cursor movement, including
  the tool-call prefix style.
- `Hutian.History`, `Hutian.Conversation`, and `Hutian.Paths` are pure models;
  `Hutian.Conversation` also represents tool calls and tool results.
- `Hutian.History` owns the pure on-disk history codec.
- `Hutian.Markdown` is a pure streaming renderer for Markdown and ANSI styles.
- `Hutian.Llm` keeps SSE parsing, tool-call accumulation, request building, and
  command-line settings parsing pure, with IO limited to HTTP.
- `Hutian.Http.Wire` contains the shared socket framing helpers used by both
  the relay and the sandbox-side client.
- `Hutian.Llm.UnixSocket` is the sandbox-side HTTP/1.1 client for the host
  relay socket.
- `Hutian.Relay` is the host-side relay: it owns the key, validates the single
  allowed request, injects authorization, and streams the response.
  `Hutian.Relay.Http` holds the pure wire parsing and key-redaction helpers.
- `Hutian.Console` is the single terminal IO boundary: writes, terminal
  attributes, cursor styling, size, and stream colouring.
- `Hutian.Terminal` is the prompt driver over `Hutian.Prompt` and
  `Hutian.Console`.
- `Hutian.Shell` returns captured command output as `CommandResult` data instead
  of printing it.
- `Hutian.History.Store` and `Hutian.Interrupt` are the file and signal
  boundaries.
- `Main` wires the boundaries together and drives the tool-call loop.

The package uses `GHC2021` and compiles with strict warnings and `-Werror`.

## Project layout

```text
flake.nix                 Flake outputs and consumer helpers
nix/hutian.nix            Sandbox rootfs, launcher, and wrapper
nix/hutian-shell.nix      Haskell package definition
nix/lint.nix              Shared lint and LiquidHaskell checks
app/                      Haskell shell source
app/Hutian/Ansi.hs        Typed terminal colours and styles
app/Hutian/Command.hs     Typed term-mode command parsing
app/Hutian/Completion.hs  Pure completion selection
app/Hutian/Console.hs     Single terminal IO boundary
app/Hutian/Prompt.hs      Pure prompt reducer
app/Hutian/Render.hs      Pure ANSI prompt rendering
app/Hutian/Relay.hs       Host-side LLM relay
app/Hutian/Llm/UnixSocket.hs  Relay client over a Unix socket
app/RelayMain.hs          Relay executable entry point
test/                     Unit and property tests
test/TestSupport.hs       Shared test helpers
hutian-shell.cabal        Haskell package manifest
example.nix               Minimal consumer flake
LICENSE                   MIT license
CHANGELOG.md              Release notes
```

## Testing

The test suite combines HUnit tests with QuickCheck properties for the editor
state machine, the pure prompt reducer, prompt layout and rendering, command
parsing, completion selection, history codecs, conversation history, LLM
requests, streaming tool calls, Markdown rendering, code highlighting, path and
shell helpers, and interrupt handling. It also includes integration tests for:

- streaming LLM responses through a local HTTP server
- streaming LLM responses through the host relay over a Unix socket, including
  key injection, header stripping, target rejection, and response streaming
- history file persistence
- shell command execution and command discovery

Run it through Nix:

```console
$ nix build .#packages.x86_64-linux.default
```

The package build runs the suite as part of the standard build. With a local
Cabal setup, `cabal test` runs the same tests directly.

The flake also exposes a check that verifies the Nix options are passed to the
launcher as command-line arguments:

```console
$ nix build .#checks.x86_64-linux.configuration
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
`Hutian.Conversation`, `Hutian.Editor`, `Hutian.History`, and `Hutian.Prompt`
modules:

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
