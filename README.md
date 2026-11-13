# Hutian

Hutian is a Nix flake that drops you into a rootless
[gVisor](https://gvisor.dev/) sandbox with a small Haskell shell.

The sandbox sees only:

- the current working directory, mounted at `/source` read-write by default or
  read-only when `sandbox.readOnly = true`;
- the runtime closures of the packages you configure, mounted under
  `/nix/store`;
- optionally, when `sandbox.nix = true`, the host Nix store and Nix daemon
  socket.

Everything else on the host is hidden. The root filesystem is built from Nix's
static BusyBox sandbox shell plus the Haskell `hutian-shell` binary.

The interactive prompt is a multiline editor with two modes. It starts in LLM
mode, where the input is preceded by a bold pink `ask:` line. Press `Ctrl+X` to
switch to term mode, where the input has a bold pink `shell:` line above it and
submitted text runs through BusyBox `ash`, so shell builtins and redirection
work as expected.

The model's `shell` tool is controlled by the mandatory `model.shell` option,
which accepts `"deny"`, `"ask"`, or `"allow"`. Term mode is always available.
With `"deny"` the model is never told that a shell tool exists and any tool call
is refused. With `"ask"` the tool is advertised, but each requested command
needs your approval before it runs. With `"allow"` model commands run
automatically. In every case you can still press `Ctrl+X` and run commands
yourself.

In LLM mode the model can also call the `shell` tool according to
`model.shell`. Commands requested by the model run through the same `/bin/sh -c`
execution path as term mode, and are shown with a bold teal `tool shell:` line.
In `"ask"` mode Hutian additionally shows a `run? [y/N]` prompt before each
command. Shell output is preceded by a dimmed pink `output:` line and tool
output by a dimmed teal `tool output:` line.

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
unless `sandbox.readOnly = true`. `/tmp` is private to the sandbox, and the
sandbox root is read-only.

## Input modes

The prompt starts in llm mode. `Ctrl+X` always switches between llm mode and
term mode, regardless of `model.shell`. Each mode keeps its own input buffer:
switching saves the current text and restores the text of the mode being
entered. Submitted text is sent to llama.cpp in llm mode and executed with
`/bin/sh -c` in term mode.

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

When `model.shell = "allow"`, the model can request shell commands through the
`shell` tool. With `"ask"`, Hutian shows the command and waits for `y` or `Y`
before running it; any other key denies the call, and non-interactive runs deny
automatically. Hutian runs each command with `/bin/sh -c` in the sandbox working
directory, streams its output to the terminal in gray under a dimmed teal
`tool output:` line, and sends the output back to the model. Every call runs in
a fresh shell, so `cd`, exported variables, and other shell state do not persist
between calls. The requested command is shown under a bold teal `tool shell:`
line, distinguishing it from commands typed by the user.

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

The flake requires explicit `sandbox` and `model` groups. `sandbox` must
contain `packages`, `readOnly`, `nix`, and `internet`; `model` must contain
`name`, `url`, `systemPrompt`, and `shell`. Hutian expects an
OpenAI-compatible llama.cpp chat endpoint; a native `/completion` URL is also
supported. Both `http://` and `https://` URLs work, and HTTPS endpoints are
verified against the CA bundle from nixpkgs:

```text
https://llama.example.test/v1/chat/completions
```

The launcher passes `model.url`, `model.name`, and `model.systemPrompt` to the
shell as `--llama-url`, `--model`, and `--system-prompt` arguments. The shell
reads no Hutian-specific environment variables. The sandbox receives the CA
bundle path as `SYSTEM_CERTIFICATE_PATH` so the Haskell HTTP client can verify
TLS certificates.

Hutian always routes model traffic through the host relay, whether or not an
API key is configured. The sandbox uses gVisor's `sandbox` network mode by
default when `sandbox.internet = true`; with `sandbox.internet = false` it
always uses `none`, regardless of `GVISOR_NETWORK`. The relay itself uses a
Unix socket, so model traffic still works without sandbox networking. See
[Relay and API keys](#relay-and-api-keys).

## Relay and API keys

Hutian always routes model traffic through a host-side relay. The shell is
launched with `--llama-socket /run/hutian/llm.sock` and the logical URL
`http://hutian-relay/...`, which preserves the chat or completion API shape
without exposing the real upstream host or any credentials embedded in
`model.url`.

Without an API key the relay starts with `--no-auth` and forwards requests
without an `Authorization` header. This works with local llama.cpp servers that
do not require a key, and the sandbox still needs no direct network access to
the model endpoint. To keep a key on the host instead, pass `model.apiKeyFile`
or `model.apiKeyCommand` to `mkHutianShellFlake` or `mkHutianShell`:

```nix
hutian.lib.mkHutianShellFlake nixpkgs {
  # ...
  model.apiKeyFile = "/run/secrets/hutian-key";
}
```

The launcher starts `hutian-llm-relay` on the host. With a key configured, the
relay reads it from standard input; the launcher pipes either `model.apiKeyFile`
or the standard output of `model.apiKeyCommand` into it. `model.apiKeyCommand`
runs in the foreground before the sandbox starts, so commands that prompt for a
password (for example `passage`) work normally and can be cancelled with
`Ctrl+C`. The relay listens on a per-session Unix socket under
`$XDG_RUNTIME_DIR` (falling back to `/tmp`). The socket directory is mode `0700`
and is bind-mounted at `/run/hutian`, so the sandbox can connect to
`/run/hutian/llm.sock`; the directory holds no key material.

The relay uses a private framing protocol over the socket instead of HTTP. The
sandbox sends only the request body and closes its write half. The relay sends
that body to the fixed path and query of `model.url` with `Content-Type:
application/json`, `Accept: text/event-stream`, and, when a key is configured,
`Authorization: Bearer <key>`. It returns a four-byte status code followed by
the upstream body, including streaming SSE. Because the client never supplies
a request line or header block, the sandbox cannot choose the upstream host,
path, or headers, cannot redirect the key, and cannot use the relay as a
general authenticated proxy. The relay does not follow upstream redirects, and
the key never appears in argv, environment variables, files, or the Nix store.

`model.apiKeyFile` and `model.apiKeyCommand` are mutually exclusive.
`model.apiKeyFile` must not point into the Nix store or the host working
directory, both of which the sandbox can read; the launcher rejects such paths
at startup. Keep `model.apiKeyCommand` as a command that fetches the key, for
example from `pass` or `sops`, rather than embedding the key in the command
string, because option values live in the Nix store.

For example, with `pass`:

```nix
model.apiKeyCommand = "pass show llama/api-key";
```

## Adding packages

`lib.mkHutianShellFlake` builds Hutian for all supported systems from a single
package selector. Each selected package's runtime closure is mounted under
`/nix/store`, and its `bin` directory is added to `PATH`. Closures of any size
are supported; Hutian bind-mounts the closure into the root filesystem instead
of passing one sandbox volume per store path.

The `sandbox` and `model` groups are required. `sandbox` configures the
package closure, whether the host working directory is writable, whether Nix
is available, and whether the sandbox may use the network. `model` configures
the system message, model name, llama.cpp endpoint, and how the model's shell
tool is handled (`"deny"`, `"ask"`, or `"allow"`):

```nix
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.hutian.url = "path:/path/to/this/flake";

  outputs = { nixpkgs, hutian, ... }:
    hutian.lib.mkHutianShellFlake nixpkgs {
      sandbox = {
        packages = pkgs: with pkgs; [ coreutils git ];
        readOnly = false;
        nix = false;
        internet = true;
      };
      model = {
        name = "your-model";
        url = "http://127.0.0.1:8080/v1/chat/completions";
        systemPrompt = ''
          You are a careful coding assistant working in a sandboxed shell.
        '';
        shell = "allow";
      };
    };
}
```

Set `sandbox.readOnly = true` to mount `/source` read-only. The sandbox can
still read the working directory, but any attempt to write to it fails with a
read-only filesystem error. This is useful when the model should only inspect a
project.

Set `sandbox.internet = false` to give the sandbox gVisor's `none` network mode.
This is a hard gate: `GVISOR_NETWORK` cannot re-enable networking. The host
relay still provides model traffic over its Unix socket.

Set `model.shell = "deny"` to disable the model's `shell` tool. Term mode
remains available for your own commands. Hutian omits the tool from every LLM
request and also refuses to execute any tool call that arrives anyway. Use
`"ask"` to keep the tool available with per-command approval, or `"allow"` to
run model-requested commands automatically.

Then run `nix run .` from that project. Package versions come from the consumer
flake's lock file. Every option is mandatory, and omitting one fails with an
error that lists the missing option paths. Unknown options are rejected, so a
misspelled key such as `model.foo` cannot be ignored silently.

For a single system or an existing `pkgs` instance, use the lower-level builder:

```nix
hutian.lib.mkHutianShell {
  inherit pkgs;
  sandbox = {
    packages = [ pkgs.git pkgs.jq ];
    readOnly = false;
    nix = false;
    internet = true;
  };
  model = {
    name = "your-model";
    url = "http://127.0.0.1:8080/v1/chat/completions";
    systemPrompt = "You are a careful coding assistant working in a sandboxed shell.";
    shell = "allow";
  };
}
```

A complete consumer example is available in [`example.nix`](./example.nix).

## Using Nix inside the sandbox

Pass `sandbox.nix = true` to include Nix and connect it to the host's Nix daemon:

```nix
outputs = { nixpkgs, hutian, ... }:
  hutian.lib.mkHutianShellFlake nixpkgs {
    sandbox = {
      packages = pkgs: with pkgs; [ coreutils git ];
      readOnly = false;
      nix = true;
      internet = true;
    };
    model = {
      name = "your-model";
      url = "http://127.0.0.1:8080/v1/chat/completions";
      systemPrompt = "You are a careful coding assistant working in a sandboxed shell.";
      shell = "allow";
    };
  };
```

The lower-level `lib.mkHutianShell` builder accepts the same option.

In this mode the host's Nix store and Nix daemon socket are exposed to the
sandbox. Commands such as `nix build`, `nix shell`, `nix run`, and
`nix develop` act as clients of the host daemon, so builds and substitutions run
on the host. Flakes and the `nix` command are enabled automatically, and a CA
bundle is provided for client-side HTTPS fetches.

The sandbox still uses gVisor's network mode, but the host Nix store is visible
in full while `sandbox.nix = true`. Set it to `false` when you only need the
configured package closures.

## Configuration

The sandbox receives a fixed `TERM=xterm-256color` and `LANG=C.UTF-8`. Only the
gVisor runtime settings can be overridden from the host:

| Variable | Default | Description |
| --- | --- | --- |
| `GVISOR_PLATFORM` | `systrap` | gVisor platform. Use `ptrace` if your host requires it. |
| `GVISOR_NETWORK` | `sandbox` | gVisor network mode: `sandbox`, `host`, or `none`. Ignored when `sandbox.internet = false`, where the mode is always `none`. |

## Permission model

Hutian separates sandbox capabilities from model-tool permissions:

| Option | Applies to | Effect |
| --- | --- | --- |
| `sandbox.packages` | sandbox | Selects the package closures mounted under `/nix/store` and added to `PATH`. |
| `sandbox.readOnly` | sandbox | Mounts `/source` read-only. It protects integrity, not confidentiality. |
| `sandbox.nix` | sandbox | Exposes the host `/nix/store` and Nix daemon socket. |
| `sandbox.internet` | sandbox | Controls direct gVisor networking. `false` forces `none`. |
| `model.shell` | model | `"deny"`, `"ask"`, or `"allow"` for the model's `shell` tool. |
| `model.apiKeyFile`, `model.apiKeyCommand` | host relay | Supply the upstream key without exposing it to the sandbox. |

Important composition rules:

- `sandbox.internet = false` removes direct sandbox networking only. The relay
  socket can still reach the fixed model upstream, and `sandbox.nix = true`
  still lets the host Nix daemon fetch and build.
- `sandbox.readOnly = true` does not prevent the model from reading `/source`
  and sending what it reads upstream. Set `model.shell = "deny"` or use a local
  model endpoint if confidentiality matters.
- `sandbox.internet = true` defaults to gVisor's `sandbox` mode, which can
  reach the host loopback interface. `GVISOR_NETWORK` can narrow it but cannot
  widen `sandbox.internet = false`.
- `model.shell = "deny"` restricts only the model. Term mode can always run
  commands in the sandbox.
- Every process in the sandbox can connect to the relay socket. Shell commands
  can therefore use the configured model endpoint, but cannot choose a
  different upstream or read the key.

## Security model

- `runsc --rootless` runs the sandbox as your unprivileged host user inside a
  user namespace. The sandbox process is UID 0 inside that namespace, mapped to
  your host UID; it gains no host privileges.
- With `sandbox.internet = true`, the default network mode is gVisor's
  `sandbox`, which can reach the host's loopback interface. Set
  `GVISOR_NETWORK=none` to disable networking for other tools. With
  `sandbox.internet = false`, the mode is always `none`; model traffic uses the
  host relay socket instead.
- Model traffic always uses the relay socket at `/run/hutian/llm.sock`, which
  accepts only a request body and forwards it to one fixed target, so it cannot
  be used as a general proxy. With `model.apiKeyFile` or
  `model.apiKeyCommand`, the key stays on the host and is never visible to the
  sandbox. Without a key, the relay forwards unauthenticated.
- With `model.shell = "deny"`, the model cannot run commands: the shell tool is
  omitted from LLM requests and tool calls are refused. `"ask"` requires
  approval before each command, while `"allow"` runs model commands
  automatically. Term mode still lets you run commands yourself.
- Writes outside `/source`, `/tmp`, and the mounted package store are refused.
  Set `sandbox.readOnly = true` to refuse writes to `/source` as well.
- With `sandbox.nix = true`, the full host `/nix/store` and the Nix daemon
  socket are visible. Other host paths remain hidden.
- The sandbox uses the `systrap` platform by default. Set
  `GVISOR_PLATFORM=ptrace` if your host needs the `ptrace` platform.

## Architecture

The code keeps pure logic separate from IO:

- `Hutian.Command` parses term-mode input into a typed `Command` and classifies
  single-line versus multi-line input.
- `Hutian.Policy` defines the model's shell-tool policy (`deny`, `ask`, or
  `allow`) used by the LLM protocol and the tool loop. Term mode is not gated
  by it.
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
- `Hutian.Llm.UnixSocket` is the sandbox-side client for the host relay socket.
- `Hutian.Relay` is the host-side relay: it owns any configured key, accepts
  only a request body, builds the single fixed upstream request, and streams the
  response.
  `Hutian.Relay.Protocol` holds the four-byte status framing shared by both
  sides.
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
app/Hutian/Policy.hs      Model shell-tool policy
app/Hutian/Prompt.hs      Pure prompt reducer
app/Hutian/Render.hs      Pure ANSI prompt rendering
app/Hutian/Relay.hs       Host-side LLM relay
app/Hutian/Relay/Protocol.hs  Relay status framing
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
  key injection, no-auth forwarding, fixed upstream and headers, body limits,
  and response streaming
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
