# Hutian

Hutian is an AI coding shell. You describe a task, and a model works on your
project with shell, web-fetch, subagent, and MCP tools — while every command runs
inside a rootless [gVisor](https://gvisor.dev/) sandbox.

It is packaged as a Nix flake. The sandbox sees only your working directory at
`/source` (read-only unless `sandbox.writable = true`), the runtime closures of
`sandbox.packages` under `/nix/store`, and — with `sandbox.nixDaemon = true` —
the host Nix store and daemon socket. Everything else on the host is hidden.

The shell has two input modes:

- **LLM mode** (default): a bold pink `ask` line precedes input sent to the
  model.
- **Term mode** (`Ctrl+X`): a bold pink `shell` line precedes input run through
  BusyBox `ash`.

Every model tool has a mandatory three-way policy: `"deny"` hides it and refuses
calls, `"ask"` advertises it but requires approval, and `"allow"` runs it
automatically.

## Quick start

Run the published flake directly:

```console
$ cd /some/project
$ nix run github:knupfer/hutian#hutian
```

Run a single command without the interactive shell:

```console
$ nix run github:knupfer/hutian#hutian -- echo hello
$ nix run github:knupfer/hutian#hutian -- /bin/sh -c 'echo hello > /tmp/result.txt'
```

With `sandbox.writable = true`, writes under `/source` appear in the host working
directory. `/tmp` is private to the sandbox and the sandbox root is read-only.

## Use it in your project

The [`examples/`](./examples) directory contains a ready-to-run configuration:

- [`hutian.nix`](./examples/hutian.nix) — the annotated option set.
- [`dedicated.nix`](./examples/dedicated.nix) — a flake that exposes Hutian as
  its only app.
- [`existing-project.nix`](./examples/existing-project.nix) — a flake that adds
  Hutian beside an existing `default`.

For a project that only needs Hutian, copy both files and rename the flake:

```console
$ cp /path/to/hutian/examples/hutian.nix .
$ cp /path/to/hutian/examples/dedicated.nix flake.nix
$ $EDITOR hutian.nix      # set endpoint.url, agent.model, and tools
$ nix run .#hutian
```

`nix run .#hutian` builds and starts the `hutian` app from the local flake;
`nix build .#hutian` builds the package instead.

To keep an existing flake's `default`, copy `existing-project.nix` as `flake.nix`
(or merge its `outputs` into yours), keep `hutian.nix` beside it, and run
`nix run .#hutian`. That example uses `mkApp` and `mkPackage`, so the project's
own `default` stays in place.

The flake exports `hutian.lib.mkFlake`, `mkApp`, and `mkPackage`. All take the
same option set:

- `mkFlake nixpkgs options` returns `packages.<system>.hutian` and
  `apps.<system>.hutian` for `x86_64-linux` and `aarch64-linux`.
- `mkApp pkgs options` returns one app; `mkPackage pkgs options` returns the
  derivation. Use them for a single system or an existing `pkgs` instance.

None replaces `default`, and `lib` exports nothing else. Validation runs when the
result is evaluated, so `nix flake check` reports a bad configuration without
building.

## Tools

`agent.tools` has four groups:

- `shell`: `shell`, `shell_background`, `jobs`, `job_output`, and `job_kill`. In
  `"ask"` mode, listing jobs and reading logs are free; starting and stopping
  jobs need approval.
- `webfetch`: fetches an http or https URL from inside the sandbox.
- `subagent`: selector names mapped to subagents, each with a `model`, a
  `description`, a `systemPrompt`, and its own `tools`. The main model calls one
  by selector; subagents run serially and return only their final answer.
- `mcp`: names mapped to MCP servers. Hutian discovers each server's tools and
  advertises them as `mcp__<server>__<tool>` (prompts show
  `mcp.<server>.<tool>`). Each server has a `url`, a `policy`, and a `handlers`
  map; see [Tool result handlers](#tool-result-handlers).

MCP tool calls show their JSON arguments with dim JSON highlighting. Error lines
are bold red when standard error is a terminal.

## Modes and output

The prompt starts in LLM mode; `Ctrl+X` switches to term mode. Each mode keeps
its own input buffer. LLM input is Markdown-highlighted and sent to the model;
term input is shell-highlighted and run with `/bin/sh -c`. Each term command is a
fresh shell, so `cd` and exported variables do not persist.

| Key | Effect |
| --- | --- |
| `Enter` | New line in LLM mode; execute in term mode. A trailing `\` or unclosed quote continues the command. |
| `Up`/`Down` | Recall history; in LLM mode, move through lines first. |
| `Backspace`/`Delete` | Edit text; deleting a newline joins lines. |
| Arrow keys, `Home`/`End` | Move the cursor; jump to line start or end. |
| `Tab` | Complete command names and paths in term mode. |
| `Ctrl+D` | Exit on empty input; in LLM mode, submit non-empty input. |
| `Ctrl+C` | Clear the current input; in term mode, cancel the running command. |

Prompt labels are dotted paths: the main agent prints `think`, `answer`,
`output`, and the tool name; a subagent `foo` prefixes them with `foo.`
(`foo.think`, `foo.answer`, `foo.shell`, `foo.shell.output`); nesting extends the
path (`foo.bar.think`).

`output.verbosity` controls detail:

- `"low"`: tool calls with arguments and main-turn answers; other blocks show
  only their prompt.
- `"normal"`: adds main-agent thinking; a subagent's final answer streams dim.
- `"high"`: everything, including full reasoning and every subagent block, plus
  the exact system message and a `# name(...)` description per tool.

In `"ask"` mode an `approve` prompt appears with `[y] Yes` and `[n] No`; only
`y`/`n` decide, and `Ctrl+C` aborts the turn. Shell output appears under a dimmed
`shell.output` line and tool output under `<tool>.output`.

Background jobs (`shell_background`) return a job id immediately. `jobs` lists
them, `job_output` prints the end of a log (50 lines by default, at most 500),
and `job_kill` stops the process group. Up to eight run at once; exiting the
shell stops any still running. Job output uses the same `<tool>.output` layout.

## Configuration

The same options apply to all three helpers. The `sandbox`, `endpoint`, `output`,
and `agent` groups are required, and every nested key is mandatory. Omitting one
fails with an error naming the path; unknown keys are rejected. Write `null` for
`endpoint.apiKeyCommand` and `{ }` for an empty `subagent`, `mcp`, or `handlers`
map.

| Option | Description |
| --- | --- |
| `sandbox.packages` | `pkgs` selector for closures mounted under `/nix/store` and added to `PATH`. |
| `sandbox.writable` | Allow writes to `/source`; `false` mounts it read-only. |
| `sandbox.nixDaemon` | Expose the host `/nix/store` and Nix daemon socket. |
| `sandbox.network` | Direct gVisor networking; `true` is `host`, `false` is `none`. |
| `endpoint.url` | OpenAI-compatible chat or native `/completion` URL. |
| `endpoint.apiKeyCommand` | Command that prints the upstream key, or `null`. |
| `output.verbosity` | `"low"`, `"normal"`, or `"high"`. |
| `agent.model` | Model name sent to the endpoint. |
| `agent.systemPrompt` | System message for every request. |
| `agent.tools` | `shell`, `webfetch`, `subagent`, and `mcp` policies. |

A minimal configuration:

```nix
{
  sandbox = {
    packages = pkgs: [ pkgs.coreutils pkgs.git ];
    writable = true;
    nixDaemon = false;
    network = false;
  };
  endpoint = {
    url = "http://127.0.0.1:8080/v1/chat/completions";
    apiKeyCommand = null;
  };
  output = { verbosity = "normal"; };
  agent = {
    model = "your-model";
    systemPrompt = "You are a careful coding assistant.";
    tools = {
      shell = "ask";
      webfetch = "deny";
      subagent = { };
      mcp = { };
    };
  };
}
```

See [`examples/hutian.nix`](./examples/hutian.nix) for the annotated version,
including commented subagent, MCP, and handler examples.

`/source` is read-only unless `sandbox.writable = true`. `sandbox.network = false`
is a hard gate: `GVISOR_NETWORK` cannot re-enable networking, but the host relay
still carries model traffic.

`agent.tools.shell = "deny"` removes the shell tools from requests and refuses
calls; term mode still works. `"ask"` requires approval per command (job listing
and log reads are free), and `"allow"` runs automatically. `webfetch` uses the
same values.

Subagents and MCP servers are configured under `agent.tools`:

```nix
tools = {
  shell = "ask";
  webfetch = "deny";
  subagent = {
    summarize = {
      model = "cheap-summary-model";
      description = "Fetch and summarize web pages when needed.";
      systemPrompt = "Fetch pages and summarize them concisely.";
      tools = { shell = "deny"; webfetch = "allow"; subagent = { }; mcp = { }; };
    };
  };
  mcp = {
    search = {
      url = "https://mcp.example.test/mcp";
      policy = "ask";
      handlers = { };
    };
  };
};
```

MCP discovery and calls run in the sandbox, so `sandbox.network = true` is needed
for normal internet access. Use a local proxy when the server needs credentials
or a fixed network path.

### Tool result handlers

A handler is a subagent configuration attached to one MCP tool. It receives the
raw result and returns only its final answer, in its own context with its own
model and tools:

```nix
tools.mcp.szmcp = {
  url = "http://localhost:3001";
  policy = "allow";
  handlers.zim_get = {
    model = "tiny";
    description = "Summarize the article; preserve paths and citations.";
    systemPrompt = "Summarize tool output tersely; preserve facts and citations.";
    tools = { shell = "deny"; webfetch = "deny"; subagent = { }; mcp = { }; };
  };
};
```

The handler has no `name`; its label is `mcp.<server>.handlers.<tool>`. When the
agent calls `mcp__szmcp__zim_get` (shown as `mcp.szmcp.zim_get`), the raw result
becomes the handler's entire prompt and only its answer is appended as the tool
result. There is no separate `<tool>.output` block: the prompt body is shown
under the handler label and follows tool-output visibility. Handlers can use real
tools and their own MCP handlers; they never inherit the caller's.

The handler's `description` is appended to the tool description after a newline.
An unknown handler is reported after discovery and ignored. Built-in tools and
error results bypass handlers.

### Nix inside the sandbox

Set `sandbox.nixDaemon = true` to expose the host Nix store and daemon socket:

```nix
sandbox = {
  packages = pkgs: [ pkgs.coreutils pkgs.git ];
  writable = true;
  nixDaemon = true;
  network = true;
};
```

`nix build`, `nix shell`, `nix run`, and `nix develop` then act as clients of the
host daemon, so builds run on the host. Flakes and the `nix` command are enabled,
and a CA bundle is provided for HTTPS fetches. The launcher supplies a local
registry mapping `nixpkgs` to the configuring flake's nixpkgs input; host
registries are hidden, so other indirect names are unavailable unless the
project defines them. Locked local builds work offline when their inputs are in
the store.

gVisor normally caches directory entries; the launcher disables that cache
(`--dcache=0`) so host changes appear. The interactive shell also avoids keeping
its working directory inside the shared directory, so each command sees the
current host state.

### Environment

The sandbox gets `TERM=xterm-256color` and `LANG=C.UTF-8`. Only these can be
overridden:

| Variable | Default | Description |
| --- | --- | --- |
| `GVISOR_PLATFORM` | `systrap` | Use `ptrace` if your host requires it. |
| `GVISOR_NETWORK` | `host` | `host` or `none`; ignored when `sandbox.network = false`. |

## Relay and API keys

Model traffic always goes through a host-side relay. The shell uses
`--llama-socket /run/hutian/llm.sock` and the logical URL
`http://hutian-relay/...`, so the real upstream host, credentials, query, and
fragment never enter the sandbox.

`endpoint.apiKeyCommand` is mandatory: set it to `null` or to a command that
prints the key:

```nix
endpoint.apiKeyCommand = "pass show llama/api-key";
```

The launcher starts `hutian-llm-relay` on the host, evaluates the command with
the host shell, and pipes its output to the relay. The command is trusted and
runs in the foreground before the sandbox, so password prompts work. Without a
key the relay uses `--no-auth` and sends no `Authorization` header.

The relay listens on a per-session Unix socket under `$XDG_RUNTIME_DIR` (or
`/tmp`), in a `0700` directory bind-mounted read-only at `/run/hutian`. The
sandbox can connect but cannot unlink or replace the socket, and the directory
holds no key material.

The relay uses a private framing protocol: the sandbox sends only a request body
and closes its write half. The relay forwards it to the fixed path and query of
`endpoint.url` with `Content-Type: application/json`,
`Accept: text/event-stream`, `Connection: close`, and `Authorization: Bearer
<key>` when configured. It uses a fresh upstream connection per request, returns
a four-byte status followed by the body (including SSE), and does not follow
redirects. Because the client never supplies a request line or headers, the
sandbox cannot choose the upstream, redirect the key, or use the relay as a
general proxy.

Keep `apiKeyCommand` as a fetcher (`pass`, `sops`, `cat`) rather than embedding
the key, because option values live in the Nix store. Do not read the key from
`/source` or the Nix store, which the sandbox can read.

## Security model

`sandbox.*` applies to everything in the sandbox; `agent.tools` restricts only
the model; `endpoint.apiKeyCommand` affects only the host relay.

- `runsc --rootless` runs the sandbox as your unprivileged host user in a user
  namespace; UID 0 inside maps to your host UID and gains no host privileges.
- `sandbox.network = false` removes direct networking only. The relay still
  reaches the fixed model upstream, and `sandbox.nixDaemon = true` still lets the
  host daemon fetch and build. A fixed-output derivation can send anything the
  sandbox can read to any URL the host reaches, so treat `network = false` as
  advisory in that mode.
- `sandbox.network = true` shares the host network namespace. `GVISOR_NETWORK`
  can narrow it to `none` but cannot widen `false`.
- `sandbox.writable = true` writes through to the host working directory. `false`
  still lets the model read `/source` and send it upstream; set `shell = "deny"`,
  `webfetch = "deny"`, and disable subagents (or use a local model) if
  confidentiality matters.
- The relay socket accepts only a request body for one fixed target. The key
  stays on the host, and every sandbox process can use the endpoint but cannot
  choose another upstream or read the key.
- `agent.tools.shell = "deny"` omits the shell tools and refuses calls. `"ask"`
  requires approval; `"allow"` runs automatically. Term mode still works.
- A static `dumb-init` runs as PID 1 to reap orphaned background jobs while
  keeping the controlling terminal and `/dev/tty` available.
- Writes outside `/source`, `/tmp`, and the mounted package store are refused.
- With `sandbox.nixDaemon = true`, the full host `/nix/store` and daemon socket
  are visible; other host paths, including flake registries, stay hidden.
- The sandbox uses `systrap` by default; set `GVISOR_PLATFORM=ptrace` if needed.

## Development

```console
$ nix build .#packages.x86_64-linux.hutian   # builds and runs the test suite
$ nix build .#checks.x86_64-linux.configuration
$ nix build .#checks.x86_64-linux.haskell-lint
$ nix build .#checks.x86_64-linux.nix-lint
$ nix build .#checks.x86_64-linux.liquid
$ nix fmt
```

The test suite combines HUnit and QuickCheck for the editor, prompt reducer,
rendering, completion, history, conversation, LLM requests and streaming, tool
plans and configuration, MCP, web fetches, jobs, framing, and interrupts, plus
integration tests for HTTP and relay streaming, MCP discovery and calls, shell
execution, and background jobs. Tests run one at a time with a buffered Tasty
report because some suites redirect stdout/stderr or install signal handlers.

`haskell-lint` runs HLint; `nix-lint` runs Statix and DeadNix. LiquidHaskell
verifies the pure modules `Hutian.Ansi`, `Hutian.Cli`, `Hutian.Command`,
`Hutian.Completion`, `Hutian.Conversation`, `Hutian.Editor`, `Hutian.History`,
`Hutian.Jobs`, `Hutian.Key`, `Hutian.Llm.Request`, `Hutian.Llm.Settings`,
`Hutian.Llm.Stream`, `Hutian.Paths`, `Hutian.Policy`, `Hutian.Prompt`,
`Hutian.Relay.Protocol`, and `Hutian.Tool`. All checks also run during
`nix build`, and `nix flake check` runs them with the package build.

The package uses `GHC2021` and compiles with strict warnings and `-Werror`.

## Requirements

- Linux with unprivileged user namespaces enabled.
- x86_64 or aarch64.

If user namespaces are disabled, `runsc` reports an error. On Debian-style
systems:

```console
$ sudo sysctl kernel.unprivileged_userns_clone=1
```
