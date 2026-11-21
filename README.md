# Hutian

Hutian is a Nix flake that drops you into a rootless
[gVisor](https://gvisor.dev/) sandbox with a small Haskell shell.

The sandbox sees only:

- the current working directory, mounted at `/source` read-only, or read-write
  when `sandbox.writable = true`;
- the runtime closures of the configured packages under `/nix/store`;
- optionally, with `sandbox.nixDaemon = true`, the host Nix store and Nix daemon
  socket.

Everything else on the host is hidden. The root filesystem is built from Nix's
static BusyBox sandbox shell and the `hutian-shell` binary.

The prompt is a multiline editor with two modes. It starts in LLM mode, where
input is preceded by a bold pink `ask:` line and sent to the model. `Ctrl+X`
switches to term mode, where a bold pink `shell:` line precedes input that runs
through BusyBox `ash`, so shell builtins and redirection work as expected.

The model's tools are controlled by the mandatory `model.tools` option. Each
tool has a three-way policy: `"deny"` hides it and refuses any call, `"ask"`
advertises it but needs approval, and `"allow"` runs it automatically.

- `tools.shell` covers `shell`, `shell_background`, `jobs`, `job_output`, and
  `job_kill`. Listing jobs and reading or stopping one do not need approval in
  `"ask"` mode; only starting a command does.
- `tools.webfetch` covers the `webfetch` tool, which fetches an http or https URL
  from inside the sandbox.
- `tools.websearch` is `null` to disable search, a search URL, or an attribute
  set with `url` and `policy`. The `websearch` tool fetches that endpoint with
  the URL-encoded query substituted for `{query}` or `%s`, or appended as `q=`.
- `tools.subagent` maps selector names to subagents. Each has an upstream
  `model`, a `description`, a `systemPrompt`, and its own `tools`. The main
  model sees the names and descriptions and calls one by selector through the
  `subagent` tool. Subagents run serially, use the same model API and
  conversation loop, and return only their final answer to the caller.

Term mode is always available. Model commands run through the same `/bin/sh -c`
path as term mode and are shown with a bold teal `tool shell:` line. In `"ask"`
mode Hutian shows an `approve:` prompt in the user's pink prompt colour while
the cursor is hidden, with `[y] Yes` and `[n] No` on their own lines. The choice
collapses into a single `Yes` or `No` line; only `y` or `n` decides, other keys
are ignored, and `Ctrl+C` aborts the current LLM turn. Shell output is preceded
by a dimmed pink `output:` line and tool output by a dimmed teal `tool output:`
line. Subagent work starts under a blue `subagent <name>:` line, followed by
the task it was given. Every tool prompt and tool output label inside that
subagent also uses the subagent blue instead of teal.

## Quick start

```console
$ cd /some/project
$ nix run github:knupfer/hutian#hutian
<Ctrl+X>          # switch from llm mode to term mode
pwd
<Enter>
/source
```

Run a single command without starting the interactive shell:

```console
$ nix run github:knupfer/hutian#hutian -- echo hello
$ nix run github:knupfer/hutian#hutian -- /bin/sh -c 'echo hello > /tmp/result.txt'
```

With `sandbox.writable = true`, files written under `/source` appear directly in
the host working directory. `/tmp` is private to the sandbox, and the sandbox
root is read-only.

## Input modes

The prompt starts in LLM mode. `Ctrl+X` switches between LLM and term mode
regardless of `model.tools.shell`. Each mode keeps its own input buffer:
switching saves the current text and restores the text of the mode being
entered. Submitted text goes to llama.cpp in LLM mode and to `/bin/sh -c` in
term mode.

| Key | Effect |
| --- | --- |
| `Enter` | Start a new line in LLM mode; execute in term mode. In term mode a trailing `\` or an unclosed single/double quote continues the command on the next line, like bash. |
| `Up`/`Down` | Recall input history. In LLM mode these first move through the current lines, then browse earlier prompts at the first or last line. |
| `Backspace`/`Delete` | Edit the text at the cursor; deleting a newline joins the two lines. |
| Arrow keys, `Home`/`End` | Move the cursor within the message, including between lines, and jump to the start or end of the current line. |
| `Tab` | Complete command names from `PATH` and filesystem paths in term mode. Ambiguous matches are listed below the input. |
| `Ctrl+D` | Exit on empty input in either mode. In LLM mode it also submits non-empty input. |
| `Ctrl+C` | Clear the current input and keep editing. |

### LLM mode

LLM mode is the initial mode. Input is preceded by a bold pink `ask:` line and
is syntax-highlighted as Markdown while you type, including headings, emphasis,
links, lists, and fenced code blocks. Replies use the same highlighting, and
fenced blocks tagged with a language use that language's syntax highlighter.
Reasoning/thinking tokens are shown in grey under a dimmed teal `think:` line,
with dimmed Markdown and code highlighting; the reply follows under a bold teal
`answer:` line.

Every LLM request includes the session's prior conversation: earlier prompts,
assistant replies, commands submitted in term mode, and tool calls with their
results.

The model can call the `shell` tool according to `model.tools.shell`. Each
command runs with `/bin/sh -c` in the sandbox working directory, its output is
streamed to the terminal in gray under a dimmed teal `tool output:` line, and
the output is sent back to the model. Every call runs in a fresh shell, so `cd`,
exported variables, and other shell state do not persist between calls. The
requested command is shown under a bold teal `tool shell:` line,
syntax-highlighted as shell, which distinguishes it from commands typed by the
user. In `"ask"` mode, non-interactive runs deny tool calls automatically.

For servers, watchers, and long builds, the model can call `shell_background`
instead of `shell`. It receives a job id immediately and uses `jobs`,
`job_output`, and `job_kill` to inspect and control the job. The requested
command is shown under a bold teal `tool shell_background:` line, and the start
result follows under `tool output:` in gray.

The model can also fetch pages with `webfetch`, search with `websearch`, and
delegate a self-contained task to a configured subagent. Web tools run in the
sandbox and need direct networking (`sandbox.network = true`) unless the URL is
reachable another way. A subagent gets its own system prompt and tools and runs
synchronously inside the tool call; only its final answer becomes the tool
result in the main conversation. Its streamed output appears under a blue
`subagent <name>:` line, which is followed by the task prompt, and its own
tool prompts and output labels stay in that same blue.

```console
Explain what a syscall is.
<Ctrl+D>
```

### Term mode

Term mode shows a bold pink `shell:` line above the input and prints command
output in gray under a dimmed pink `output:` line. Input is syntax-highlighted
as shell while you type. Every line is executed with `/bin/sh -c` in the
sandbox working directory.

Each command runs in a fresh shell, so `cd`, exported variables, and other shell
state do not carry over. Use absolute paths or combine steps in one command, for
example `cd sub && make`.

`Ctrl+C` cancels the current input or command without terminating the shell.
Term command history is stored in `/tmp/hutian-shell-history`, and LLM prompt
history in `/tmp/hutian-llm-history`.

## Background jobs

Long-running commands can run as background jobs so they do not hold the prompt
or an LLM turn open. The model starts one with `shell_background`. A started
command runs with `/bin/sh -c` in a new process group with stdin closed. Its
stdout and stderr are appended to `/tmp/hutian-jobs/<id>.log` inside the
sandbox; each job gets a fresh shell and starts in the sandbox working
directory.

The model lists every job in the session with `jobs`, including its status, pid
while running, and command. `job_output` prints the end of a log (50 lines by
default, at most 500, with an explicit truncation note). `job_kill` sends
SIGTERM to the process group and then SIGKILL if it has not exited.

Up to eight jobs can run at once. Finished jobs stay listed until the shell
exits, and exiting the shell stops any that are still running. Model job tool
calls use the same layout as the foreground shell tool: a bold teal
`tool <name>:` line, then `tool output:` with the result in gray. A background
start therefore shows `job <id> started` as tool output. A start notice shows
only the job id because the command is already visible on the request line. Job
completion is not announced; the model checks with `jobs` or `job_output`.

## Configuration

`lib.mkFlake` builds Hutian for every supported system from one package
selector. It exposes `apps.<system>.hutian` and `packages.<system>.hutian`, so a
dedicated flake can return its outputs directly and run with `nix run .#hutian`.
Each selected package's runtime closure is mounted under `/nix/store`, and its
`bin` directory is added to `PATH`. Closures of any size work, because Hutian
bind-mounts the closure into the root filesystem instead of passing one sandbox
volume per store path.

The `sandbox` and `model` groups are required. `sandbox` configures the package
closure, whether the host working directory is writable, whether Nix is
available, and whether the sandbox may use the network; `model` configures the
system message, model name, llama.cpp endpoint, and model permissions:

| Option | Applies to | Description |
| --- | --- | --- |
| `sandbox.packages` | sandbox | `pkgs` selector for the package closures mounted under `/nix/store` and added to `PATH`. |
| `sandbox.writable` | sandbox | Allows writes to `/source`; `false` mounts it read-only. It protects integrity, not confidentiality. |
| `sandbox.nixDaemon` | sandbox | Exposes the host `/nix/store` and Nix daemon socket. |
| `sandbox.network` | sandbox | Controls direct gVisor networking. `true` uses `host` mode; `false` forces `none`. |
| `model.name` | model | Model name sent to the endpoint. |
| `model.url` | relay | OpenAI-compatible chat or native `/completion` URL. `http://` and `https://` are supported. |
| `model.systemPrompt` | model | System message for every LLM request. |
| `model.tools` | model | Tool policies and configuration: `shell`, `webfetch`, `websearch`, and `subagent`. |
| `model.apiKeyCommand` | host relay | Command that prints the upstream key, or `null` for unauthenticated forwarding. |

Every option above is mandatory and has no default. Omitting one fails with an
error that lists the missing option paths, and unknown options are rejected, so
a misspelled key such as `model.foo` cannot be ignored silently.

Hutian expects an OpenAI-compatible llama.cpp chat endpoint; a native
`/completion` URL is also supported. HTTPS endpoints are verified against the CA
bundle from nixpkgs:

```text
https://llama.example.test/v1/chat/completions
```

A complete configuration:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    hutian = {
      url = "github:knupfer/hutian";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, hutian, ... }:
    hutian.lib.mkFlake nixpkgs {
      sandbox = {
        packages = pkgs: [ pkgs.coreutils pkgs.git ];
        writable = true;
        nixDaemon = false;
        network = true;
      };
      model = {
        name = "your-model";
        url = "http://127.0.0.1:8080/v1/chat/completions";
        systemPrompt = ''
          You are a careful coding assistant working in a sandboxed shell.
        '';
        tools = {
          shell = "allow";
          webfetch = "deny";
          websearch = null;
          subagent = { };
        };
        apiKeyCommand = null;
      };
    };
}
```

`/source` is read-only unless `sandbox.writable = true`. The sandbox can still
read the working directory, but any attempt to write to it fails with a
read-only filesystem error. Set `sandbox.writable = true` when the model should
be able to modify the project.

Set `sandbox.network = false` to give the sandbox gVisor's `none` network mode.
This is a hard gate: `GVISOR_NETWORK` cannot re-enable networking. The host
relay still provides model traffic over its Unix socket.

Set `model.tools.shell = "deny"` to disable the model's shell tools. Term mode
remains available for your own commands. Hutian omits the tools from every LLM
request and also refuses to execute any tool call that arrives anyway. Use
`"ask"` to keep the tools available with per-command approval (job inspection
stays free), or `"allow"` to run model-requested commands automatically.
`model.tools.webfetch` uses the same three values for the `webfetch` tool.

`model.tools.websearch` is `null` to disable search, a URL string, or an
attribute set such as `{ url = "https://search.example/?q="; policy = "ask"; }`.
The endpoint is fetched in the sandbox, so `sandbox.network = true` is required
for normal internet access. The query replaces `{query}` or `%s`, or is
appended as `q=` when the URL has neither placeholder.

`model.tools.subagent` maps selector names to subagents. Each selector has an
upstream `model` name, a `description`, a `systemPrompt`, and its own `tools`
with the same shape. The description and selector list appear in the
`subagent` tool schema, so the main model knows when each subagent is useful.
The model calls a selector with a self-contained prompt; the subagent cannot
see the main conversation. A subagent runs serially with the same API and tool
loop, never in parallel or in the background, and the caller receives only the
final answer. For example, a cheap summarization model can be given only
`webfetch` and `websearch` while the main model keeps the shell.

```nix
tools = {
  shell = "ask";
  webfetch = "deny";
  websearch = null;
  subagent = {
    summarize = {
      model = "cheap-summary-model";
      description = "Fetch and summarize web pages when current information is needed.";
      systemPrompt = "Fetch pages and summarize them concisely.";
      tools = {
        shell = "deny";
        webfetch = "allow";
        websearch = {
          url = "https://search.example/?q=";
          policy = "allow";
        };
        subagent = { };
      };
    };
  };
};
```

Then run `nix run .#hutian` from that project. Package versions come from the
consumer flake's lock file.

For a single system or an existing `pkgs` instance, use the lower-level builder:

```nix
hutian.lib.mkPackage pkgs {
  sandbox = {
    packages = pkgs: [ pkgs.git pkgs.jq ];
    writable = true;
    nixDaemon = false;
    network = true;
  };
  model = {
    name = "your-model";
    url = "http://127.0.0.1:8080/v1/chat/completions";
    systemPrompt = "You are a careful coding assistant working in a sandboxed shell.";
    tools = {
      shell = "allow";
      webfetch = "deny";
      websearch = null;
      subagent = { };
    };
    apiKeyCommand = null;
  };
}
```

To add Hutian to a flake that already has its own `default` app or package, use
`lib.mkApp` and `lib.mkPackage`. They add one output each and leave everything
else untouched:

```nix
outputs = { nixpkgs, hutian, ... }:
  let
    inherit (nixpkgs) lib;

    systems = [ "x86_64-linux" ];
    forAllSystems = lib.genAttrs systems;
    pkgsFor = system: nixpkgs.legacyPackages.${system};

    hutianConfig = {
      sandbox = {
        packages = pkgs: [ pkgs.coreutils ];
        writable = false;
        nixDaemon = false;
        network = false;
      };
      model = { ... };
    };
  in
  {
    # Keep your existing packages and apps, and add Hutian next to them.
    packages = forAllSystems (system: existingPackages.${system} // {
      hutian = hutian.lib.mkPackage (pkgsFor system) hutianConfig;
    });

    apps = forAllSystems (system: existingApps.${system} // {
      hutian = hutian.lib.mkApp (pkgsFor system) hutianConfig;
    });
  };
```

Here `existingPackages` and `existingApps` are the flake's existing per-system
attrsets. Complete examples are available in
[`examples/dedicated.nix`](./examples/dedicated.nix) for a dedicated Hutian
flake and [`examples/existing-project.nix`](./examples/existing-project.nix)
for adding Hutian to an existing project flake. Both import the shared
configuration in [`examples/hutian.nix`](./examples/hutian.nix).

The launcher passes `model.url` to the shell as `--llama-url`, keeping only the
path so the query and fragment stay on the host. It writes `model.name` and
`model.systemPrompt` to short-lived files passed as `--model-file` and
`--system-prompt-file`; the shell removes those files before it accepts input,
so they do not appear in the process listing. The shell reads no Hutian-specific
environment variables. The sandbox receives the CA bundle path as
`SYSTEM_CERTIFICATE_PATH` so the Haskell HTTP client can verify TLS
certificates.

### Using Nix inside the sandbox

Pass `sandbox.nixDaemon = true` to expose the host Nix store and connect the
sandbox to the host's Nix daemon:

```nix
sandbox = {
  packages = pkgs: [ pkgs.coreutils pkgs.git ];
  writable = true;
  nixDaemon = true;
  network = true;
};
```

The rest of the configuration is unchanged, and the lower-level
`lib.mkPackage` builder accepts the same options and package selector.

In this mode the host's Nix store and Nix daemon socket are exposed to the
sandbox. Commands such as `nix build`, `nix shell`, `nix run`, and `nix develop`
act as clients of the host daemon, so builds and substitutions run on the host.
Flakes and the `nix` command are enabled automatically, and a CA bundle is
provided for client-side HTTPS fetches.

The launcher supplies a local flake registry instead of downloading the global
one. It contains a single entry, mapping `nixpkgs` to the nixpkgs input of the
flake that configured Hutian, so `nix run nixpkgs#...` uses the same package set
as `sandbox.packages`. The host's system and user registries are not exposed, so
other indirect names are not available unless the project defines them itself.

Consequently, `nix run nixpkgs#...` resolves and starts an already-built package
without fetching the registry or the nixpkgs flake. Locked local builds work
offline as long as their inputs are already in the store; pass `--offline` if
you want Nix to fail instead of trying a substituter for a missing path.

gVisor normally caches directory entries for the life of the sandbox. That hides
changes made on the host after startup: a file added to the working directory
stays invisible and writes to its name fail with `ENOENT`, and a store path the
host daemon creates after `/nix/store` was listed cannot be executed. The
launcher therefore disables the gVisor dentry cache (`--dcache=0`) in every
mode, so lookups go back to the host.

The interactive shell also avoids keeping its own working directory inside the
shared working directory, because gVisor keeps a directory listing cached while
a process is in it. Each command enters the working directory for its own
lifetime, so files created or changed outside the sandbox appear in the next
command. A long-running process that stays in the working directory keeps the
view it had when it started.

The host Nix store is visible in full while `sandbox.nixDaemon = true`. Set it to
`false` when you only need the configured package closures.

### Environment

The sandbox receives a fixed `TERM=xterm-256color` and `LANG=C.UTF-8`. Only the
gVisor runtime settings can be overridden from the host:

| Variable | Default | Description |
| --- | --- | --- |
| `GVISOR_PLATFORM` | `systrap` | gVisor platform. Use `ptrace` if your host requires it. |
| `GVISOR_NETWORK` | `host` | gVisor network mode: `host` or `none`. `sandbox` is not supported with `--rootless` and falls back to `host`. Ignored when `sandbox.network = false`, where the mode is always `none`. |

## Relay and API keys

Hutian always routes model traffic through a host-side relay. The shell is
launched with `--llama-socket /run/hutian/llm.sock` and the logical URL
`http://hutian-relay/...`, which preserves the chat or completion API shape
without exposing the real upstream host, credentials, query, or fragment
embedded in `model.url`.

`model.apiKeyCommand` is mandatory: set it to `null` when no key is used, or to
a command that prints the key when the upstream requires one:

```nix
hutian.lib.mkFlake nixpkgs {
  # ...
  model.apiKeyCommand = "pass show llama/api-key";
}
```

To read a key from a file, use `cat`:

```nix
model.apiKeyCommand = "cat /run/secrets/hutian-key";
```

The launcher starts `hutian-llm-relay` on the host. With a command configured,
the relay reads the key from standard input; the launcher runs the command and
pipes its output into the relay. `model.apiKeyCommand` runs in the foreground
before the sandbox starts, so commands that prompt for a password (for example
`passage`) work normally and can be cancelled with `Ctrl+C`. Without a key the
relay starts with `--no-auth` and forwards requests without an `Authorization`
header. This works with local llama.cpp servers that do not require a key, and
the sandbox still needs no direct network access to the model endpoint.

The relay listens on a per-session Unix socket under `$XDG_RUNTIME_DIR` (falling
back to `/tmp`). The socket directory is mode `0700` and is bind-mounted
read-only at `/run/hutian`, so the sandbox can connect to `/run/hutian/llm.sock`
but cannot unlink or replace the socket; the directory holds no key material.

The relay uses a private framing protocol over the socket instead of HTTP. The
sandbox sends only the request body and closes its write half. The relay sends
that body to the fixed path and query of `model.url` with `Content-Type:
application/json`, `Accept: text/event-stream`, and, when a key is configured,
`Authorization: Bearer <key>`. It returns a four-byte status code followed by
the upstream body, including streaming SSE. Because the client never supplies a
request line or header block, the sandbox cannot choose the upstream host, path,
or headers, cannot redirect the key, and cannot use the relay as a general
authenticated proxy. The relay does not follow upstream redirects, and the key
never appears in argv, environment variables, files, or the Nix store.

Keep `model.apiKeyCommand` as a command that fetches the key, for example from
`pass` or `sops`, rather than embedding the key in the command string, because
option values live in the Nix store. The command runs on the host, but do not
read the key from `/source` or the Nix store: the sandbox can read both. Use
`model.apiKeyCommand = null` when the upstream does not need a key.

## Permission model

Hutian separates sandbox capabilities from model-tool permissions. The
`sandbox.*` options apply to everything running in the sandbox; `model.tools`
restricts only the model, and `model.apiKeyCommand` only affects the host relay.

- `sandbox.network = false` removes direct sandbox networking only. The relay
  socket can still reach the fixed model upstream, and `sandbox.nixDaemon = true`
  still lets the host Nix daemon fetch and build. Because the daemon runs on the
  host, a fixed-output derivation can also send anything the sandbox can read to
  any URL the host network reaches; treat `network = false` as advisory in that
  mode.
- `sandbox.writable = true` writes through to the host working directory. Files
  created or changed on the host appear in the next sandbox command; a command
  that keeps its own working directory there sees the view from when it started.
- `sandbox.writable = false` does not prevent the model from reading `/source`
  and sending what it reads upstream. Set `model.tools.shell = "deny"`,
  `model.tools.webfetch = "deny"`, and disable subagents or use a local model
  endpoint if confidentiality matters.
- `sandbox.network = true` defaults to gVisor's `host` mode, so the sandbox
  shares the host network namespace: loopback, LAN, and the Internet.
  `GVISOR_NETWORK` can narrow it to `none` but cannot widen
  `sandbox.network = false`.
- `model.tools.shell = "deny"` restricts only the model. Term mode can always run
  commands in the sandbox, including background jobs. The same applies to
  `webfetch`, `websearch`, and subagents; subagents can use only the tools in
  their own `tools` attribute.
- Every process in the sandbox can connect to the relay socket. Shell commands
  can therefore use the configured model endpoint, but cannot choose a
  different upstream or read the key.

## Security model

- `runsc --rootless` runs the sandbox as your unprivileged host user inside a
  user namespace. The sandbox process is UID 0 inside that namespace, mapped to
  your host UID; it gains no host privileges.
- With `sandbox.network = true`, the default network mode is gVisor's `host`,
  which shares the host network namespace (loopback, LAN, and the Internet). Set
  `GVISOR_NETWORK=none` to disable networking for other tools. With
  `sandbox.network = false`, the mode is always `none`; model traffic uses the
  host relay socket instead.
- Model traffic always uses the relay socket at `/run/hutian/llm.sock`, which
  accepts only a request body and forwards it to one fixed target, so it cannot
  be used as a general proxy. With `model.apiKeyCommand`, the key stays on the
  host and is never visible to the sandbox. Without a key
  (`model.apiKeyCommand = null`), the relay forwards unauthenticated.
- With `model.tools.shell = "deny"`, the model cannot run commands: the shell
  tool is omitted from LLM requests and tool calls are refused. `"ask"` requires
  approval before each command, while `"allow"` runs model commands
  automatically. `webfetch` and `websearch` have their own policies, and a
  subagent can use only its configured tools. Term mode still lets you run
  commands yourself.
- A static `dumb-init` runs as PID 1 so background jobs that outlive their shell
  are reaped rather than left as zombies. It runs in single-child mode so the
  sandbox keeps its controlling terminal and interactive programs can open
  `/dev/tty`; the user shell runs as its child.
- Writes outside `/source`, `/tmp`, and the mounted package store are refused.
  `sandbox.writable = false` refuses writes to `/source` as well.
- With `sandbox.nixDaemon = true`, the full host `/nix/store` and the Nix daemon
  socket are visible. Other host paths remain hidden, including the host's flake
  registries; `nixpkgs` comes from the configured flake instead.
- The sandbox uses the `systrap` platform by default. Set
  `GVISOR_PLATFORM=ptrace` if your host needs the `ptrace` platform.

## Architecture

The code keeps pure logic separate from IO:

- `Hutian.Command` classifies submitted term-mode text as blank, single-line,
  or multi-line.
- `Hutian.Policy` defines the shared three-way tool policy (`deny`, `ask`, or
  `allow`) used by the LLM protocol and the tool loop. `Hutian.Tool.Config`
  holds the tool policies, websearch endpoint, and serial subagent
  configuration, with JSON parsing for the launcher-provided tools file.
- `Hutian.Tool` turns a model tool call and the tool configuration into a pure
  `ToolPlan`. `Hutian.Tool.Exec` performs only the terminal, shell, web, or
  subagent IO that the plan requests, so refusals, approvals, commands, and job
  actions can be tested without a terminal. `Hutian.Web` builds search URLs and
  performs bounded HTTP fetches.
- `Hutian.Jobs` is the pure job model: ids, status, log paths, and the text
  used by the model tools. `Hutian.Jobs.Runtime` owns the process handles,
  bounded log tails, and process-group signalling.
- `Hutian.Cli` is the shared strict option parser for the shell and relay. It
  rejects duplicate options, unknown options, bare positional arguments, and
  missing values instead of silently letting argument order decide.
- `Hutian.Editor` owns prompt state, key semantics, and layout.
- `Hutian.Prompt` is the pure prompt reducer: it turns a key and the current
  state into a step such as redraw, move cursor, complete, or finish.
- `Hutian.Completion` selects command and path completions.
- `Hutian.Ansi` represents RGB colours and text styles as data and encodes them
  with `ansi-terminal`.
- `Hutian.Render` turns prompts and output labels into ANSI output, and holds
  the pure approval decisions shared with the console.
- `Hutian.History` owns the pure on-disk history codec.
- `Hutian.Conversation` is a pure model of chat messages, tool calls, and tool
  results, including the abort results recorded when an interrupt stops a
  turn; `Hutian.Paths` holds the pure path-completion helpers.
- `Hutian.Markdown` is a pure streaming renderer for Markdown and ANSI styles.
- `Hutian.Highlight` wraps Skylighting for fenced code and shell input, used by
  both term-mode input and model-requested commands.
- `Hutian.Llm` keeps SSE parsing, tool-call accumulation, request building, and
  settings parsing pure, with IO limited to HTTP, bounded error bodies, and a
  caller-supplied output sink.
- `Hutian.Http.Wire` contains the shared socket framing helpers used by both
  the relay and the sandbox-side client.
- `Hutian.Llm.UnixSocket` is the sandbox-side client for the host relay socket.
- `Hutian.Relay` is the host-side relay: it owns any configured key, accepts
  only a request body, builds the single fixed upstream request, and streams the
  response. `Hutian.Relay.Protocol` holds the four-byte status framing shared by
  both sides.
- `Hutian.Console` is the single terminal IO boundary: writes, terminal
  attributes, cursor styling, size, and stream colouring.
- `Hutian.Key` decodes terminal input, and `Hutian.Session` owns the interactive
  prompt, history, and command loop.
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
flake.nix                       Flake outputs and consumer helpers
nix/hutian.nix                  Sandbox rootfs, launcher, and wrapper
nix/hutian-shell.nix            Haskell package definition
nix/sandbox.nix                 Rootfs mounts, working directory, and relay mount
nix/lint.nix                    Shared lint and LiquidHaskell checks
src/                            Haskell library source
src/Hutian/Ansi.hs              Typed terminal colours and styles
src/Hutian/Cli.hs               Strict shared command-line option parser
src/Hutian/Command.hs           Term-mode input classification
src/Hutian/Completion.hs        Pure completion selection
src/Hutian/Console.hs           Single terminal IO boundary
src/Hutian/Conversation.hs      Conversation entries, tool results, and aborts
src/Hutian/Editor.hs            Prompt text, cursor, and layout state
src/Hutian/Highlight.hs         Skylighting for fenced code and shell input
src/Hutian/History.hs           Pure history codec and navigation
src/Hutian/History/Store.hs     History file IO
src/Hutian/Http/Wire.hs         Shared socket framing helpers
src/Hutian/Interrupt.hs         Interrupt flag and signal handling
src/Hutian/Jobs.hs              Pure background-job model and text
src/Hutian/Jobs/Runtime.hs      Job processes, logs, and signals
src/Hutian/Key.hs               Pure terminal key classification
src/Hutian/Llm.hs               HTTP, relay, and streamed-response IO
src/Hutian/Llm/Loop.hs          Model and tool exchange loop
src/Hutian/Llm/Presentation.hs  Stream labels, colours, and Markdown flushing
src/Hutian/Llm/Request.hs       Chat and completion request bodies
src/Hutian/Llm/Settings.hs      Launcher settings parsing
src/Hutian/Llm/Stream.hs        Stream parsing, tool-call assembly, and result
src/Hutian/Llm/UnixSocket.hs    Relay client over a Unix socket
src/Hutian/Markdown.hs          Pure streaming Markdown renderer
src/Hutian/Paths.hs             Pure path completion helpers
src/Hutian/Policy.hs            Shared model tool policy
src/Hutian/Prompt.hs            Pure prompt reducer
src/Hutian/Relay.hs             Host-side LLM relay
src/Hutian/Relay/Protocol.hs    Relay status framing
src/Hutian/Render.hs            Pure ANSI rendering and approval decisions
src/Hutian/Session.hs           Interactive prompt/history/session loop
src/Hutian/Shell.hs             Captured shell command execution
src/Hutian/Terminal.hs          Prompt driver over Prompt and Console
src/Hutian/Tool.hs              Pure tool schemas, decoding, and planning
src/Hutian/Tool/Config.hs       Tool policies, websearch, and subagent config
src/Hutian/Tool/Exec.hs         Terminal execution of tool-call plans
src/Hutian/Web.hs               Bounded HTTP fetches and search URLs
app/Main.hs                     Shell executable entry point
app/RelayMain.hs                Relay executable entry point
test/                           Unit and property tests
test/TestReporter.hs            Deferred, single-threaded Tasty report
test/TestSupport.hs             Shared test helpers
hutian-shell.cabal              Haskell package manifest
examples/                       Example flakes
  hutian.nix                    Shared Hutian configuration
  dedicated.nix                 Dedicated Hutian flake
  existing-project.nix          Project flake that also exposes Hutian
LICENSE                         MIT license
CHANGELOG.md                    Release notes
```

## Testing

The test suite combines HUnit tests with QuickCheck properties for the editor
state machine, the pure prompt reducer, prompt layout and rendering, command
parsing, completion selection, history codecs, conversation history, LLM
requests, streaming tool calls, Markdown rendering, code highlighting, path and
shell helpers, strict option parsing, tool-call plans, tool and subagent
configuration, web search URLs, stream presentation, the background-job model,
socket framing, and interrupt handling. It also includes integration tests for:

- streaming LLM responses through a local HTTP server, including bounded
  non-success error reporting
- streaming LLM responses through the host relay over a Unix socket, including
  key injection, no-auth forwarding, fixed upstream and headers, body limits,
  and response streaming
- history file persistence
- shell command execution and command discovery
- background jobs: output capture, status, bounded log tails, and stopping a
  process group

Run it through Nix:

```console
$ nix build .#packages.x86_64-linux.hutian
```

The package build runs the suite as part of the standard build. With a local
Cabal setup, `cabal test` runs the same tests directly.

Tests run one at a time and the Tasty report is buffered until the last test
finishes. Several suites redirect the process-wide stdout and stderr or install
signal handlers, so a live report would leak into their captures and make
otherwise deterministic tests flaky.

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

`haskell-lint` runs HLint over `src/`, `app/`, and `test/`. `nix-lint` runs
Statix and DeadNix over the flake and Nix files. These linters also run
automatically in the package check phase, so a normal `nix build` fails on lint
errors. `nix flake check` runs the explicit checks along with the package build.

LiquidHaskell verifies the pure modules of the shell and relay: `Hutian.Ansi`,
`Hutian.Cli`, `Hutian.Command`, `Hutian.Completion`, `Hutian.Conversation`,
`Hutian.Editor`, `Hutian.History`, `Hutian.Jobs`, `Hutian.Key`,
`Hutian.Llm.Request`, `Hutian.Llm.Settings`, `Hutian.Llm.Stream`,
`Hutian.Paths`, `Hutian.Policy`, `Hutian.Prompt`, `Hutian.Relay.Protocol`, and
`Hutian.Tool`.

```console
$ nix build .#checks.x86_64-linux.liquid
```

The check disables DEFAULT case expansion (`--max-case-expand=0`). Expanding
those cases does not change the verification result, but the default depth
makes LiquidHaskell warn about every unexpanded match, so the check keeps the
output clean instead.

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
