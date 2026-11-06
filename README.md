# Hutian

Hutian is a Nix flake that drops you into a rootless
[gVisor](https://gvisor.dev/) sandbox with a small Haskell shell.

The sandbox sees only:

- the current working directory, mounted read-write at `/source`;
- the runtime closures of the packages you configure, mounted under
  `/nix/store`;
- optionally, when `nix = true`, the host Nix store and Nix daemon socket.

Everything else on the host is hidden. The root filesystem is built from Nix's
static BusyBox sandbox shell plus the Haskell `hutian-shell` binary.

The normal shell uses [Haskeline](https://hackage.haskell.org/package/haskeline)
for line editing, history, and tab completion. External commands run through
BusyBox `ash`, so shell builtins and redirection work as expected.

## Quick start

```console
$ cd /some/project
$ nix run /path/to/this/flake
hutian:~ $ pwd
/source
```

Run a single command without starting the interactive shell:

```console
$ nix run /path/to/this/flake -- echo hello
$ nix run /path/to/this/flake -- /bin/sh -c 'echo hello > result.txt'
```

Files written under `/source` appear directly in the host working directory.
`/tmp` is private to the sandbox, and the sandbox root is read-only.

## Shell behavior

The Haskell shell provides these builtins:

| Command | Description |
| --- | --- |
| `cd [dir]` | Change directory, defaulting to `$HOME`. |
| `pwd` | Print the current directory. |
| `llm [prompt]` | Send a prompt to llama.cpp and stream the response. |
| `help` | Show the builtin help. |
| `exit`, `quit` | Leave the shell. |

Everything else is executed with `/bin/sh -c` inside the sandbox.

`Ctrl+D` at the normal prompt exits the shell. `Ctrl+C` cancels the current
input, command, or LLM request without terminating the shell.

## LLM mode

Typing `llm` starts a multiline prompt. The background turns dark magenta while
you enter text; there is no textual prompt prefix.

- `Enter` starts a new line.
- `Backspace` and `Delete` edit the current line.
- `Ctrl+D` sends the accumulated message, even when the current line is not
  empty.
- `Ctrl+C` cancels the message without sending it and leaves the typed text
  visible.

```console
hutian:~ $ llm
Explain what a syscall is.
<Ctrl+D>
```

By default Hutian uses the OpenAI-compatible llama.cpp endpoint:

```text
http://127.0.0.1:8080/v1/chat/completions
```

Configure it with:

| Variable | Default | Description |
| --- | --- | --- |
| `HUTIAN_LLAMA_URL` | `http://127.0.0.1:8080/v1/chat/completions` | llama.cpp endpoint. A native `/completion` URL is also supported. |
| `HUTIAN_LLAMA_MODEL` | `default` | Model name sent to llama.cpp. |

The sandbox uses gVisor's `sandbox` network mode by default, which can reach a
server bound to the host's loopback interface.

## Adding packages

`lib.mkHutianShellFlake` builds Hutian for all supported systems from a single
package selector. Each selected package's runtime closure is mounted under
`/nix/store`, and its `bin` directory is added to `PATH`.

```nix
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.hutian.url = "path:/path/to/this/flake";

  outputs = { nixpkgs, hutian, ... }:
    hutian.lib.mkHutianShellFlake nixpkgs {
      packages = pkgs: with pkgs; [ coreutils git ];
    };
}
```

Then run `nix run .` from that project. Package versions come from the consumer
flake's lock file.

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
}
```

A complete consumer example is available in [`example.nix`](./example.nix).

## Using Nix inside the sandbox

Pass `nix = true` to include Nix and connect it to the host's Nix daemon:

```nix
outputs = { nixpkgs, hutian, ... }:
  hutian.lib.mkHutianShellFlake nixpkgs {
    packages = pkgs: with pkgs; [ coreutils git ];
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
| `HUTIAN_LLAMA_URL` | see above | llama.cpp endpoint. |
| `HUTIAN_LLAMA_MODEL` | `default` | llama.cpp model name. |

## Security model

- `runsc --rootless` runs the sandbox as your unprivileged host user inside a
  user namespace. The sandbox process is UID 0 inside that namespace, mapped to
  your host UID; it gains no host privileges.
- With the default `sandbox` network mode, the sandbox can reach the host's
  loopback interface. Set `GVISOR_NETWORK=none` to disable networking.
- Writes outside `/source`, `/tmp`, and the mounted package store are refused.
- With `nix = true`, the full host `/nix/store` and the Nix daemon socket are
  visible. Other host paths remain hidden.
- The sandbox uses the `systrap` platform by default. Set
  `GVISOR_PLATFORM=ptrace` if your host needs the `ptrace` platform.

## Project layout

```text
flake.nix                 Flake outputs and consumer helpers
nix/hutian.nix            Sandbox rootfs, launcher, and wrapper
nix/hutian-shell.nix      Haskell package definition
haskell-shell/            Haskell shell source and cabal file
example.nix               Minimal consumer flake
```

## Requirements

- Linux with unprivileged user namespaces enabled.
- x86_64 or aarch64.

If user namespaces are disabled, `runsc` reports an error. On systems with the
Debian-style sysctl this is enabled with:

```console
$ sudo sysctl kernel.unprivileged_userns_clone=1
```
