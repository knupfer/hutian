# gVisor shell

A Nix flake that drops you into a rootless [gVisor](https://gvisor.dev/) sandbox.

Only the current working directory and the package closures you explicitly
configure are shared with the sandbox. The working directory is mounted
read-write at `/source`; configured runtime closures are mounted read-only under
`/nix/store`. The sandbox root filesystem is based on Nix's minimal static
BusyBox shell (`busybox-sandbox-shell`), plus a small Haskell shell that provides
the interactive prompt. All other host paths are invisible.

The interactive shell is written in Haskell using Haskeline. It provides line
editing, history, and tab completion for executables found in `PATH`. External
commands are executed through the BusyBox `ash` inside the sandbox, so shell
builtins and redirection behave as usual. Configure packages (see below) to add
tools such as `ls`, `cat`, or `cp`.

`Ctrl+C` interrupts the current input, command, or LLM request without leaving
the shell. `Ctrl+D` at the normal shell prompt exits.

## Usage

```console
$ cd /some/project
$ nix run /path/to/this/flake
hutian:~ $ pwd
/source
```

You can also run a single command directly without the Haskell shell:

```console
$ nix run /path/to/this/flake -- echo hello
$ nix run /path/to/this/flake -- /bin/sh -c 'echo hello > result.txt'
```

Files created under `/source` are written directly to the current directory on
the host. `/tmp` is a private in-memory tmpfs, and the sandbox root is
read-only.

## Asking llama.cpp from the shell

Typing `llm` starts a multiline prompt. The background turns dark magenta while
you enter your text; there is no textual prompt prefix. Press `Ctrl+D` to send
everything to a llama.cpp server, even if the current line still contains text.
The response is streamed back into the terminal.

```console
hutian:~ $ llm
Explain what a syscall is.
<Ctrl+D>
```

Press `Ctrl+C` to cancel the input or an in-progress response and return to the
shell.

By default it uses the OpenAI-compatible endpoint:

```text
http://127.0.0.1:8080/v1/chat/completions
```

Set `HUTIAN_LLAMA_URL` to use a different endpoint, or `HUTIAN_LLAMA_MODEL` to
select a model if your llama.cpp server requires one (default: `default`). The
sandbox uses gVisor's default network mode so it can reach a server listening on
the host's loopback interface.

## Adding packages

`lib.mkHutianShellFlake` builds the shell for all supported systems from a
single package selector. The full runtime closure of every package (the package
itself plus all runtime dependencies) is bind-mounted under `/nix/store`, and
each package's `bin` output is added to `PATH`.

Create a flake in your project:

```nix
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.gvisor-shell.url = "path:/path/to/this/flake";

  outputs = { nixpkgs, gvisor-shell, ... }:
    gvisor-shell.lib.mkHutianShellFlake nixpkgs {
      packages = pkgs: with pkgs; [ coreutils git ];
    };
}
```

Then run `nix run .` from that project. The `nixpkgs` input is the first,
required argument, so package versions come from your project's lock file. To
use a custom package set, for example with overlays, pass `pkgsFor`:

```nix
pkgsFor = system:
  nixpkgs.legacyPackages.${system}.extend (final: prev: {
    # your overlay
  });
```

`pkgsFor` defaults to `system: nixpkgs.legacyPackages.${system}`.

For a single system or a custom `pkgs` instance, use the lower-level builder:

```nix
gvisor-shell.lib.mkHutianShell {
  inherit pkgs;
  packages = [ pkgs.git pkgs.jq ];
}
```

A complete copy of the minimal example is available in
[`example.nix`](./example.nix); copy it to your project as `flake.nix` and
adjust the input path.

## Using Nix in the sandbox

Pass `nix = true` to include Nix and let it talk to the host's Nix daemon:

```nix
outputs = { nixpkgs, gvisor-shell, ... }:
  gvisor-shell.lib.mkHutianShellFlake nixpkgs {
    packages = pkgs: with pkgs; [ coreutils git ];
    nix = true;
  };
```

The lower-level `lib.mkHutianShell` builder accepts the same option.

In this mode the host's Nix store and the Nix daemon socket are bind-mounted
into the sandbox. Commands such as `nix build`, `nix shell`, `nix run`, and
`nix develop` act as clients of the host daemon, so builds and substitutions run
on the host. Flakes and the `nix` command are enabled automatically.

## Properties

- Uses `runsc --rootless`, so the runtime and sandbox processes run as your
  unprivileged host user inside a user namespace. The sandbox process is UID 0
  inside that namespace, mapped to your host UID; it gains no host privileges.
- Uses gVisor's default `sandbox` network mode, which allows access to the host's
  loopback interface. Set `GVISOR_NETWORK=none` to disable networking, or
  `GVISOR_NETWORK=host` to use the host network namespace directly.
- Writes outside `/source`, `/tmp`, and the read-only package store are refused.
- With `nix = true`, the host's `/nix/store` is visible and the sandbox can
  connect to the host Nix daemon socket; other host paths remain hidden.
- The sandbox uses the gVisor `systrap` platform by default. Set
  `GVISOR_PLATFORM=ptrace` if your host needs the `ptrace` platform instead.

## Requirements

- Linux with unprivileged user namespaces enabled.
- x86_64 or aarch64.

If user namespaces are disabled, `runsc` will report an error. On systems with
the Debian-style sysctl this is enabled with:

```console
$ sudo sysctl kernel.unprivileged_userns_clone=1
```
