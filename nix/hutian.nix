# Internal package builder. `flake.nix` validates the options before calling
# this module, so the values arriving here are already checked.
{
  pkgs,
  src,
  packages,
  systemPrompt,
  modelName,
  llamaUrl,
  writable,
  nixDaemon,
  network,
  modelShell,
  apiKeyCommand ? null,
}:

let
  inherit (pkgs) lib;
  inherit (builtins) dirOf storeDir;
  inherit (lib)
    boolToString
    escapeShellArg
    makeBinPath
    optional
    optionalString
    ;

  # `packages` is the `sandbox.packages` selector from the flake options.
  selectedPackages = packages pkgs;
  allPackages = selectedPackages ++ optional nixDaemon pkgs.nix;

  hutianShell = pkgs.callPackage ./hutian-shell.nix { inherit src; };
  sandboxLauncher = import ./sandbox.nix { inherit pkgs; };
  relayBin = "${hutianShell}/bin/hutian-llm-relay";

  # The shell only needs the path to choose the chat or completion request
  # shape. Replacing the authority keeps a URL's credentials, upstream host,
  # and query or fragment parameters out of the sandbox.
  relayUrlPath =
    let
      captures = builtins.match "[a-zA-Z][a-zA-Z0-9+.-]*://[^/?#]*(/[^?#]*)?([?#].*)?" llamaUrl;
      path = if captures == null then null else builtins.head captures;
    in
    if path == null then "" else path;
  relayLogicalUrl = "http://hutian-relay" + relayUrlPath;

  # The relay is always present. Without a configured key it forwards without
  # an Authorization header and never reads standard input.
  relayLaunch =
    if apiKeyCommand != null then
      ''
        # Run the command in the foreground so its password prompt keeps
        # Ctrl+C and the terminal. The key is captured in a shell variable
        # and piped to the relay, so it stays out of argv and the environment.
        # The relay creates its socket only after the key is available, so the
        # launcher waits here rather than timing out.
        key=$(eval ${escapeShellArg apiKeyCommand})
        printf '%s' "$key" | ${relayBin} \
          --listen "$relay_socket" \
          --upstream ${escapeShellArg llamaUrl} &
        unset key
      ''
    else
      ''
        # No key is configured, so the relay forwards without Authorization.
        ${relayBin} \
          --listen "$relay_socket" \
          --upstream ${escapeShellArg llamaUrl} \
          --no-auth &
      '';

  # The runtime closure is mounted at the same paths inside the sandbox. The CA
  # bundle lets the HTTP client verify HTTPS model endpoints; Nix additionally
  # uses it for HTTPS fetches performed by the client.
  closure = pkgs.closureInfo {
    rootPaths = allPackages ++ [
      hutianShell
      pkgs.cacert
    ];
  };

  binPath = makeBinPath allPackages;
  pathEnv = "/bin" + optionalString (binPath != "") ":${binPath}";

  stateDir = "${dirOf storeDir}/var/nix";

  # Nix normally downloads the global flake registry before resolving an
  # indirect reference such as `nixpkgs#...`. Point it at a local registry
  # instead, so an offline sandbox can still resolve the flake. The registry
  # contains only `nixpkgs`, mapped to the nixpkgs input configured here; the
  # host's system and user registries are not exposed.
  nixRegistry = pkgs.writeText "hutian-flake-registry.json" (
    builtins.toJSON {
      version = 2;
      flakes = [
        {
          exact = true;
          from = {
            type = "indirect";
            id = "nixpkgs";
          };
          to = {
            type = "path";
            path = toString pkgs.path;
          };
        }
      ];
    }
  );

  rootfs = pkgs.runCommand "hutian-rootfs" { } ''
    mkdir -p "$out/bin" "$out/source" "$out/etc" "$out${storeDir}"
    cp -a ${pkgs.busybox-sandbox-shell}/bin/. "$out/bin/"
    # cp -a copies the read-only store mode onto $out/bin, so make it writable
    # before adding the init.
    chmod u+w "$out/bin"
    # A tiny static init that reaps orphaned background jobs as PID 1.
    cp ${pkgs.pkgsStatic.dumb-init}/bin/dumb-init "$out/bin/dumb-init"
    chmod 755 "$out/bin/dumb-init"
    chmod 555 "$out/bin"
    printf '127.0.0.1 localhost\n::1 localhost\n' > "$out/etc/hosts"

    # Mountpoint for the host-side LLM relay socket. The launcher mounts a
    # private per-session directory here; no key material is ever inside it.
    mkdir -p "$out/run/hutian"
    # Mountpoint for the short-lived model and system-prompt files.
    mkdir -p "$out/run/hutian-config"

    # gVisor recursively binds the rootfs into the sandbox, so the launcher can
    # bind each closure path under its mountpoint instead of passing one gVisor
    # volume per path. Mountpoints must have the same type as the store path.
    while IFS= read -r path; do
      if [ -d "$path" ]; then
        mkdir -p "$out$path"
      else
        mkdir -p "$(dirname "$out$path")"
        touch "$out$path"
      fi
    done < ${closure}/store-paths

    ${optionalString nixDaemon ''
      # Mountpoint for the host Nix daemon socket.
      mkdir -p "$out${stateDir}/daemon-socket"

      # Nix reads this as the user registry, so `nixpkgs` resolves to the
      # nixpkgs input of the flake that configured Hutian. The global registry
      # points at the same local file so Nix never downloads one, and the host
      # registries contribute nothing.
      mkdir -p "$out/run/hutian-nix/nix"
      cp ${nixRegistry} "$out/run/hutian-nix/nix/registry.json"
    ''}
  '';

  launcher = pkgs.writeShellApplication {
    name = "hutian";
    runtimeInputs = with pkgs; [
      coreutils
      gvisor
    ];
    text = ''
      rootfs=${rootfs}
      closure=${closure}
      source_dir=$(pwd -P)
      platform="''${GVISOR_PLATFORM:-systrap}"
      saved_tty=$(stty -g 2>/dev/null || true)
      writable=${boolToString writable}
      nix_daemon=${boolToString nixDaemon}

      # The network option is a hard gate. Without it the sandbox gets no
      # network regardless of GVISOR_NETWORK; with it, GVISOR_NETWORK can still
      # narrow the mode. gVisor does not support sandbox networking with
      # --rootless and silently falls back to the host namespace, so use host
      # explicitly rather than relying on that fallback.
      ${
        if network then
          ''
            network="''${GVISOR_NETWORK:-host}"
          ''
        else
          ''
            unset GVISOR_NETWORK
            network=none
          ''
      }

      # A key command or the sandbox may leave the terminal in raw or no-echo
      # mode. Restore the saved settings on every exit path.
      # shellcheck disable=SC2329
      restore_terminal() {
        if [ -n "''${saved_tty:-}" ]; then
          stty "$saved_tty" 2>/dev/null || true
        fi
        if [ -t 1 ]; then
          printf '\033[0m\033[?25h' || true
        fi
      }

      # Create a directory that only the current user can access.
      private_dir() {
        local dir
        dir=$(mktemp -d "$relay_parent/$1.XXXXXX")
        chmod 700 "$dir"
        printf '%s\n' "$dir"
      }

      nix_env=()
      nix_volumes=()
      runsc_args=()
      config_volumes=()
      relay_runsc_args=()
      llm_args=()

      relay_pid=""
      relay_dir=""
      config_dir=""

      # shellcheck disable=SC2329
      cleanup() {
        if [ -n "''${relay_pid:-}" ]; then
          kill "$relay_pid" 2>/dev/null || true
          wait "$relay_pid" 2>/dev/null || true
        fi
        if [ -n "''${relay_dir:-}" ]; then
          rm -rf "$relay_dir"
        fi
        if [ -n "''${config_dir:-}" ]; then
          rm -rf "$config_dir"
        fi
        restore_terminal
      }
      trap cleanup EXIT
      # While the relay starts, Ctrl+C aborts the launcher. Once the sandbox
      # is running, the sandbox owns the terminal again.
      trap 'exit 130' INT TERM HUP

      # gVisor never revalidates cached directory entries against host changes.
      # Both /source and, with the host daemon, /nix/store can change outside
      # the sandbox while it runs. Without this, files created by the user's
      # editor stay invisible and writes to those names fail with ENOENT,
      # while paths built by the host daemon cannot be executed. Disable the
      # dentry cache so lookups always go back to the host.
      runsc_args+=(--dcache=0)

      relay_parent="''${XDG_RUNTIME_DIR:-/tmp}"
      [ -d "$relay_parent" ] || relay_parent=/tmp
      relay_dir=$(private_dir hutian-relay)
      relay_socket="$relay_dir/llm.sock"

      ${relayLaunch}
      relay_pid=$!

      while [ ! -S "$relay_socket" ]; do
        if ! kill -0 "$relay_pid" 2>/dev/null; then
          echo "hutian: the LLM relay exited before creating its socket" >&2
          exit 1
        fi
        sleep 0.05
      done

      # The sandbox handles Ctrl+C from here on.
      trap cleanup INT TERM HUP

      # The sandbox helper mounts the relay directory read-only. The socket is
      # still connectable, but a shell command cannot unlink or replace it.
      relay_runsc_args=(--host-uds=open)
      llm_args=(--llama-socket /run/hutian/llm.sock --model-shell ${escapeShellArg modelShell})

      ${optionalString nixDaemon ''
        # The launcher exposes the host store in the rootfs. Mount the Nix
        # daemon socket when present. Builds are performed by the host daemon;
        # the sandbox only reads the resulting store paths.
        if [ -S "${stateDir}/daemon-socket/socket" ]; then
          nix_volumes+=(-volume "${stateDir}/daemon-socket:${stateDir}/daemon-socket")
          runsc_args+=(--host-uds=open)
          nix_env+=(
            NIX_REMOTE=daemon
            NIX_STATE_DIR=${stateDir}
          )
        fi

        nix_env+=(
          NIX_CONFIG="experimental-features = nix-command flakes
            flake-registry = file:///run/hutian-nix/nix/registry.json"
          NIX_SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
          XDG_CACHE_HOME=/tmp/nix-cache
          XDG_CONFIG_HOME=/run/hutian-nix
        )
      ''}

      if [ "$#" -eq 0 ]; then
        # Keep the model name and system prompt out of the process listing.
        # The shell reads these files once and removes them before it can run
        # any command.
        config_dir=$(private_dir hutian-config)
        printf '%s' ${escapeShellArg modelName} > "$config_dir/model"
        printf '%s' ${escapeShellArg systemPrompt} > "$config_dir/system-prompt"
        chmod 600 "$config_dir/model" "$config_dir/system-prompt"
        config_volumes=(-volume "$config_dir:/run/hutian-config")

        set -- ${hutianShell}/bin/hutian-shell \
          --llama-url ${escapeShellArg relayLogicalUrl} \
          --model-file /run/hutian-config/model \
          --system-prompt-file /run/hutian-config/system-prompt \
          "''${llm_args[@]}"
      fi

      # runsc resolves -cwd on the host, so start in / and enter /source after
      # the launcher has mounted the working directory there. dumb-init runs as
      # PID 1 so orphaned background jobs are reaped instead of staying as
      # zombies for the life of the sandbox. Its default setsid mode detaches
      # from the controlling terminal, which gVisor does not reattach, so
      # --single-child keeps /dev/tty available for interactive programs.
      # shellcheck disable=SC2016
      set -- /bin/dumb-init --single-child -- /bin/sh -c 'cd /source && "$@"' sh "$@"

      sandbox=(
        env -i
        "PATH=${pathEnv}"
        HOME=/source
        TERM=xterm-256color
        LANG=C.UTF-8
        TMPDIR=/tmp
        "SYSTEM_CERTIFICATE_PATH=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
        "''${nix_env[@]}"
        "$(command -v runsc)"
          --rootless
          --network "$network"
          "''${runsc_args[@]}"
          "''${relay_runsc_args[@]}"
          --platform "$platform"
          'do'
            -quiet
            -root "$rootfs"
            -cwd /
            -force-overlay=false
            "''${nix_volumes[@]}"
            "''${config_volumes[@]}"
            "$@"
      )

      # gVisor reserves FDs from 256 for stdio, so passing one volume per store
      # path fails once a closure has more than about 250 paths. Instead, mount
      # the closure and working directory into the rootfs in a private mount
      # namespace and let gVisor recursively bind the rootfs into the sandbox.
      status=0
      ${pkgs.util-linux}/bin/unshare --user --map-root-user --mount --propagation private \
        ${sandboxLauncher} "$rootfs" "$closure" "$source_dir" "$writable" "$nix_daemon" "$relay_dir" \
        "''${sandbox[@]}" || status=$?

      exit "$status"
    '';
    meta = {
      description = "Run an unprivileged gVisor shell with a configured package closure";
      mainProgram = "hutian";
    };
  };
in
launcher
