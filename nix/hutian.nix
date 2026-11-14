{
  pkgs,
  src,
  packages,
  systemPrompt,
  model,
  llamaUrl,
  readOnly,
  nix,
  internet,
  modelShell,
  apiKeyFile ? null,
  apiKeyCommand ? null,
}:

assert pkgs.lib.assertMsg (builtins.isList packages) "hutian: packages must be a list";
assert pkgs.lib.assertMsg (builtins.isString systemPrompt) "hutian: systemPrompt must be a string";
assert pkgs.lib.assertMsg (
  builtins.isString model && model != ""
) "hutian: model must be a non-empty string";
assert pkgs.lib.assertMsg (
  builtins.isString llamaUrl && llamaUrl != ""
) "hutian: llamaUrl must be a non-empty string";
assert pkgs.lib.assertMsg (builtins.isBool readOnly) "hutian: readOnly must be a boolean";
assert pkgs.lib.assertMsg (builtins.isBool nix) "hutian: nix must be a boolean";
assert pkgs.lib.assertMsg (builtins.isBool internet) "hutian: internet must be a boolean";
assert pkgs.lib.assertMsg (
  builtins.isString modelShell
  && builtins.elem modelShell [
    "deny"
    "ask"
    "allow"
  ]
) "hutian: modelShell must be \"deny\", \"ask\", or \"allow\"";
assert pkgs.lib.assertMsg (
  apiKeyFile == null || (builtins.isString apiKeyFile && apiKeyFile != "")
) "hutian: apiKeyFile must be a non-empty string";
assert pkgs.lib.assertMsg (
  apiKeyCommand == null || (builtins.isString apiKeyCommand && apiKeyCommand != "")
) "hutian: apiKeyCommand must be a non-empty string";
assert pkgs.lib.assertMsg (
  apiKeyFile == null || apiKeyCommand == null
) "hutian: apiKeyFile and apiKeyCommand are mutually exclusive";
assert pkgs.lib.assertMsg (
  apiKeyFile == null || !(pkgs.lib.hasPrefix builtins.storeDir apiKeyFile)
) "hutian: apiKeyFile must not be under the Nix store; the sandbox can read the store";

let
  inherit (pkgs) lib;
  inherit (builtins) dirOf storeDir;

  inherit (lib)
    escapeShellArg
    makeBinPath
    optional
    optionalString
    ;

  allPackages = packages ++ optional nix pkgs.nix;

  hutianShell = pkgs.callPackage ./hutian-shell.nix { inherit src; };

  sandboxLauncher = import ./sandbox.nix { inherit pkgs; };

  relayBin = "${hutianShell}/bin/hutian-llm-relay";

  # The shell only needs the path to choose the chat or completion request
  # shape. Replacing the authority keeps a URL that contains credentials, and
  # the upstream host itself, out of the sandbox.
  llamaUrlSuffix =
    let
      matched = builtins.match "[a-zA-Z][a-zA-Z0-9+.-]*://[^/?]*(.*)" llamaUrl;
    in
    if matched == null then "" else builtins.head matched;
  relayLogicalUrl = "http://hutian-relay" + llamaUrlSuffix;

  # The relay is always present. Without a configured key it forwards without
  # an Authorization header and never reads standard input.
  relayLaunch =
    if apiKeyFile != null then
      ''
        key_file=${escapeShellArg apiKeyFile}
        key_file=$(realpath -m -- "$key_file")
        case "$key_file" in
          ${builtins.storeDir}/* | "$sourcedir" | "$sourcedir"/*)
            echo "hutian: apiKeyFile must not be inside the sandbox working directory or the Nix store" >&2
            exit 1
            ;;
        esac
        if [ ! -r "$key_file" ]; then
          echo "hutian: cannot read apiKeyFile: $key_file" >&2
          exit 1
        fi
        # The relay reads the key from stdin, so it never appears in argv.
        cat -- "$key_file" | ${relayBin} \
          --listen "$relay_socket" \
          --upstream ${escapeShellArg llamaUrl} &
      ''
    else if apiKeyCommand != null then
      ''
        # Run the command in the foreground so its password prompt keeps
        # Ctrl+C and the terminal. The key is captured in a shell variable
        # and piped to the relay, so it stays out of argv and the
        # environment. The relay creates its socket only after the key is
        # available, so the launcher waits here rather than timing out.
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

  rootfs = pkgs.runCommand "hutian-rootfs" { } ''
    mkdir -p "$out/bin" "$out/source" "$out/etc" "$out${storeDir}"
    cp -a ${pkgs.busybox-sandbox-shell}/bin/. "$out/bin/"
    printf '127.0.0.1 localhost\n::1 localhost\n' > "$out/etc/hosts"

    # Mountpoint for the host-side LLM relay socket. The launcher mounts a
    # private per-session directory here; no key material is ever inside it.
    mkdir -p "$out/run/hutian"

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

    ${optionalString nix ''
      # Mountpoint for the host Nix daemon socket.
      mkdir -p "$out${stateDir}/daemon-socket"
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
      sourcedir=$(pwd -P)
      platform="''${GVISOR_PLATFORM:-systrap}"
      saved_tty=$(stty -g 2>/dev/null || true)
      read_only=${if readOnly then "true" else "false"}
      nix_mode=${if nix then "true" else "false"}

      # The internet option is a hard gate. Without it the sandbox gets no
      # network regardless of GVISOR_NETWORK; with it, GVISOR_NETWORK can still
      # narrow the mode. gVisor does not support sandbox networking with
      # --rootless and silently falls back to the host namespace, so use host
      # explicitly rather than relying on that fallback.
      ${
        if internet then
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
      restore_terminal() {
        if [ -n "''${saved_tty:-}" ]; then
          stty "$saved_tty" 2>/dev/null || true
        fi
        if [ -t 1 ]; then
          printf '\033[0m\033[?25h' || true
        fi
      }

      nixEnv=()
      nixVolumes=()
      nixRunscArgs=()

      relay_volumes=()
      relay_runsc_args=()
      llm_args=()

      relay_pid=""
      relay_dir=""

      # shellcheck disable=SC2329
      cleanup() {
        if [ -n "''${relay_pid:-}" ]; then
          kill "$relay_pid" 2>/dev/null || true
          wait "$relay_pid" 2>/dev/null || true
        fi
        if [ -n "''${relay_dir:-}" ]; then
          rm -rf "$relay_dir"
        fi
        restore_terminal
      }
      trap cleanup EXIT
      # While the relay starts, Ctrl+C aborts the launcher. Once the sandbox
      # is running, the sandbox owns the terminal again.
      trap 'exit 130' INT TERM HUP

      relay_parent="''${XDG_RUNTIME_DIR:-/tmp}"
      if [ ! -d "$relay_parent" ]; then
        relay_parent=/tmp
      fi
      relay_dir=$(mktemp -d "$relay_parent/hutian-relay.XXXXXX")
      chmod 700 "$relay_dir"
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

      relay_volumes=(-volume "$relay_dir:/run/hutian")
      relay_runsc_args=(--host-uds=open)
      llm_args=(--llama-socket /run/hutian/llm.sock --model-shell ${escapeShellArg modelShell})

      ${optionalString nix ''
        # The launcher exposes the host store in the rootfs. Mount the Nix
        # daemon socket when present. Builds are performed by the host daemon;
        # the sandbox only reads the resulting store paths.
        if [ -S "${stateDir}/daemon-socket/socket" ]; then
          nixVolumes+=(-volume "${stateDir}/daemon-socket:${stateDir}/daemon-socket")
          nixRunscArgs+=(--host-uds=open)
          nixEnv+=(
            NIX_REMOTE=daemon
            NIX_STATE_DIR=${stateDir}
          )
        fi

        nixEnv+=(
          NIX_CONFIG="experimental-features = nix-command flakes"
          NIX_SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
          XDG_CACHE_HOME=/tmp/nix-cache
        )
      ''}

      if [ "$#" -eq 0 ]; then
        set -- ${hutianShell}/bin/hutian-shell \
          --llama-url ${escapeShellArg relayLogicalUrl} \
          --model ${escapeShellArg model} \
          --system-prompt ${escapeShellArg systemPrompt} \
          "''${llm_args[@]}"
      fi

      # runsc resolves -cwd on the host, so start in / and enter /source after
      # the launcher has mounted the working directory there.
      # shellcheck disable=SC2016
      set -- /bin/sh -c 'cd /source && "$@"' sh "$@"

      sandbox=(
        env -i
        PATH=${pathEnv}
        HOME=/source
        TERM=xterm-256color
        LANG=C.UTF-8
        TMPDIR=/tmp
        SYSTEM_CERTIFICATE_PATH=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
        "''${nixEnv[@]}"
        "$(command -v runsc)"
          --rootless
          --network "$network"
          "''${nixRunscArgs[@]}"
          "''${relay_runsc_args[@]}"
          --platform "$platform"
          'do'
            -quiet
            -root "$rootfs"
            -cwd /
            -force-overlay=false
            "''${nixVolumes[@]}"
            "''${relay_volumes[@]}"
            "$@"
      )

      # gVisor reserves FDs from 256 for stdio, so passing one volume per store
      # path fails once a closure has more than about 250 paths. Instead, mount
      # the closure and working directory into the rootfs in a private mount
      # namespace and let gVisor recursively bind the rootfs into the sandbox.
      status=0
      ${pkgs.util-linux}/bin/unshare --user --map-root-user --mount --propagation private \
        ${sandboxLauncher} "$rootfs" "$closure" "$sourcedir" "$read_only" "$nix_mode" \
        "''${sandbox[@]}" || status=$?

      # Keep the host terminal usable even if the sandbox leaves it in raw mode.
      restore_terminal
      exit "$status"
    '';
    meta = {
      description = "Run an unprivileged gVisor shell with a configured package closure";
      mainProgram = "hutian";
    };
  };
in
launcher
