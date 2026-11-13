{
  pkgs,
  src,
  packages,
  systemPrompt,
  model,
  llamaUrl,
  readOnly,
  nix,
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

  relayEnabled = apiKeyFile != null || apiKeyCommand != null;

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
    ${optionalString relayEnabled ''
      mkdir -p "$out/run/hutian"
    ''}

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

  shell = pkgs.writeShellApplication {
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
      network="''${GVISOR_NETWORK:-sandbox}"
      saved_tty=$(stty -g 2>/dev/null || true)
      read_only=${if readOnly then "true" else "false"}
      nix_mode=${if nix then "true" else "false"}

      nixEnv=()
      nixVolumes=()
      nixRunscArgs=()

      relay_volumes=()
      relay_runsc_args=()
      shell_args=()

      ${optionalString relayEnabled ''
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
        }
        trap cleanup EXIT INT TERM HUP

        relay_parent="''${XDG_RUNTIME_DIR:-/tmp}"
        if [ ! -d "$relay_parent" ]; then
          relay_parent=/tmp
        fi
        relay_dir=$(mktemp -d "$relay_parent/hutian-relay.XXXXXX")
        chmod 700 "$relay_dir"
        relay_socket="$relay_dir/llm.sock"

        ${
          if apiKeyCommand == null then
            ''
              key_file=${escapeShellArg (if apiKeyFile == null then "" else apiKeyFile)}
              key_file=$(realpath -m -- "$key_file")
              case "$key_file" in
                ${builtins.storeDir}/* | "$sourcedir" | "$sourcedir"/*)
                  echo "hutian: apiKeyFile must not be inside the sandbox working directory or the Nix store" >&2
                  exit 1
                  ;;
              esac
              ${relayBin} \
                --key-file "$key_file" \
                --listen "$relay_socket" \
                --upstream ${escapeShellArg llamaUrl} &
            ''
          else
            ''
              # The command writes the key to the relay's stdin. It never appears
              # in an argument list or in the sandbox environment.
              eval ${escapeShellArg apiKeyCommand} | ${relayBin} \
                --key-stdin \
                --listen "$relay_socket" \
                --upstream ${escapeShellArg llamaUrl} &
            ''
        }
        relay_pid=$!

        attempt=0
        while [ "$attempt" -lt 200 ]; do
          if [ -S "$relay_socket" ]; then
            break
          fi
          if ! kill -0 "$relay_pid" 2>/dev/null; then
            echo "hutian: the LLM relay exited before creating its socket" >&2
            exit 1
          fi
          attempt=$((attempt + 1))
          sleep 0.05
        done
        if [ ! -S "$relay_socket" ]; then
          echo "hutian: the LLM relay did not create its socket" >&2
          exit 1
        fi

        # With a relay the sandbox needs no network at all. GVISOR_NETWORK can
        # still opt back in explicitly.
        network="''${GVISOR_NETWORK:-none}"
        relay_volumes=(-volume "$relay_dir:/run/hutian:ro")
        relay_runsc_args=(--host-uds=open)
        shell_args=(--llama-socket /run/hutian/llm.sock)
      ''}

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
          --llama-url ${escapeShellArg (if relayEnabled then relayLogicalUrl else llamaUrl)} \
          --model ${escapeShellArg model} \
          --system-prompt ${escapeShellArg systemPrompt} \
          "''${shell_args[@]}"
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
      if [ -n "''${saved_tty:-}" ]; then
        stty "$saved_tty" 2>/dev/null || true
      fi
      if [ -t 1 ]; then
        printf '\033[0m\033[?25h' || true
      fi
      exit "$status"
    '';
    meta = {
      description = "Run an unprivileged gVisor shell with a configured package closure";
      mainProgram = "hutian";
    };
  };
in
shell
