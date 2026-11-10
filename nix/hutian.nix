{
  pkgs,
  src,
  packages,
  systemPrompt,
  model,
  llamaUrl,
  readOnly,
  nix,
}:

let
  inherit (pkgs) lib;
  inherit (builtins) dirOf storeDir;

  allPackages = packages ++ lib.optional nix pkgs.nix;

  hutianShell = pkgs.callPackage ./hutian-shell.nix { inherit src; };

  sandboxLauncher = import ./sandbox.nix { inherit pkgs; };

  # The runtime closure is mounted at the same paths inside the sandbox. Nix
  # additionally needs a CA bundle for HTTPS fetches performed by the client.
  closure = pkgs.closureInfo {
    rootPaths = allPackages ++ [ hutianShell ] ++ lib.optional nix pkgs.cacert;
  };

  binPath = lib.makeBinPath allPackages;
  pathEnv = "/bin" + lib.optionalString (binPath != "") ":${binPath}";

  stateDir = "${dirOf storeDir}/var/nix";

  rootfs = pkgs.runCommand "hutian-rootfs" { } ''
    mkdir -p "$out/bin" "$out/source" "$out/etc" "$out${storeDir}"
    cp -a ${pkgs.busybox-sandbox-shell}/bin/. "$out/bin/"
    printf '127.0.0.1 localhost\n::1 localhost\n' > "$out/etc/hosts"

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

    ${lib.optionalString nix ''
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
      ${lib.optionalString nix ''
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
          --llama-url ${lib.escapeShellArg llamaUrl} \
          --model ${lib.escapeShellArg model} \
          --system-prompt ${lib.escapeShellArg systemPrompt}
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
        "''${nixEnv[@]}"
        "$(command -v runsc)"
          --rootless
          --network "$network"
          "''${nixRunscArgs[@]}"
          --platform "$platform"
          'do'
            -quiet
            -root "$rootfs"
            -cwd /
            -force-overlay=false
            "''${nixVolumes[@]}"
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
