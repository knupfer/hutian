let
  requiredOption = import ./required-option.nix;
in
{
  pkgs,
  packages ? [ ],
  systemPrompt ?
    requiredOption "mkHutianShell" "systemPrompt"
      ''systemPrompt = "You are a coding assistant.";'',
  model ? requiredOption "mkHutianShell" "model" ''model = "your-model";'',
  llamaUrl ?
    requiredOption "mkHutianShell" "llamaUrl"
      ''llamaUrl = "http://127.0.0.1:8080/v1/chat/completions";'',
  readOnly ? requiredOption "mkHutianShell" "readOnly" "readOnly = false;",
  nix ? false,
  src ? ../.,
}:

let
  inherit (pkgs) lib;
  inherit (builtins) dirOf storeDir;

  allPackages = packages ++ lib.optional nix pkgs.nix;

  hutianShell = pkgs.callPackage ./hutian-shell.nix { inherit src; };

  readOnlyWrapper = import ./read-only.nix { inherit pkgs; };

  # The runtime closure is mounted at the same paths inside the sandbox. Nix
  # additionally needs a CA bundle for HTTPS fetches performed by the client.
  closure = pkgs.closureInfo {
    rootPaths = allPackages ++ [ hutianShell ] ++ lib.optional nix pkgs.cacert;
  };

  binPath = lib.makeBinPath allPackages;

  stateDir = "${dirOf storeDir}/var/nix";

  rootfs = pkgs.runCommand "hutian-rootfs" { } ''
    mkdir -p "$out/bin" "$out/source" "$out/etc" "$out${storeDir}"
    cp -a ${pkgs.busybox-sandbox-shell}/bin/. "$out/bin/"
    printf '127.0.0.1 localhost\n::1 localhost\n' > "$out/etc/hosts"

    # gVisor requires volume mountpoints to exist in the read-only rootfs.
    while IFS= read -r path; do
      mkdir -p "$out$path"
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

      volumes=()
      while IFS= read -r path; do
        volumes+=(-volume "$path:$path")
      done < "$closure/store-paths"

      nixEnv=()
      nixVolumes=()
      nixRunscArgs=()
      ${lib.optionalString nix ''
                # Expose the host store and, when present, the Nix daemon socket.
                # Builds are performed by the host daemon; the sandbox only reads the
                # resulting store paths.
                nixVolumes+=(-volume "${storeDir}:${storeDir}")
                if [ -S "${stateDir}/daemon-socket/socket" ]; then
                  nixVolumes+=(-volume "${stateDir}/daemon-socket:${stateDir}/daemon-socket")
                  nixRunscArgs+=(--host-uds=open)
                  nixEnv+=(
                    NIX_REMOTE=daemon
                    NIX_STATE_DIR=${stateDir}
                  )
                fi

                nixEnv+=(
                  NIX_CONFIG="experimental-features = nix-command flakes''${NIX_CONFIG:+
        $NIX_CONFIG}"
                  NIX_SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
                  XDG_CACHE_HOME=/tmp/nix-cache
                )
      ''}

      if [ "$#" -eq 0 ]; then
        set -- ${hutianShell}/bin/hutian-shell
      else
        # shellcheck disable=SC2016
        set -- /bin/sh -c 'cd /source && HOME=/source PATH="/bin''${GVISOR_PATH:+:$GVISOR_PATH}"; export HOME PATH; "$@"' sh "$@"
      fi

      sandbox=(
        env -i
        PATH=/bin:${binPath}
        HOME=/source
        GVISOR_PATH=${binPath}
        TERM="''${TERM:-xterm-256color}"
        LANG="''${LANG:-C.UTF-8}"
        TMPDIR=/tmp
        HUTIAN_WORKDIR=/source
        HUTIAN_LLAMA_URL=${lib.escapeShellArg llamaUrl}
        HUTIAN_LLAMA_MODEL=${lib.escapeShellArg model}
        HUTIAN_SYSTEM_PROMPT=${lib.escapeShellArg systemPrompt}
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
            "''${volumes[@]}"
            "''${nixVolumes[@]}"
            -volume "$sourcedir:/source"
            "$@"
      )

      status=0
      ${lib.optionalString readOnly ''
        ${readOnlyWrapper} "$sourcedir" "''${sandbox[@]}" || status=$?
      ''}
      ${lib.optionalString (!readOnly) ''
        "''${sandbox[@]}" || status=$?
      ''}

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
