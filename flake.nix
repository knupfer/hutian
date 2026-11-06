{
  description = "An unprivileged, rootless gVisor shell with a configurable Nix package closure";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      defaultSystems = [ "x86_64-linux" "aarch64-linux" ];
      forEachSystem = nixpkgs.lib.genAttrs defaultSystems;

      # Build a gVisor shell whose sandbox has:
      #   - a minimal static BusyBox shell as the root filesystem
      #   - the current directory mounted read-write at /source
      #   - the runtime closures of `packages` bind-mounted under /nix/store
      #
      # With `nix = true`, Nix is added to the sandbox. The host Nix store and
      # daemon socket are exposed as well, so Nix commands can use the host
      # daemon for evaluation, builds, and substitutions.
      mkHutianShell = { pkgs, packages, nix ? false }:
        let
          lib = pkgs.lib;

          allPackages = packages ++ lib.optional nix pkgs.nix;

          hutianShellPackage = pkgs.haskellPackages.mkDerivation {
            pname = "hutian-shell";
            version = "0.1.0.0";
            src = ./haskell-shell;
            isLibrary = false;
            isExecutable = true;
            executableHaskellDepends = with pkgs.haskellPackages; [
              aeson
              bytestring
              containers
              directory
              exceptions
              filepath
              haskeline
              http-client
              http-types
              process
              text
              unix
              vector
            ];
            doHaddock = false;
            doCheck = false;
          };

          # Every store path needed at runtime, including dependencies. Nix
          # needs a CA bundle for HTTPS fetches performed by the client.
          closure = pkgs.closureInfo {
            rootPaths = allPackages ++ [ hutianShellPackage ] ++ lib.optional nix pkgs.cacert;
          };
          binPath = lib.makeBinPath allPackages;

          storeDir = builtins.storeDir;
          stateDir = "${builtins.dirOf builtins.storeDir}/var/nix";

          rootfs = pkgs.runCommand "hutian-rootfs" { } ''
            mkdir -p "$out/bin" "$out/source" "$out/nix/store" "$out/etc"
            cp -a ${pkgs.busybox-sandbox-shell}/bin/. "$out/bin/"
            printf '127.0.0.1 localhost\n::1 localhost\n' > "$out/etc/hosts"

            # gVisor needs the mountpoints to exist in the read-only rootfs.
            while IFS= read -r path; do
              mkdir -p "$out/nix/store/''${path##*/}"
            done < ${closure}/store-paths

            ${lib.optionalString nix ''
              # Mountpoint for the host's Nix daemon socket.
              mkdir -p "$out${stateDir}/daemon-socket"
            ''}
          '';

          shell = pkgs.writeShellApplication {
            name = "hutian";
            runtimeInputs = [ pkgs.gvisor pkgs.coreutils ];
            text = ''
              rootfs=${rootfs}
              closure=${closure}
              sourcedir=$(pwd -P)
              platform="''${GVISOR_PLATFORM:-systrap}"
              network="''${GVISOR_NETWORK:-sandbox}"
              saved_tty=$(stty -g 2>/dev/null || true)

              # Mount the entire runtime closure of the configured packages,
              # using the same paths inside and outside the sandbox.
              volumes=()
              while IFS= read -r path; do
                volumes+=(-volume "$path:$path")
              done < "$closure/store-paths"

              nixEnv=()
              nixVolumes=()
              nixRunscArgs=()
              ${lib.optionalString nix ''
                # Expose the host store and, when available, its daemon socket.
                # Builds and substitutions are performed by the host daemon;
                # the sandbox only reads the resulting store paths.
                nixVolumes+=(-volume "${storeDir}:${storeDir}")
                if [ -S "${stateDir}/daemon-socket/socket" ]; then
                  nixVolumes+=(-volume "${stateDir}/daemon-socket:${stateDir}/daemon-socket")
                  nixRunscArgs+=(--host-uds=open)
                  nixEnv+=(
                    NIX_REMOTE=daemon
                    NIX_STATE_DIR=${stateDir}
                  )
                fi

                # Flakes are an experimental feature; keep any settings the
                # caller supplied through NIX_CONFIG.
                nixEnv+=(
                  NIX_CONFIG="experimental-features = nix-command flakes''${NIX_CONFIG:+
$NIX_CONFIG}"
                  # The sandbox rootfs has no /etc/ssl; point Nix at the CA
                  # bundle from the store instead.
                  NIX_SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
                  # Keep Nix's evaluation cache out of the shared project
                  # directory, since HOME points at /source.
                  XDG_CACHE_HOME=/tmp/nix-cache
                )
              ''}

              if [ "$#" -eq 0 ]; then
                set -- ${hutianShellPackage}/bin/hutian-shell
              else
                # shellcheck disable=SC2016
                set -- /bin/sh -c 'cd /source && HOME=/source PATH="/bin''${GVISOR_PATH:+:$GVISOR_PATH}"; export HOME PATH; "$@"' sh "$@"
              fi

              set +e
              env -i \
                PATH=/bin:${binPath} \
                HOME=/source \
                GVISOR_PATH=${binPath} \
                TERM="''${TERM:-xterm-256color}" \
                LANG="''${LANG:-C.UTF-8}" \
                TMPDIR=/tmp \
                HUTIAN_WORKDIR=/source \
                HUTIAN_LLAMA_URL="''${HUTIAN_LLAMA_URL:-http://127.0.0.1:8080/v1/chat/completions}" \
                HUTIAN_LLAMA_MODEL="''${HUTIAN_LLAMA_MODEL:-default}" \
                "''${nixEnv[@]}" \
                "$(command -v runsc)" \
                  --rootless \
                  --network "$network" \
                  "''${nixRunscArgs[@]}" \
                  --platform "$platform" \
                  'do' \
                    -quiet \
                    -root "$rootfs" \
                    -cwd / \
                    -force-overlay=false \
                    "''${volumes[@]}" \
                    "''${nixVolumes[@]}" \
                    -volume "$sourcedir:/source" \
                    "$@"
              status=$?
              set -e

              # Make sure the terminal is usable again even if the sandbox
              # exits while it is in raw mode.
              if [ -n "''${saved_tty:-}" ]; then
                stty "$saved_tty" 2>/dev/null || true
              fi
              if [ -t 1 ]; then
                printf '\033[0m\033[?25h' || true
              fi
              exit "$status"
            '';
          };
        in
        shell;

      # Convenience helper for consumer flakes: derive packages/apps for every
      # system from a single package selector.
      mkHutianShellFlake = nixpkgs: {
        packages,
        nix ? false,
        systems ? defaultSystems,
        pkgsFor ? (system: nixpkgs.legacyPackages.${system}),
      }:
        let
          lib = nixpkgs.lib;

          mkPackage = system:
            let
              pkgs = pkgsFor system;
            in
            mkHutianShell {
              inherit pkgs nix;
              packages = packages pkgs;
            };
        in
        {
          packages = lib.genAttrs systems (system: {
            default = mkPackage system;
          });

          apps = lib.genAttrs systems (system: {
            default = {
              type = "app";
              program = "${mkPackage system}/bin/hutian";
              meta.description = "Run an unprivileged gVisor shell with the configured packages";
            };
          });
        };
    in
    {
      # Import this from another flake:
      #
      #   gvisor-shell.lib.mkHutianShell {
      #     inherit pkgs;
      #     packages = [ pkgs.git pkgs.jq ];
      #   }
      #
      # Or, for a minimal consumer flake:
      #
      #   outputs = { nixpkgs, gvisor-shell, ... }:
      #     gvisor-shell.lib.mkHutianShellFlake nixpkgs {
      #       packages = pkgs: [ pkgs.git pkgs.jq ];
      #     };
      lib = { inherit mkHutianShell mkHutianShellFlake; };

      packages = forEachSystem (system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        {
          default = mkHutianShell { inherit pkgs; packages = [];};
        });

      apps = forEachSystem (system:
        {
          default = {
            type = "app";
            program = "${self.packages.${system}.default}/bin/hutian";
            meta.description = "Run an unprivileged gVisor shell with a package closure";
          };
        });
    };
}
