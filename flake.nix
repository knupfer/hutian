{
  description = "A rootless gVisor sandbox with a Haskell shell and configurable Nix closure";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { self, nixpkgs }:
    let
      inherit (nixpkgs) lib;

      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      forAllSystems = lib.genAttrs systems;
      pkgsFor = system: nixpkgs.legacyPackages.${system};

      # Keep every helper option mandatory while naming any that are missing
      # instead of relying on Nix's low-level argument error.
      requiredOptions = [
        "packages"
        "systemPrompt"
        "model"
        "llamaUrl"
        "readOnly"
        "nix"
        "internet"
        "shell"
      ];

      requireOptions =
        helper: names: args:
        let
          missing = builtins.filter (name: !(args ? ${name})) names;
          plural = lib.optionalString (builtins.length missing != 1) "s";
        in
        if missing == [ ] then
          args
        else
          throw ''
            hutian: ${helper} is missing required option${plural}: ${lib.concatStringsSep ", " missing}

            Required options: ${lib.concatStringsSep ", " names}.
            See README.md for a complete example.
          '';

      requireString =
        name: value:
        if builtins.isString value then value else throw "hutian: option `${name}` must be a string";

      requireNonEmptyString =
        name: value:
        if builtins.isString value && value != "" then
          value
        else
          throw "hutian: option `${name}` must be a non-empty string";

      optionalNonEmptyString =
        name: args:
        if args ? ${name} && args.${name} != null then requireNonEmptyString name args.${name} else null;

      requireBool =
        name: value:
        if builtins.isBool value then value else throw "hutian: option `${name}` must be a boolean";

      requireList =
        name: value:
        if builtins.isList value then
          value
        else
          throw "hutian: option `${name}` must be a list of packages";

      requirePackageSelector =
        name: value:
        if builtins.isFunction value then
          value
        else
          throw "hutian: option `${name}` must be a function of pkgs";

      checkedCommonOptions =
        helper: extraOptions: args:
        let
          checked = requireOptions helper (extraOptions ++ requiredOptions) args;
        in
        {
          systemPrompt = requireString "systemPrompt" checked.systemPrompt;
          model = requireNonEmptyString "model" checked.model;
          llamaUrl = requireNonEmptyString "llamaUrl" checked.llamaUrl;
          readOnly = requireBool "readOnly" checked.readOnly;
          nix = requireBool "nix" checked.nix;
          internet = requireBool "internet" checked.internet;
          shell = requireBool "shell" checked.shell;
          apiKeyFile = optionalNonEmptyString "apiKeyFile" args;
          apiKeyCommand = optionalNonEmptyString "apiKeyCommand" args;
        };

      # Used only by this flake's own package. Consumers must pass every
      # option explicitly.
      defaults = {
        systemPrompt = ''
          You are a careful coding assistant working inside a sandboxed shell.
          Prefer small, verifiable changes and explain commands before destructive
          actions.
        '';
        model = "default";
        llamaUrl = "http://127.0.0.1:8080/v1/chat/completions";
        readOnly = false;
        nix = false;
        internet = true;
        shell = true;
      };

      # Always build the Haskell package from this flake's source.
      mkHutianShell =
        args:
        let
          options = checkedCommonOptions "mkHutianShell" [ "pkgs" ] args;
          packages = requireList "packages" args.packages;
        in
        import ./nix/hutian.nix (
          options
          // {
            inherit packages;
            inherit (args) pkgs;
            src = self;
          }
        );

      mkApp = program: {
        type = "app";
        inherit program;
        meta.description = "Run an unprivileged gVisor shell with a configured package closure";
      };

      # Convenience helper for consumer flakes: derive packages and apps for
      # every supported system from a single package selector.
      mkHutianShellFlake =
        nixpkgsInput: args:
        let
          options = checkedCommonOptions "mkHutianShellFlake" [ ] args;
          packages = requirePackageSelector "packages" args.packages;
          mkPackage =
            system:
            let
              pkgs = nixpkgsInput.legacyPackages.${system};
            in
            mkHutianShell (
              options
              // {
                inherit pkgs;
                packages = packages pkgs;
              }
            );
        in
        {
          packages = lib.genAttrs systems (system: {
            default = mkPackage system;
          });

          apps = lib.genAttrs systems (system: {
            default = mkApp "${mkPackage system}/bin/hutian";
          });
        };

      built = mkHutianShellFlake nixpkgs (defaults // { packages = _: [ ]; });
    in
    {
      # Import this from another flake:
      #
      #   hutian.lib.mkHutianShell {
      #     inherit pkgs;
      #     packages = [ pkgs.git pkgs.jq ];
      #     systemPrompt = "You are a coding assistant.";
      #     model = "your-model";
      #     llamaUrl = "http://127.0.0.1:8080/v1/chat/completions";
      #     readOnly = false;
      #     nix = false;
      #     internet = true;
      #     shell = true;
      #   }
      #
      # Or, for a minimal consumer flake:
      #
      #   outputs = { nixpkgs, hutian, ... }:
      #     hutian.lib.mkHutianShellFlake nixpkgs {
      #       packages = pkgs: [ pkgs.git pkgs.jq ];
      #       systemPrompt = "You are a coding assistant.";
      #       model = "your-model";
      #       llamaUrl = "http://127.0.0.1:8080/v1/chat/completions";
      #       readOnly = false;
      #       nix = false;
      #       internet = true;
      #       shell = true;
      #     };
      lib = { inherit mkHutianShell mkHutianShellFlake; };

      inherit (built) packages apps;

      formatter = forAllSystems (system: (pkgsFor system).nixfmt-tree);

      checks = forAllSystems (
        system:
        let
          pkgs = pkgsFor system;
          lint = import ./nix/lint.nix {
            inherit pkgs;
            src = self;
          };
          probeOptions = defaults // {
            packages = [ ];
            systemPrompt = "probe system prompt";
            model = "probe-model";
            llamaUrl = "http://probe.invalid/v1/chat/completions";
          };
          probe = extra: mkHutianShell ({ inherit pkgs; } // probeOptions // extra);
          relayPackage = pkgs.callPackage ./nix/hutian-shell.nix { src = self; };
          probeLauncher = "${probe { }}/bin/hutian";
          readOnlyLauncher = "${probe { readOnly = true; }}/bin/hutian";
          noInternetLauncher = "${probe { internet = false; }}/bin/hutian";
          noShellLauncher = "${probe { shell = false; }}/bin/hutian";
          keyLauncher = "${probe { apiKeyFile = "/run/secrets/hutian-key"; }}/bin/hutian";
          keyCommandLauncher = "${probe { apiKeyCommand = "pass show k"; }}/bin/hutian";
          keyStoreRejected =
            !(builtins.tryEval (probe { apiKeyFile = "${builtins.storeDir}/hutian-key"; }).drvPath).success;
          keyConflictRejected =
            !(builtins.tryEval
              (probe {
                apiKeyFile = "/run/secrets/k";
                apiKeyCommand = "pass show k";
              }).drvPath
            ).success;
          keyEmptyRejected = !(builtins.tryEval (probe { apiKeyFile = ""; }).drvPath).success;
          keyCommandEmptyRejected = !(builtins.tryEval (probe { apiKeyCommand = ""; }).drvPath).success;
          expected = [
            "--llama-url"
            "--model"
            "--system-prompt"
            "probe system prompt"
            "probe-model"
            "http://probe.invalid/v1/chat/completions"
            "SYSTEM_CERTIFICATE_PATH"
            "ca-bundle.crt"
            "hutian-llm-relay"
            "--no-auth"
            "hutian-relay/v1/chat/completions"
            "--llama-socket"
            "/run/hutian/llm.sock"
            "relay_dir:/run/hutian"
            "--host-uds=open"
            "--allow-shell"
          ];
        in
        {
          package = built.packages.${system}.default;
          haskell-lint = lint.haskell;
          nix-lint = lint.nix;
          inherit (lint) liquid sandboxClosure sandboxSource;
          configuration = pkgs.runCommand "hutian-configuration-check" { } ''
            ${lib.concatMapStrings (pattern: ''
              ${pkgs.gnugrep}/bin/grep -F -- ${lib.escapeShellArg pattern} ${probeLauncher}
            '') expected}
            if ${pkgs.gnugrep}/bin/grep -F -- 'HUTIAN_' ${probeLauncher}; then
              echo "the launcher must not set HUTIAN_ environment variables" >&2
              exit 1
            fi
            ${pkgs.coreutils}/bin/touch $out
          '';
          read-only-option = pkgs.runCommand "hutian-read-only-option-check" { } ''
            ${pkgs.gnugrep}/bin/grep -F 'read_only=true' ${readOnlyLauncher}
            if ${pkgs.gnugrep}/bin/grep -F 'read_only=true' ${probeLauncher}; then
              echo "readOnly = false must not request a read-only working directory" >&2
              exit 1
            fi
            ${pkgs.coreutils}/bin/touch $out
          '';
          internet-option = pkgs.runCommand "hutian-internet-option-check" { } ''
            ${pkgs.gnugrep}/bin/grep -F 'GVISOR_NETWORK:-sandbox' ${probeLauncher}
            ${pkgs.gnugrep}/bin/grep -F 'network=none' ${noInternetLauncher}
            if ${pkgs.gnugrep}/bin/grep -F 'GVISOR_NETWORK:-sandbox' ${noInternetLauncher}; then
              echo "internet = false must not allow GVISOR_NETWORK to re-enable networking" >&2
              exit 1
            fi
            ${pkgs.coreutils}/bin/touch $out
          '';
          shell-option = pkgs.runCommand "hutian-shell-option-check" { } ''
            ${pkgs.gnugrep}/bin/grep -F -- '--allow-shell' ${probeLauncher}
            if ${pkgs.gnugrep}/bin/grep -F -- '--allow-shell' ${noShellLauncher}; then
              echo "shell = false must not grant the model's shell tool" >&2
              exit 1
            fi
            ${pkgs.gnugrep}/bin/grep -F 'hutian-llm-relay' ${noShellLauncher}
            ${pkgs.coreutils}/bin/touch $out
          '';
          launcher-volumes = pkgs.runCommand "hutian-launcher-volume-check" { } ''
            if ${pkgs.gnugrep}/bin/grep -F 'volumes+=(-volume' ${probeLauncher}; then
              echo "the launcher must not pass one gVisor volume per store path" >&2
              exit 1
            fi
            if ${pkgs.gnugrep}/bin/grep -F -- '-volume "$sourcedir:/source"' ${probeLauncher}; then
              echo "the launcher must bind the working directory into the rootfs" >&2
              exit 1
            fi
            ${pkgs.gnugrep}/bin/grep -F 'hutian-sandbox' ${probeLauncher}
            ${pkgs.coreutils}/bin/touch $out
          '';
          api-key-configuration = pkgs.runCommand "hutian-api-key-check" { } ''
            ${builtins.concatStringsSep "\n" (
              map
                (pattern: ''
                  ${pkgs.gnugrep}/bin/grep -F -- ${lib.escapeShellArg pattern} ${keyLauncher}
                '')
                [
                  "hutian-llm-relay"
                  "cat --"
                  "/run/secrets/hutian-key"
                  "chmod 700"
                  "trap cleanup"
                  "must not be inside the sandbox working directory"
                  "--llama-socket"
                  "/run/hutian/llm.sock"
                  "relay_dir:/run/hutian"
                  "--host-uds=open"
                  "GVISOR_NETWORK:-sandbox"
                  "http://hutian-relay/v1/chat/completions"
                ]
            )}
            ${builtins.concatStringsSep "\n" (
              map
                (pattern: ''
                  ${pkgs.gnugrep}/bin/grep -F -- ${lib.escapeShellArg pattern} ${keyCommandLauncher}
                '')
                [
                  "hutian-llm-relay"
                  "key=$(eval"
                  "printf '%s'"
                  "unset key"
                  "trap 'exit 130' INT TERM HUP"
                  "restore_terminal"
                  "--llama-socket"
                  "/run/hutian/llm.sock"
                ]
            )}
            if ${pkgs.gnugrep}/bin/grep -F -- 'did not create its socket' ${keyCommandLauncher}; then
              echo "the launcher must wait for the key command without a fixed timeout" >&2
              exit 1
            fi
            if ${pkgs.gnugrep}/bin/grep -F -- '--key-stdin' ${keyCommandLauncher}; then
              echo "the relay always reads the key from stdin" >&2
              exit 1
            fi
            if ${pkgs.gnugrep}/bin/grep -F -- '--api-key' ${keyCommandLauncher}; then
              echo "the launcher must not pass the API key as an argument" >&2
              exit 1
            fi
            if ${pkgs.gnugrep}/bin/grep -F -- '--key-file' ${keyLauncher}; then
              echo "the launcher must not pass the key file path to the relay" >&2
              exit 1
            fi
            if ${pkgs.gnugrep}/bin/grep -F -- '--key-stdin' ${keyLauncher}; then
              echo "the relay always reads the key from stdin" >&2
              exit 1
            fi
            if ${pkgs.gnugrep}/bin/grep -F -- '--api-key' ${keyLauncher}; then
              echo "the launcher must not pass the API key as an argument" >&2
              exit 1
            fi
            if ${pkgs.gnugrep}/bin/grep -F -- '--no-auth' ${keyLauncher}; then
              echo "a launcher with an API key must not disable authentication" >&2
              exit 1
            fi
            if ${pkgs.gnugrep}/bin/grep -F -- '--no-auth' ${keyCommandLauncher}; then
              echo "a launcher with an API key must not disable authentication" >&2
              exit 1
            fi
            ${pkgs.coreutils}/bin/touch $out
          '';
          api-key-rejected =
            if keyStoreRejected && keyConflictRejected && keyEmptyRejected && keyCommandEmptyRejected then
              pkgs.runCommand "hutian-api-key-rejected-check" { } "touch $out"
            else
              throw "hutian: a store-backed, empty, or conflicting key source must be rejected";
          relay-sigint = pkgs.runCommand "hutian-relay-sigint-check" { } ''
            socket_dir=$(${pkgs.coreutils}/bin/mktemp -d)
            relay=${relayPackage}/bin/hutian-llm-relay
            printf 'test-key' | "$relay" \
              --listen "$socket_dir/llm.sock" \
              --upstream http://127.0.0.1:1/v1/chat/completions &
            relay_pid=$!
            trap 'kill "$relay_pid" 2>/dev/null || true' EXIT
            for _ in $(${pkgs.coreutils}/bin/seq 1 200); do
              [ -S "$socket_dir/llm.sock" ] && break
              ${pkgs.coreutils}/bin/sleep 0.01
            done
            test -S "$socket_dir/llm.sock"
            kill -INT "$relay_pid"
            ${pkgs.coreutils}/bin/sleep 0.2
            kill -0 "$relay_pid"
            test -S "$socket_dir/llm.sock"
            kill "$relay_pid"
            wait "$relay_pid" 2>/dev/null || true
            ${pkgs.coreutils}/bin/rm -rf "$socket_dir"
            ${pkgs.coreutils}/bin/touch $out
          '';
          relay-no-key = pkgs.runCommand "hutian-relay-no-key-check" { } ''
            socket_dir=$(${pkgs.coreutils}/bin/mktemp -d)
            relay=${relayPackage}/bin/hutian-llm-relay
            # No standard input is provided: --no-auth must not read a key.
            "$relay" \
              --listen "$socket_dir/llm.sock" \
              --upstream http://127.0.0.1:1/v1/chat/completions \
              --no-auth &
            relay_pid=$!
            trap 'kill "$relay_pid" 2>/dev/null || true' EXIT
            for _ in $(${pkgs.coreutils}/bin/seq 1 200); do
              [ -S "$socket_dir/llm.sock" ] && break
              ${pkgs.coreutils}/bin/sleep 0.01
            done
            test -S "$socket_dir/llm.sock"
            kill "$relay_pid"
            wait "$relay_pid" 2>/dev/null || true
            ${pkgs.coreutils}/bin/rm -rf "$socket_dir"
            ${pkgs.coreutils}/bin/touch $out
          '';
        }
      );
    };
}
