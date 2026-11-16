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

      # Keep every helper option mandatory while naming any missing paths
      # instead of relying on Nix's low-level argument error.
      requiredPaths = [
        [
          "sandbox"
          "packages"
        ]
        [
          "sandbox"
          "writable"
        ]
        [
          "sandbox"
          "nixDaemon"
        ]
        [
          "sandbox"
          "network"
        ]
        [
          "model"
          "name"
        ]
        [
          "model"
          "url"
        ]
        [
          "model"
          "systemPrompt"
        ]
        [
          "model"
          "shell"
        ]
      ];

      hasPath =
        path: value:
        if path == [ ] then
          true
        else if !(builtins.isAttrs value) then
          false
        else
          let
            name = builtins.head path;
          in
          value ? ${name} && hasPath (builtins.tail path) value.${name};

      requirePaths =
        helper: extraPaths: args:
        let
          paths = extraPaths ++ requiredPaths;
          missing = builtins.filter (path: !(hasPath path args)) paths;
          formatPath = path: lib.concatStringsSep "." path;
          names = map formatPath paths;
          missingNames = map formatPath missing;
          plural = lib.optionalString (builtins.length missingNames != 1) "s";
        in
        if missing == [ ] then
          args
        else
          throw ''
            hutian: ${helper} is missing required option${plural}: ${lib.concatStringsSep ", " missingNames}

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
        name: label: args:
        if args ? ${name} && args.${name} != null then requireNonEmptyString label args.${name} else null;

      requireBool =
        name: value:
        if builtins.isBool value then value else throw "hutian: option `${name}` must be a boolean";

      requireModelShell =
        name: value:
        if
          builtins.isString value
          && builtins.elem value [
            "deny"
            "ask"
            "allow"
          ]
        then
          value
        else
          throw "hutian: option `${name}` must be \"deny\", \"ask\", or \"allow\"";

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

      # Known option names for each group. Every other key is rejected so a
      # misspelled or stale option cannot silently fall back to a default.
      knownTopLevelOptions = [
        "sandbox"
        "model"
      ];
      knownSandboxOptions = [
        "packages"
        "writable"
        "nixDaemon"
        "network"
      ];
      knownModelOptions = [
        "name"
        "url"
        "systemPrompt"
        "shell"
        "apiKeyFile"
        "apiKeyCommand"
      ];

      unknownOptionPaths =
        path: allowed: value:
        if !(builtins.isAttrs value) then
          [ ]
        else
          map (name: path ++ [ name ]) (
            builtins.filter (name: !(builtins.elem name allowed)) (builtins.attrNames value)
          );

      checkedCommonOptions =
        helper: extraPaths: args:
        let
          checked = requirePaths helper extraPaths args;
          inherit (checked) sandbox model;
          extraNames = map builtins.head extraPaths;
          knownTopLevel = knownTopLevelOptions ++ extraNames;
          unknown =
            unknownOptionPaths [ ] knownTopLevel checked
            ++ unknownOptionPaths [ "sandbox" ] knownSandboxOptions sandbox
            ++ unknownOptionPaths [ "model" ] knownModelOptions model;
          formatPath = path: lib.concatStringsSep "." path;
          unknownNames = map formatPath unknown;
          plural = lib.optionalString (builtins.length unknownNames != 1) "s";
          knownNames = map formatPath (
            map (name: [ name ]) knownTopLevel
            ++ map (name: [
              "sandbox"
              name
            ]) knownSandboxOptions
            ++ map (name: [
              "model"
              name
            ]) knownModelOptions
          );
        in
        if unknown != [ ] then
          throw ''
            hutian: ${helper} has unknown option${plural}: ${lib.concatStringsSep ", " unknownNames}

            Known options: ${lib.concatStringsSep ", " knownNames}.
            See README.md for a complete example.
          ''
        else
          {
            inherit (sandbox) packages;
            systemPrompt = requireString "model.systemPrompt" model.systemPrompt;
            model = requireNonEmptyString "model.name" model.name;
            llamaUrl = requireNonEmptyString "model.url" model.url;
            writable = requireBool "sandbox.writable" sandbox.writable;
            nixDaemon = requireBool "sandbox.nixDaemon" sandbox.nixDaemon;
            network = requireBool "sandbox.network" sandbox.network;
            modelShell = requireModelShell "model.shell" model.shell;
            apiKeyFile = optionalNonEmptyString "apiKeyFile" "model.apiKeyFile" model;
            apiKeyCommand = optionalNonEmptyString "apiKeyCommand" "model.apiKeyCommand" model;
          };

      # Used only by this flake's own package. Consumers must pass every
      # option explicitly.
      defaults = {
        sandbox = {
          packages = _: [ ];
          writable = false;
          nixDaemon = false;
          network = false;
        };
        model = {
          name = "default";
          url = "http://127.0.0.1:8080/v1/chat/completions";
          systemPrompt = "You are a coding agent.";
          shell = "ask";
        };
      };

      # Always build the Haskell package from this flake's source.
      mkHutianShell =
        args:
        let
          options = checkedCommonOptions "mkHutianShell" [ [ "pkgs" ] ] args;
          packages = requireList "sandbox.packages" options.packages;
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
          packages = requirePackageSelector "sandbox.packages" options.packages;
          mkPackage =
            system:
            let
              pkgs = nixpkgsInput.legacyPackages.${system};
            in
            import ./nix/hutian.nix (
              options
              // {
                inherit pkgs;
                packages = packages pkgs;
                src = self;
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

      built = mkHutianShellFlake nixpkgs defaults;
    in
    {
      # Import this from another flake:
      #
      #   hutian.lib.mkHutianShell {
      #     inherit pkgs;
      #     sandbox = {
      #       packages = [ pkgs.git pkgs.jq ];
      #       writable = true;
      #       nixDaemon = false;
      #       network = true;
      #     };
      #     model = {
      #       name = "your-model";
      #       url = "http://127.0.0.1:8080/v1/chat/completions";
      #       systemPrompt = "You are a coding assistant.";
      #       shell = "allow";
      #     };
      #   }
      #
      # Or, for a minimal consumer flake:
      #
      #   outputs = { nixpkgs, hutian, ... }:
      #     hutian.lib.mkHutianShellFlake nixpkgs {
      #       sandbox = {
      #         packages = pkgs: [ pkgs.git pkgs.jq ];
      #         writable = true;
      #         nixDaemon = false;
      #         network = true;
      #       };
      #       model = {
      #         name = "your-model";
      #         url = "http://127.0.0.1:8080/v1/chat/completions";
      #         systemPrompt = "You are a coding assistant.";
      #         shell = "allow";
      #       };
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
            sandbox = defaults.sandbox // {
              packages = [ ];
            };
            model = defaults.model // {
              name = "probe-model";
              url = "http://probe.invalid/v1/chat/completions";
              systemPrompt = "probe system prompt";
            };
          };
          probe = extra: mkHutianShell ({ inherit pkgs; } // probeOptions // extra);
          withSandbox = extra: probe { sandbox = probeOptions.sandbox // extra; };
          withModel = extra: probe { model = probeOptions.model // extra; };
          relayPackage = pkgs.callPackage ./nix/hutian-shell.nix { src = self; };
          probeLauncher = "${probe { }}/bin/hutian";
          queryLauncher = "${
            withModel { url = "http://probe.invalid/v1/chat/completions?token=secret#frag"; }
          }/bin/hutian";
          sandboxScript = import ./nix/sandbox.nix { inherit pkgs; };
          writableLauncher = "${withSandbox { writable = true; }}/bin/hutian";
          networkLauncher = "${withSandbox { network = true; }}/bin/hutian";
          noNetworkLauncher = "${withSandbox { network = false; }}/bin/hutian";
          nixDaemonLauncher = "${withSandbox { nixDaemon = true; }}/bin/hutian";
          allowedModelShellLauncher = "${withModel { shell = "allow"; }}/bin/hutian";
          deniedModelShellLauncher = "${withModel { shell = "deny"; }}/bin/hutian";
          askModelShellLauncher = "${withModel { shell = "ask"; }}/bin/hutian";
          keyLauncher = "${withModel { apiKeyFile = "/run/secrets/hutian-key"; }}/bin/hutian";
          keyCommandLauncher = "${withModel { apiKeyCommand = "pass show k"; }}/bin/hutian";
          keyStoreRejected =
            !(builtins.tryEval (withModel { apiKeyFile = "${builtins.storeDir}/hutian-key"; }).drvPath).success;
          keyConflictRejected =
            !(builtins.tryEval
              (withModel {
                apiKeyFile = "/run/secrets/k";
                apiKeyCommand = "pass show k";
              }).drvPath
            ).success;
          keyEmptyRejected = !(builtins.tryEval (withModel { apiKeyFile = ""; }).drvPath).success;
          keyCommandEmptyRejected = !(builtins.tryEval (withModel { apiKeyCommand = ""; }).drvPath).success;
          unknownTopLevelRejected = !(builtins.tryEval (probe { unknownOption = true; }).drvPath).success;
          unknownSandboxRejected =
            !(builtins.tryEval (withSandbox { unknownOption = true; }).drvPath).success;
          unknownModelRejected = !(builtins.tryEval (withModel { unknownOption = true; }).drvPath).success;
          expected = [
            "--llama-url"
            "--model-file"
            "--system-prompt-file"
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
            "/run/hutian-config/model"
            "/run/hutian-config/system-prompt"
            "/bin/dumb-init"
            "--single-child"
            "--host-uds=open"
            "--model-shell ask"
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
          logical-url = pkgs.runCommand "hutian-logical-url-check" { } ''
            ${pkgs.gnugrep}/bin/grep -F 'hutian-relay/v1/chat/completions' ${queryLauncher}
            if ${pkgs.gnugrep}/bin/grep -F 'hutian-relay/v1/chat/completions?token=secret' ${queryLauncher}; then
              echo "the sandbox URL must not include the upstream query" >&2
              exit 1
            fi
            if ${pkgs.gnugrep}/bin/grep -F 'hutian-relay/v1/chat/completions#frag' ${queryLauncher}; then
              echo "the sandbox URL must not include the upstream fragment" >&2
              exit 1
            fi
            ${pkgs.coreutils}/bin/touch $out
          '';
          relay-read-only = pkgs.runCommand "hutian-relay-read-only-check" { } ''
            ${pkgs.gnugrep}/bin/grep -F -- 'relaydir=$6' ${sandboxScript}
            ${pkgs.gnugrep}/bin/grep -F -- 'remount,bind,ro "$rootfs/run/hutian"' ${sandboxScript}
            if ${pkgs.gnugrep}/bin/grep -F 'relay_dir:/run/hutian' ${probeLauncher}; then
              echo "the relay directory must not be mounted writable with -volume" >&2
              exit 1
            fi
            ${pkgs.coreutils}/bin/touch $out
          '';
          binary-stripped = pkgs.runCommand "hutian-binary-stripped-check" { } ''
            if ${pkgs.binutils}/bin/readelf -S ${relayPackage}/bin/hutian-shell | ${pkgs.gnugrep}/bin/grep -E '\.(symtab|debug_)'; then
              echo "hutian-shell must be stripped" >&2
              exit 1
            fi
            ${pkgs.coreutils}/bin/touch $out
          '';
          writable-option = pkgs.runCommand "hutian-writable-option-check" { } ''
            ${pkgs.gnugrep}/bin/grep -F 'writable=false' ${probeLauncher}
            ${pkgs.gnugrep}/bin/grep -F 'writable=true' ${writableLauncher}
            if ${pkgs.gnugrep}/bin/grep -F 'writable=false' ${writableLauncher}; then
              echo "sandbox.writable = true must not request a read-only working directory" >&2
              exit 1
            fi
            ${pkgs.coreutils}/bin/touch $out
          '';
          network-option = pkgs.runCommand "hutian-network-option-check" { } ''
            ${pkgs.gnugrep}/bin/grep -F 'GVISOR_NETWORK:-host' ${networkLauncher}
            ${pkgs.gnugrep}/bin/grep -F 'network=none' ${noNetworkLauncher}
            if ${pkgs.gnugrep}/bin/grep -F 'GVISOR_NETWORK:-' ${noNetworkLauncher}; then
              echo "sandbox.network = false must not allow GVISOR_NETWORK to re-enable networking" >&2
              exit 1
            fi
            ${pkgs.coreutils}/bin/touch $out
          '';
          nix-registry = pkgs.runCommand "hutian-nix-registry-check" { } ''
            ${pkgs.gnugrep}/bin/grep -F 'NIX_REMOTE=daemon' ${nixDaemonLauncher}
            ${pkgs.gnugrep}/bin/grep -F 'flake-registry = file:///run/hutian-nix/nix/registry.json' ${nixDaemonLauncher}
            ${pkgs.gnugrep}/bin/grep -F 'XDG_CONFIG_HOME=/run/hutian-nix' ${nixDaemonLauncher}
            if ${pkgs.gnugrep}/bin/grep -F -e '/etc/nix/registry.json' -e 'host_registry' ${sandboxScript}; then
              echo "the sandbox must not expose the host flake registry" >&2
              exit 1
            fi
            if ${pkgs.gnugrep}/bin/grep -F 'flake-registry = file://' ${probeLauncher}; then
              echo "sandbox.nixDaemon = false must not configure a flake registry" >&2
              exit 1
            fi
            ${pkgs.coreutils}/bin/touch $out
          '';
          dentry-cache = pkgs.runCommand "hutian-dentry-cache-check" { } ''
            ${pkgs.gnugrep}/bin/grep -F -- '--dcache=0' ${probeLauncher}
            ${pkgs.gnugrep}/bin/grep -F -- '--dcache=0' ${nixDaemonLauncher}
            ${pkgs.coreutils}/bin/touch $out
          '';
          model-shell-option = pkgs.runCommand "hutian-model-shell-option-check" { } ''
            ${pkgs.gnugrep}/bin/grep -F -- '--model-shell allow' ${allowedModelShellLauncher}
            ${pkgs.gnugrep}/bin/grep -F -- '--model-shell ask' ${askModelShellLauncher}
            ${pkgs.gnugrep}/bin/grep -F -- '--model-shell deny' ${deniedModelShellLauncher}
            if ${pkgs.gnugrep}/bin/grep -F -- '--model-shell allow' ${deniedModelShellLauncher}; then
              echo "model.shell = \"deny\" must not grant the model's shell tool" >&2
              exit 1
            fi
            ${pkgs.gnugrep}/bin/grep -F 'hutian-llm-relay' ${deniedModelShellLauncher}
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
                  "--host-uds=open"
                  "network=none"
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
          unknown-option-rejected =
            if unknownTopLevelRejected && unknownSandboxRejected && unknownModelRejected then
              pkgs.runCommand "hutian-unknown-option-check" { } "touch $out"
            else
              throw "hutian: unknown top-level, sandbox, or model options must be rejected";
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
