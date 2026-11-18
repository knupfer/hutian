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

      # Every option is mandatory. The groups also list the only accepted keys,
      # so a misspelled or stale option fails instead of being ignored. Keep
      # the group order to make error messages match the documented order.
      groupNames = [
        "sandbox"
        "model"
      ];
      optionGroups = {
        sandbox = [
          "packages"
          "writable"
          "nixDaemon"
          "network"
        ];
        model = [
          "name"
          "url"
          "systemPrompt"
          "shell"
          "apiKeyCommand"
        ];
      };

      optionPaths =
        group:
        map (key: [
          group
          key
        ]) optionGroups.${group};
      knownPaths = lib.concatMap optionPaths groupNames;

      formatPath = lib.concatStringsSep ".";
      formatPaths = paths: lib.concatStringsSep ", " (map formatPath paths);
      plural = count: lib.optionalString (count != 1) "s";

      # A value is present when every component along the path names an
      # attribute set that contains it.
      hasPath =
        path: value:
        path == [ ]
        || (
          builtins.isAttrs value
          && value ? ${builtins.head path}
          && hasPath (builtins.tail path) value.${builtins.head path}
        );

      requireOptions =
        helper: args:
        let
          missing = builtins.filter (path: !hasPath path args) knownPaths;
          missingNames = formatPaths missing;
        in
        if missing == [ ] then
          args
        else
          throw ''
            hutian: ${helper} is missing required option${plural (builtins.length missing)}: ${missingNames}

            Required options: ${formatPaths knownPaths}.
            See README.md for a complete example.
          '';

      unknownPaths =
        args:
        let
          isKnownGroup = name: optionGroups ? ${name};
          unknownInGroup =
            group:
            let
              isKnownKey = key: builtins.elem key optionGroups.${group};
            in
            map (key: [
              group
              key
            ]) (builtins.filter (key: !isKnownKey key) (builtins.attrNames args.${group}));
        in
        map (name: [ name ]) (builtins.filter (name: !isKnownGroup name) (builtins.attrNames args))
        ++ lib.concatMap unknownInGroup groupNames;

      rejectUnknown =
        helper: args:
        let
          unknown = unknownPaths args;
          unknownNames = formatPaths unknown;
        in
        if unknown == [ ] then
          args
        else
          throw ''
            hutian: ${helper} has unknown option${plural (builtins.length unknown)}: ${unknownNames}

            Known options: ${formatPaths knownPaths}.
            See README.md for a complete example.
          '';

      # Validate one option value. `expected` completes the error message.
      checked =
        name: expected: predicate: value:
        if predicate value then value else throw "hutian: option `${name}` must be ${expected}";

      isNonEmptyString = value: builtins.isString value && value != "";
      modelShells = [
        "deny"
        "ask"
        "allow"
      ];

      # Normalize and validate the options shared by every helper. The result
      # is passed unchanged to the internal package builder.
      checkedOptions =
        helper: args:
        let
          valid = rejectUnknown helper (requireOptions helper args);
          inherit (valid) sandbox model;
        in
        {
          packages = checked "sandbox.packages" "a function of pkgs" builtins.isFunction sandbox.packages;
          writable = checked "sandbox.writable" "a boolean" builtins.isBool sandbox.writable;
          nixDaemon = checked "sandbox.nixDaemon" "a boolean" builtins.isBool sandbox.nixDaemon;
          network = checked "sandbox.network" "a boolean" builtins.isBool sandbox.network;
          modelName = checked "model.name" "a non-empty string" isNonEmptyString model.name;
          llamaUrl = checked "model.url" "a non-empty string" isNonEmptyString model.url;
          systemPrompt = checked "model.systemPrompt" "a string" builtins.isString model.systemPrompt;
          modelShell = checked "model.shell" ''"deny", "ask", or "allow"'' (
            value: builtins.elem value modelShells
          ) model.shell;
          apiKeyCommand =
            if model.apiKeyCommand == null then
              null
            else
              checked "model.apiKeyCommand" "a non-empty string" isNonEmptyString model.apiKeyCommand;
        };

      # Used only by this flake's own package and its option checks. Consumers
      # must pass every option explicitly.
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
          apiKeyCommand = null;
        };
      };

      buildPackage =
        pkgs: options:
        import ./nix/hutian.nix (
          options
          // {
            inherit pkgs;
            src = self;
          }
        );

      # Build a Hutian package for one pkgs instance.
      mkPackage = pkgs: args: buildPackage pkgs (checkedOptions "mkPackage" args);

      mkAppOutput = program: {
        type = "app";
        inherit program;
        meta.description = "Run an unprivileged gVisor shell with a configured package closure";
      };

      # Add Hutian to an existing flake's apps without replacing its default.
      mkApp = pkgs: args: mkAppOutput "${mkPackage pkgs args}/bin/hutian";

      # Convenience helper for consumer flakes: derive packages and apps for
      # every supported system from a single configuration.
      mkFlake =
        nixpkgsInput: args:
        let
          options = checkedOptions "mkFlake" args;
          packageFor = system: buildPackage nixpkgsInput.legacyPackages.${system} options;
        in
        {
          packages = lib.genAttrs systems (system: {
            hutian = packageFor system;
          });

          apps = lib.genAttrs systems (system: {
            hutian = mkAppOutput "${packageFor system}/bin/hutian";
          });
        };

      built = mkFlake nixpkgs defaults;
    in
    {
      # A dedicated Hutian flake returns the helper's outputs directly:
      #
      #   outputs = { nixpkgs, hutian, ... }:
      #     hutian.lib.mkFlake nixpkgs {
      #       sandbox = { ... };
      #       model = { ... };
      #     };
      #
      # That exposes `apps.<system>.hutian` and `packages.<system>.hutian`.
      #
      # To add Hutian to an existing flake, insert its app without replacing
      # the flake's own default:
      #
      #   apps.<system>.hutian =
      #     hutian.lib.mkApp pkgs {
      #       sandbox = { ... };
      #       model = { ... };
      #     };
      #
      # For a single package, use `hutian.lib.mkPackage` with the same
      # arguments.
      lib = { inherit mkApp mkFlake mkPackage; };

      inherit (built) packages apps;

      formatter = forAllSystems (system: (pkgsFor system).nixfmt-tree);

      checks = forAllSystems (
        system:
        let
          pkgs = pkgsFor system;
          inherit (pkgs) binutils coreutils gnugrep;

          lint = import ./nix/lint.nix {
            inherit pkgs;
            src = self;
          };
          relayPackage = pkgs.callPackage ./nix/hutian-shell.nix { src = self; };
          sandboxScript = import ./nix/sandbox.nix { inherit pkgs; };

          # Probe values differ from the flake defaults so the checks can tell
          # configured values from hard-coded ones.
          probeOptions = lib.recursiveUpdate defaults {
            model = {
              name = "probe-model";
              url = "http://probe.invalid/v1/chat/completions";
              systemPrompt = "probe system prompt";
            };
          };

          launcherFor = extra: "${mkPackage pkgs (lib.recursiveUpdate probeOptions extra)}/bin/hutian";
          withSandbox = extra: launcherFor { sandbox = extra; };
          withModel = extra: launcherFor { model = extra; };

          probeLauncher = launcherFor { };
          queryLauncher = withModel {
            url = "http://probe.invalid/v1/chat/completions?token=secret#frag";
          };
          writableLauncher = withSandbox { writable = true; };
          networkLauncher = withSandbox { network = true; };
          noNetworkLauncher = withSandbox { network = false; };
          nixDaemonLauncher = withSandbox { nixDaemon = true; };
          allowedModelShellLauncher = withModel { shell = "allow"; };
          deniedModelShellLauncher = withModel { shell = "deny"; };
          askModelShellLauncher = withModel { shell = "ask"; };
          keyCommandLauncher = withModel { apiKeyCommand = "pass show k"; };

          # Assertions for the generated launchers and scripts.
          grep = pattern: file: "${gnugrep}/bin/grep -F -- ${lib.escapeShellArg pattern} ${file}";

          contains = pattern: file: grep pattern file;

          omits = message: pattern: file: ''
            if ${grep pattern file}; then
              echo ${lib.escapeShellArg message} >&2
              exit 1
            fi
          '';

          check =
            name: assertions:
            pkgs.runCommand "hutian-${name}-check" { } ''
              ${lib.concatStringsSep "\n" assertions}
              ${coreutils}/bin/touch $out
            '';

          # `true` when evaluating a launcher fails, as it should for rejected
          # options.
          rejected = attempt: !(builtins.tryEval attempt).success;

          checkRejected =
            name: message: attempts:
            if lib.all rejected attempts then
              pkgs.runCommand "hutian-${name}-check" { } "${coreutils}/bin/touch $out"
            else
              throw message;

          # The relay uses the same readiness loop in both relay checks.
          waitForSocket = ''
            for _ in $(${coreutils}/bin/seq 1 200); do
              [ -S "$socket_dir/llm.sock" ] && break
              ${coreutils}/bin/sleep 0.01
            done
            test -S "$socket_dir/llm.sock"
          '';
        in
        {
          package = built.packages.${system}.hutian;
          haskell-lint = lint.haskell;
          nix-lint = lint.nix;
          inherit (lint) liquid sandboxClosure sandboxSource;

          configuration = check "configuration" (
            map (pattern: contains pattern probeLauncher) [
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
            ]
            ++ [
              (omits "the launcher must not set HUTIAN_ environment variables" "HUTIAN_" probeLauncher)
            ]
          );

          logical-url = check "logical-url" [
            (contains "hutian-relay/v1/chat/completions" queryLauncher)
            (omits "the sandbox URL must not include the upstream query"
              "hutian-relay/v1/chat/completions?token=secret"
              queryLauncher
            )
            (omits "the sandbox URL must not include the upstream fragment"
              "hutian-relay/v1/chat/completions#frag"
              queryLauncher
            )
          ];

          relay-read-only = check "relay-read-only" [
            (contains "relay_dir=$6" sandboxScript)
            (contains "remount,bind,ro \"$rootfs/run/hutian\"" sandboxScript)
            (omits "the relay directory must not be mounted writable with -volume" "relay_dir:/run/hutian"
              probeLauncher
            )
          ];

          binary-stripped = pkgs.runCommand "hutian-binary-stripped-check" { } ''
            if ${binutils}/bin/readelf -S ${relayPackage}/bin/hutian-shell \
              | ${gnugrep}/bin/grep -E '\.(symtab|debug_)'; then
              echo "hutian-shell must be stripped" >&2
              exit 1
            fi
            ${coreutils}/bin/touch $out
          '';

          writable-option = check "writable-option" [
            (contains "writable=false" probeLauncher)
            (contains "writable=true" writableLauncher)
            (omits "sandbox.writable = true must not request a read-only working directory" "writable=false"
              writableLauncher
            )
          ];

          network-option = check "network-option" [
            (contains "GVISOR_NETWORK:-host" networkLauncher)
            (contains "network=none" noNetworkLauncher)
            (omits "sandbox.network = false must not allow GVISOR_NETWORK to re-enable networking"
              "GVISOR_NETWORK:-"
              noNetworkLauncher
            )
          ];

          nix-registry = check "nix-registry" [
            (contains "NIX_REMOTE=daemon" nixDaemonLauncher)
            (contains "flake-registry = file:///run/hutian-nix/nix/registry.json" nixDaemonLauncher)
            (contains "XDG_CONFIG_HOME=/run/hutian-nix" nixDaemonLauncher)
            (omits "the sandbox must not expose the host flake registry" "/etc/nix/registry.json" sandboxScript)
            (omits "the sandbox must not expose the host flake registry" "host_registry" sandboxScript)
            (omits "sandbox.nixDaemon = false must not configure a flake registry" "flake-registry = file://"
              probeLauncher
            )
          ];

          dentry-cache = check "dentry-cache" [
            (contains "--dcache=0" probeLauncher)
            (contains "--dcache=0" nixDaemonLauncher)
          ];

          model-shell-option = check "model-shell-option" [
            (contains "--model-shell allow" allowedModelShellLauncher)
            (contains "--model-shell ask" askModelShellLauncher)
            (contains "--model-shell deny" deniedModelShellLauncher)
            (omits "model.shell = \"deny\" must not grant the model's shell tool" "--model-shell allow"
              deniedModelShellLauncher
            )
            (contains "hutian-llm-relay" deniedModelShellLauncher)
          ];

          launcher-volumes = check "launcher-volumes" [
            (omits "the launcher must not pass one gVisor volume per store path" "volumes+=(-volume"
              probeLauncher
            )
            (omits "the launcher must bind the working directory into the rootfs"
              "-volume \"$source_dir:/source\""
              probeLauncher
            )
            (contains "hutian-sandbox" probeLauncher)
          ];

          api-key-configuration = check "api-key" (
            map (pattern: contains pattern keyCommandLauncher) [
              "hutian-llm-relay"
              "key=$(eval"
              "printf '%s'"
              "unset key"
              "trap 'exit 130' INT TERM HUP"
              "restore_terminal"
              "chmod 700"
              "trap cleanup"
              "network=none"
              "--llama-socket"
              "/run/hutian/llm.sock"
              "--host-uds=open"
              "http://hutian-relay/v1/chat/completions"
            ]
            ++ [
              (omits "the launcher must wait for the key command without a fixed timeout"
                "did not create its socket"
                keyCommandLauncher
              )
              (omits "the relay always reads the key from stdin" "--key-stdin" keyCommandLauncher)
              (omits "the launcher must not pass the API key as an argument" "--api-key" keyCommandLauncher)
              (omits "a launcher with an API key must not disable authentication" "--no-auth" keyCommandLauncher)
            ]
          );

          api-key-rejected =
            checkRejected "api-key-rejected" "hutian: a removed, empty, or missing key command must be rejected"
              [
                (withModel { apiKeyFile = "/run/secrets/k"; })
                (withModel { apiKeyCommand = ""; })
                "${
                  mkPackage pkgs (
                    probeOptions
                    // {
                      model = builtins.removeAttrs probeOptions.model [ "apiKeyCommand" ];
                    }
                  )
                }/bin/hutian"
              ];

          unknown-option-rejected =
            checkRejected "unknown-option"
              "hutian: unknown top-level, sandbox, or model options must be rejected"
              [
                (launcherFor { unknownOption = true; })
                (withSandbox { unknownOption = true; })
                (withModel { unknownOption = true; })
              ];

          relay-sigint = pkgs.runCommand "hutian-relay-sigint-check" { } ''
            socket_dir=$(${coreutils}/bin/mktemp -d)
            relay=${relayPackage}/bin/hutian-llm-relay
            printf 'test-key' | "$relay" \
              --listen "$socket_dir/llm.sock" \
              --upstream http://127.0.0.1:1/v1/chat/completions &
            relay_pid=$!
            trap 'kill "$relay_pid" 2>/dev/null || true' EXIT
            ${waitForSocket}
            kill -INT "$relay_pid"
            ${coreutils}/bin/sleep 0.2
            kill -0 "$relay_pid"
            test -S "$socket_dir/llm.sock"
            kill "$relay_pid"
            wait "$relay_pid" 2>/dev/null || true
            ${coreutils}/bin/rm -rf "$socket_dir"
            ${coreutils}/bin/touch $out
          '';

          relay-no-key = pkgs.runCommand "hutian-relay-no-key-check" { } ''
            socket_dir=$(${coreutils}/bin/mktemp -d)
            relay=${relayPackage}/bin/hutian-llm-relay
            # No standard input is provided: --no-auth must not read a key.
            "$relay" \
              --listen "$socket_dir/llm.sock" \
              --upstream http://127.0.0.1:1/v1/chat/completions \
              --no-auth &
            relay_pid=$!
            trap 'kill "$relay_pid" 2>/dev/null || true' EXIT
            ${waitForSocket}
            kill "$relay_pid"
            wait "$relay_pid" 2>/dev/null || true
            ${coreutils}/bin/rm -rf "$socket_dir"
            ${coreutils}/bin/touch $out
          '';
        }
      );
    };
}
