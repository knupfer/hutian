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
        "endpoint"
        "output"
        "agent"
      ];
      optionGroups = {
        sandbox = [
          "packages"
          "writable"
          "nixDaemon"
          "network"
        ];
        endpoint = [
          "url"
          "apiKeyCommand"
        ];
        output = [
          "verbosity"
        ];
        agent = [
          "model"
          "systemPrompt"
          "tools"
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
        if !(builtins.isAttrs args) then
          [ ]
        else
          let
            isKnownGroup = name: optionGroups ? ${name};
            unknownInGroup =
              group:
              let
                isKnownKey = key: builtins.elem key optionGroups.${group};
                groupValue = args.${group} or { };
                keys = if builtins.isAttrs groupValue then builtins.attrNames groupValue else [ ];
              in
              map (key: [
                group
                key
              ]) (builtins.filter (key: !isKnownKey key) keys);
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
      toolPolicies = [
        "deny"
        "ask"
        "allow"
      ];

      # Tool configuration is validated recursively because every subagent has
      # its own tools attribute with the same shape.
      toolOptionNames = [
        "shell"
        "webfetch"
        "subagent"
        "mcp"
      ];

      toolPolicy =
        path: value:
        if builtins.isString value && builtins.elem value toolPolicies then
          value
        else
          throw "hutian: option `${path}` must be \"deny\", \"ask\", or \"allow\"";

      requiredKeys =
        path: names: value:
        let
          missing = builtins.filter (name: !(value ? ${name})) names;
        in
        if missing == [ ] then
          value
        else
          throw "hutian: option `${path}` is missing required key${plural (builtins.length missing)}: ${lib.concatStringsSep ", " missing}";

      allowedKeys =
        path: names: value:
        let
          unknown = builtins.filter (name: !(builtins.elem name names)) (builtins.attrNames value);
        in
        if unknown == [ ] then
          value
        else
          throw "hutian: option `${path}` has unknown key${plural (builtins.length unknown)}: ${lib.concatStringsSep ", " unknown}";

      checkedTools =
        path: value:
        if !(builtins.isAttrs value) then
          throw "hutian: option `${path}` must be an attribute set"
        else
          let
            present = allowedKeys path toolOptionNames (requiredKeys path toolOptionNames value);
          in
          {
            shell = toolPolicy "${path}.shell" present.shell;
            webfetch = toolPolicy "${path}.webfetch" present.webfetch;
            subagent = checkedSubagents "${path}.subagent" present.subagent;
            mcp = checkedMcp "${path}.mcp" present.mcp;
          };

      checkedSubagents =
        path: value:
        if !(builtins.isAttrs value) then
          throw "hutian: option `${path}` must be an attribute set keyed by subagent name"
        else
          lib.mapAttrs (
            name:
            if name == "" then
              throw "hutian: option `${path}` has an empty subagent name"
            else
              checkedSubagent "${path}.${name}"
          ) value;

      checkedHandlers =
        path: value:
        if !(builtins.isAttrs value) then
          throw "hutian: option `${path}` must be an attribute set keyed by tool name"
        else
          lib.mapAttrs (
            name:
            if name == "" then
              throw "hutian: option `${path}` has an empty tool name"
            else
              checkedSubagent "${path}.${name}"
          ) value;

      checkedSubagent =
        path: value:
        if !(builtins.isAttrs value) then
          throw "hutian: option `${path}` must be an attribute set"
        else
          let
            names = [
              "model"
              "description"
              "systemPrompt"
              "tools"
            ];
            present = allowedKeys path names (requiredKeys path names value);
          in
          {
            model =
              if builtins.isString present.model && present.model != "" then
                present.model
              else
                throw "hutian: option `${path}.model` must be a non-empty string";
            description =
              if builtins.isString present.description && present.description != "" then
                present.description
              else
                throw "hutian: option `${path}.description` must be a non-empty string";
            systemPrompt =
              if builtins.isString present.systemPrompt then
                present.systemPrompt
              else
                throw "hutian: option `${path}.systemPrompt` must be a string";
            tools = checkedTools "${path}.tools" present.tools;
          };

      checkedMcp =
        path: value:
        if !(builtins.isAttrs value) then
          throw "hutian: option `${path}` must be an attribute set keyed by MCP server name"
        else
          lib.mapAttrs (
            name:
            if name == "" then
              throw "hutian: option `${path}` has an empty MCP server name"
            else
              checkedMcpServer "${path}.${name}"
          ) value;

      checkedMcpServer =
        path: value:
        if !(builtins.isAttrs value) then
          throw "hutian: option `${path}` must be an attribute set"
        else
          let
            names = [
              "url"
              "policy"
              "handlers"
            ];
            present = allowedKeys path names (requiredKeys path [ "url" ] value);
            policy = if present ? policy then toolPolicy "${path}.policy" present.policy else "ask";
          in
          {
            url =
              if builtins.isString present.url && present.url != "" then
                present.url
              else
                throw "hutian: option `${path}.url` must be a non-empty string";
            inherit policy;
            handlers = checkedHandlers "${path}.handlers" (present.handlers or { });
          };

      # Normalize and validate the options shared by every helper. The result
      # is passed unchanged to the internal package builder.
      checkedOptions =
        helper: args:
        let
          valid = requireOptions helper (rejectUnknown helper args);
          inherit (valid) sandbox endpoint;
          inherit (valid) output agent;
        in
        {
          packages = checked "sandbox.packages" "a function of pkgs" builtins.isFunction sandbox.packages;
          writable = checked "sandbox.writable" "a boolean" builtins.isBool sandbox.writable;
          nixDaemon = checked "sandbox.nixDaemon" "a boolean" builtins.isBool sandbox.nixDaemon;
          network = checked "sandbox.network" "a boolean" builtins.isBool sandbox.network;
          llamaUrl = checked "endpoint.url" "a non-empty string" isNonEmptyString endpoint.url;
          apiKeyCommand =
            if endpoint.apiKeyCommand == null then
              null
            else
              checked "endpoint.apiKeyCommand" "a non-empty string" isNonEmptyString endpoint.apiKeyCommand;
          verbosity = checked "output.verbosity" ''"low", "normal", or "high"'' (
            value:
            builtins.elem value [
              "low"
              "normal"
              "high"
            ]
          ) output.verbosity;
          modelName = checked "agent.model" "a non-empty string" isNonEmptyString agent.model;
          systemPrompt = checked "agent.systemPrompt" "a string" builtins.isString agent.systemPrompt;
          tools = checkedTools "agent.tools" agent.tools;
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
        endpoint = {
          url = "http://127.0.0.1:8080/v1/chat/completions";
          apiKeyCommand = null;
        };
        output = {
          verbosity = "high";
        };
        agent = {
          model = "default";
          systemPrompt = "You are a coding agent.";
          tools = {
            shell = "ask";
            webfetch = "deny";
            subagent = { };
            mcp = { };
          };
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
      #       endpoint = { ... };
      #       output = { ... };
      #       agent = { ... };
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
      #       endpoint = { ... };
      #       output = { ... };
      #       agent = { ... };
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
            endpoint = {
              url = "http://probe.invalid/v1/chat/completions";
            };
            output = {
              verbosity = "low";
            };
            agent = {
              model = "probe-model";
              systemPrompt = "probe system prompt";
            };
          };

          launcherFor = extra: "${mkPackage pkgs (lib.recursiveUpdate probeOptions extra)}/bin/hutian";
          withSandbox = extra: launcherFor { sandbox = extra; };
          withEndpoint = extra: launcherFor { endpoint = extra; };
          withAgent = extra: launcherFor { agent = extra; };

          probeLauncher = launcherFor { };
          queryLauncher = withEndpoint {
            url = "http://probe.invalid/v1/chat/completions?token=secret#frag";
          };
          writableLauncher = withSandbox { writable = true; };
          networkLauncher = withSandbox { network = true; };
          noNetworkLauncher = withSandbox { network = false; };
          nixDaemonLauncher = withSandbox { nixDaemon = true; };
          allowedToolPolicyLauncher = withAgent {
            tools = {
              shell = "allow";
            };
          };
          deniedToolPolicyLauncher = withAgent {
            tools = {
              shell = "deny";
            };
          };
          askToolPolicyLauncher = withAgent {
            tools = {
              shell = "ask";
            };
          };
          allowedWebfetchLauncher = withAgent {
            tools = {
              webfetch = "allow";
            };
          };
          subagentLauncher = withAgent {
            tools = {
              subagent = {
                probe = {
                  model = "probe-subagent";
                  description = "probe subagent description";
                  systemPrompt = "probe subagent prompt";
                  tools = defaults.agent.tools;
                };
              };
            };
          };
          mcpLauncher = withAgent {
            tools = {
              mcp = {
                demo = {
                  url = "https://mcp.example.test/mcp";
                  policy = "ask";
                };
              };
            };
          };
          keyCommandLauncher = withEndpoint { apiKeyCommand = "pass show k"; };

          # User text must not affect shellcheck: prompts, descriptions, and
          # key commands can legitimately contain backticks and dollar signs.
          shellTextLauncher = withAgent {
            systemPrompt = "Use `backticks` and $HOME in prompts.";
            tools = {
              subagent = {
                probe = {
                  model = "probe-subagent";
                  description = "Call `search` for $HOME.";
                  systemPrompt = "Run `search` without expanding $HOME.";
                  tools = defaults.agent.tools;
                };
              };
            };
          };
          shellTextKeyLauncher = withEndpoint {
            apiKeyCommand = "$(pass show key)";
          };

          handlerAgent = {
            model = "probe-result";
            description = "probe result description";
            systemPrompt = "probe result prompt";
            tools = defaults.agent.tools;
          };
          mcpHandlerLauncher = withAgent {
            tools.mcp.demo = {
              url = "https://mcp.example.test/mcp";
              policy = "allow";
              handlers.search = handlerAgent;
            };
          };

          # Assertions for the generated launchers and scripts.
          grep = pattern: file: "${gnugrep}/bin/grep -F -- ${lib.escapeShellArg pattern} ${file}";

          contains = pattern: file: grep pattern file;

          omits = message: pattern: file: ''
            if ${grep pattern file}; then
              echo ${lib.escapeShellArg message} >&2
              exit 1
            fi
          '';

          # Launchers keep user-provided text in store files instead of the
          # generated script. Follow one such reference so assertions can read
          # the actual configuration.
          configFile =
            launcher: name:
            pkgs.runCommand "hutian-${name}-config" { } ''
              path=$(${gnugrep}/bin/grep -o -m 1 "/nix/store/[a-z0-9]*-${name}" ${launcher})
              test -n "$path"
              ${coreutils}/bin/cat "$path" > $out
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
              "SYSTEM_CERTIFICATE_PATH"
              "ca-bundle.crt"
              "hutian-llm-relay"
              "--no-auth"
              "--llama-socket"
              "/run/hutian/llm.sock"
              "/run/hutian-config/model"
              "/run/hutian-config/system-prompt"
              "/run/hutian-config/tools"
              "--tools-file"
              "--verbosity low"
              "/bin/dumb-init"
              "--single-child"
              "--host-uds=open"
              "--model-shell ask"
            ]
            ++ map (pattern: contains pattern (configFile probeLauncher "hutian-model")) [
              "probe-model"
            ]
            ++ map (pattern: contains pattern (configFile probeLauncher "hutian-system-prompt")) [
              "probe system prompt"
            ]
            ++ map (pattern: contains pattern (configFile probeLauncher "hutian-upstream-url")) [
              "http://probe.invalid/v1/chat/completions"
            ]
            ++ map (pattern: contains pattern (configFile probeLauncher "hutian-logical-url")) [
              "hutian-relay/v1/chat/completions"
            ]
            ++ map (pattern: contains pattern (configFile probeLauncher "hutian-tools")) [
              "\"shell\":\"ask\""
              "\"webfetch\":\"deny\""
            ]
            ++ [
              (omits "the launcher must not set HUTIAN_ environment variables" "HUTIAN_" probeLauncher)
            ]
          );

          logical-url = check "logical-url" [
            (contains "hutian-relay/v1/chat/completions" (configFile queryLauncher "hutian-logical-url"))
            (omits "the sandbox URL must not include the upstream query"
              "hutian-relay/v1/chat/completions?token=secret"
              (configFile queryLauncher "hutian-logical-url")
            )
            (omits "the sandbox URL must not include the upstream fragment"
              "hutian-relay/v1/chat/completions#frag"
              (configFile queryLauncher "hutian-logical-url")
            )
            (contains "http://probe.invalid/v1/chat/completions?token=secret#frag" (
              configFile queryLauncher "hutian-upstream-url"
            ))
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
            (contains "--model-shell allow" allowedToolPolicyLauncher)
            (contains "--model-shell ask" askToolPolicyLauncher)
            (contains "--model-shell deny" deniedToolPolicyLauncher)
            (omits "agent.tools.shell = \"deny\" must not grant the model's shell tool" "--model-shell allow"
              deniedToolPolicyLauncher
            )
            (contains "\"shell\":\"allow\"" (configFile allowedToolPolicyLauncher "hutian-tools"))
            (contains "\"shell\":\"deny\"" (configFile deniedToolPolicyLauncher "hutian-tools"))
            (contains "hutian-llm-relay" deniedToolPolicyLauncher)
          ];

          web-tools-option = check "web-tools-option" [
            (contains "\"webfetch\":\"allow\"" (configFile allowedWebfetchLauncher "hutian-tools"))
            (contains "\"webfetch\":\"deny\"" (configFile probeLauncher "hutian-tools"))
            (omits "websearch must no longer be accepted" "websearch" (
              configFile allowedWebfetchLauncher "hutian-tools"
            ))
          ];

          shellcheck-user-text = check "shellcheck-user-text" [
            (contains "Use `backticks` and $HOME" (configFile shellTextLauncher "hutian-system-prompt"))
            (contains "Call `search` for $HOME." (configFile shellTextLauncher "hutian-tools"))
            (contains "$(pass show key)" (configFile shellTextKeyLauncher "hutian-api-key-command"))
          ];

          handler-option = check "handler-option" [
            (contains "probe-result" (configFile mcpHandlerLauncher "hutian-tools"))
            (contains "probe result description" (configFile mcpHandlerLauncher "hutian-tools"))
            (contains "\"search\"" (configFile mcpHandlerLauncher "hutian-tools"))
          ];

          subagent-option = check "subagent-option" [
            (contains "\"subagent\":{}" (configFile probeLauncher "hutian-tools"))
            (contains "\"subagent\":{\"probe\":{" (configFile subagentLauncher "hutian-tools"))
            (contains "\"model\":\"probe-subagent\"" (configFile subagentLauncher "hutian-tools"))
            (contains "\"description\":\"probe subagent description\"" (
              configFile subagentLauncher "hutian-tools"
            ))
            (contains "\"systemPrompt\":\"probe subagent prompt\"" (configFile subagentLauncher "hutian-tools"))
          ];

          mcp-option = check "mcp-option" [
            (contains "\"mcp\":{}" (configFile probeLauncher "hutian-tools"))
            (contains "\"mcp\":{\"demo\":{" (configFile mcpLauncher "hutian-tools"))
            (contains "\"url\":\"https://mcp.example.test/mcp\"" (configFile mcpLauncher "hutian-tools"))
            (contains "\"policy\":\"ask\"" (configFile mcpLauncher "hutian-tools"))
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
            ]
            ++ [
              (contains "http://hutian-relay/v1/chat/completions" (
                configFile keyCommandLauncher "hutian-logical-url"
              ))
              (contains "pass show k" (configFile keyCommandLauncher "hutian-api-key-command"))
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
                (withEndpoint { apiKeyFile = "/run/secrets/k"; })
                (withEndpoint { apiKeyCommand = ""; })
                "${
                  mkPackage pkgs (
                    probeOptions
                    // {
                      endpoint = builtins.removeAttrs probeOptions.endpoint [ "apiKeyCommand" ];
                    }
                  )
                }/bin/hutian"
              ];

          unknown-option-rejected =
            checkRejected "unknown-option" "hutian: unknown top-level group or option must be rejected"
              [
                (launcherFor { unknownOption = true; })
                (withSandbox { unknownOption = true; })
                (withEndpoint { unknownOption = true; })
                (launcherFor {
                  output = defaults.output // {
                    unknownOption = true;
                  };
                })
                (withAgent { unknownOption = true; })
                (withAgent {
                  tools = defaults.agent.tools // {
                    unknownTool = "ask";
                  };
                })
                (withAgent {
                  tools.mcp.demo = {
                    url = "https://mcp.example.test/mcp";
                    policy = "allow";
                    handlers.search = handlerAgent // {
                      unknownOption = true;
                    };
                  };
                })
                (withAgent {
                  tools.handlers.webfetch = handlerAgent;
                })
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
