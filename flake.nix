{
  description = "A rootless gVisor sandbox with a Haskell shell and configurable Nix closure";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { self, nixpkgs }:
    let
      inherit (nixpkgs) lib;

      defaultSystems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      forAllSystems = lib.genAttrs defaultSystems;
      pkgsFor = system: nixpkgs.legacyPackages.${system};

      # Keep every helper option mandatory while naming any that are missing
      # instead of relying on Nix's low-level argument error.
      shellOptions = [
        "packages"
        "systemPrompt"
        "model"
        "llamaUrl"
        "readOnly"
        "nix"
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
      };

      # Always build the Haskell package from this flake's source.
      mkHutianShell =
        args:
        let
          checked = requireOptions "mkHutianShell" ([ "pkgs" ] ++ shellOptions) args;
        in
        import ./nix/hutian.nix {
          inherit (checked)
            pkgs
            packages
            systemPrompt
            model
            llamaUrl
            readOnly
            nix
            ;
          src = self;
        };

      mkApp = program: {
        type = "app";
        inherit program;
        meta.description = "Run an unprivileged gVisor shell with a configured package closure";
      };

      # Convenience helper for consumer flakes: derive packages and apps for
      # every supported system from a single package selector.
      mkHutianShellFlake =
        nixpkgs: args:
        let
          checked = requireOptions "mkHutianShellFlake" shellOptions args;
          inherit (checked)
            packages
            systemPrompt
            model
            llamaUrl
            readOnly
            nix
            ;
          mkPackage =
            system:
            let
              pkgs = nixpkgs.legacyPackages.${system};
            in
            mkHutianShell {
              inherit
                pkgs
                systemPrompt
                model
                llamaUrl
                readOnly
                nix
                ;
              packages = packages pkgs;
            };
        in
        {
          packages = lib.genAttrs defaultSystems (system: {
            default = mkPackage system;
          });

          apps = lib.genAttrs defaultSystems (system: {
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
          probe = mkHutianShell {
            inherit pkgs;
            packages = [ ];
            systemPrompt = "probe system prompt";
            model = "probe-model";
            llamaUrl = "http://probe.invalid/v1/chat/completions";
            readOnly = false;
            nix = false;
          };
          readOnlyProbe = mkHutianShell {
            inherit pkgs;
            packages = [ ];
            systemPrompt = "probe system prompt";
            model = "probe-model";
            llamaUrl = "http://probe.invalid/v1/chat/completions";
            readOnly = true;
            nix = false;
          };
          expected = [
            "--llama-url"
            "--model"
            "--system-prompt"
            "probe system prompt"
            "probe-model"
            "http://probe.invalid/v1/chat/completions"
          ];
        in
        {
          package = built.packages.${system}.default;
          haskell-lint = lint.haskell;
          nix-lint = lint.nix;
          inherit (lint) liquid;
          configuration = pkgs.runCommand "hutian-configuration-check" { } ''
            ${lib.concatMapStrings (pattern: ''
              ${pkgs.gnugrep}/bin/grep -F -- ${lib.escapeShellArg pattern} ${probe}/bin/hutian
            '') expected}
            if ${pkgs.gnugrep}/bin/grep -F -- 'HUTIAN_' ${probe}/bin/hutian; then
              echo "the launcher must not set HUTIAN_ environment variables" >&2
              exit 1
            fi
            ${pkgs.coreutils}/bin/touch $out
          '';
          read-only-option = pkgs.runCommand "hutian-read-only-option-check" { } ''
            ${pkgs.gnugrep}/bin/grep -F 'hutian-read-only' ${readOnlyProbe}/bin/hutian
            if ${pkgs.gnugrep}/bin/grep -F 'hutian-read-only' ${probe}/bin/hutian; then
              echo "readOnly = false must not use the read-only wrapper" >&2
              exit 1
            fi
            ${pkgs.coreutils}/bin/touch $out
          '';
          read-only-mount = lint.readOnlyMount;
        }
      );
    };
}
