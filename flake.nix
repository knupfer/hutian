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

      # Used only by this flake's own package. Consumers must pass their own
      # required `systemPrompt`, `model`, and `llamaUrl` options.
      defaults = {
        systemPrompt = ''
          You are a careful coding assistant working inside a sandboxed shell.
          Prefer small, verifiable changes and explain commands before destructive
          actions.
        '';
        model = "default";
        llamaUrl = "http://127.0.0.1:8080/v1/chat/completions";
      };

      mkHutianShell = import ./nix/hutian.nix;

      mkApp = program: {
        type = "app";
        inherit program;
        meta.description = "Run an unprivileged gVisor shell with a configured package closure";
      };

      # Convenience helper for consumer flakes: derive packages and apps for
      # every system from a single package selector.
      mkHutianShellFlake =
        nixpkgs:
        {
          packages,
          systemPrompt,
          model,
          llamaUrl,
          nix ? false,
          src ? null,
          systems ? defaultSystems,
          pkgsFor ? (system: nixpkgs.legacyPackages.${system}),
        }:
        let
          mkPackage =
            system:
            let
              pkgs = pkgsFor system;
            in
            mkHutianShell (
              {
                inherit
                  pkgs
                  nix
                  systemPrompt
                  model
                  llamaUrl
                  ;
                packages = packages pkgs;
              }
              // lib.optionalAttrs (src != null) { inherit src; }
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

      built = mkHutianShellFlake nixpkgs {
        inherit (defaults) systemPrompt model llamaUrl;
        packages = _: [ ];
        src = self;
      };
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
            src = self;
          };
          expected = [
            "HUTIAN_SYSTEM_PROMPT="
            "probe system prompt"
            "HUTIAN_LLAMA_MODEL=probe-model"
            "HUTIAN_LLAMA_URL=http://probe.invalid/v1/chat/completions"
          ];
        in
        {
          package = built.packages.${system}.default;
          haskell-lint = lint.haskell;
          nix-lint = lint.nix;
          inherit (lint) liquid;
          required-options = pkgs.runCommand "hutian-required-options-check" { } ''
            ${lib.concatMapStrings (pattern: ''
              ${pkgs.gnugrep}/bin/grep -F ${lib.escapeShellArg pattern} ${probe}/bin/hutian
            '') expected}
            ${pkgs.coreutils}/bin/touch $out
          '';
        }
      );
    };
}
