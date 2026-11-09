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

      # Used only by this flake's own package. Consumers must pass their own
      # required `systemPrompt`, `model`, and `llamaUrl` options.
      defaultSystemPrompt = ''
        You are a careful coding assistant working inside a sandboxed shell.
        Prefer small, verifiable changes and explain commands before destructive
        actions.
      '';
      defaultModel = "default";
      defaultLlamaUrl = "http://127.0.0.1:8080/v1/chat/completions";

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
          systems ? defaultSystems,
          pkgsFor ? (system: nixpkgs.legacyPackages.${system}),
        }:
        let
          mkPackage =
            system:
            let
              pkgs = pkgsFor system;
            in
            mkHutianShell {
              inherit
                pkgs
                nix
                systemPrompt
                model
                llamaUrl
                ;
              packages = packages pkgs;
            };
        in
        {
          packages = lib.genAttrs systems (system: {
            default = mkPackage system;
          });

          apps = lib.genAttrs systems (system: {
            default = mkApp "${mkPackage system}/bin/hutian";
          });
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

      packages = forAllSystems (system: {
        default = mkHutianShell {
          pkgs = nixpkgs.legacyPackages.${system};
          packages = [ ];
          systemPrompt = defaultSystemPrompt;
          model = defaultModel;
          llamaUrl = defaultLlamaUrl;
          src = self;
        };
      });

      apps = forAllSystems (system: {
        default = mkApp "${self.packages.${system}.default}/bin/hutian";
      });

      formatter = forAllSystems (system: nixpkgs.legacyPackages.${system}.nixfmt-tree);

      checks = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          probe = mkHutianShell {
            inherit pkgs;
            packages = [ ];
            systemPrompt = "probe system prompt";
            model = "probe-model";
            llamaUrl = "http://probe.invalid/v1/chat/completions";
            src = self;
          };
          lint = import ./nix/lint.nix {
            inherit pkgs;
            src = self;
          };
        in
        {
          package = self.packages.${system}.default;
          haskell-lint = lint.haskell;
          nix-lint = lint.nix;
          inherit (lint) liquid;
          required-options = pkgs.runCommand "hutian-required-options-check" { } ''
            ${pkgs.gnugrep}/bin/grep -F 'HUTIAN_SYSTEM_PROMPT=' ${probe}/bin/hutian
            ${pkgs.gnugrep}/bin/grep -F 'probe system prompt' ${probe}/bin/hutian
            ${pkgs.gnugrep}/bin/grep -F 'HUTIAN_LLAMA_MODEL=probe-model' ${probe}/bin/hutian
            ${pkgs.gnugrep}/bin/grep -F 'HUTIAN_LLAMA_URL=http://probe.invalid/v1/chat/completions' ${probe}/bin/hutian
            ${pkgs.coreutils}/bin/touch $out
          '';
        }
      );
    };
}
