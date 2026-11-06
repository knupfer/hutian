{
  description = "A rootless gVisor sandbox with a Haskell shell and configurable Nix closure";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      lib = nixpkgs.lib;
      defaultSystems = [ "x86_64-linux" "aarch64-linux" ];
      forAllSystems = lib.genAttrs defaultSystems;

      mkHutianShell = import ./nix/hutian.nix;

      mkApp = program: {
        type = "app";
        inherit program;
        meta.description = "Run an unprivileged gVisor shell with a configured package closure";
      };

      # Convenience helper for consumer flakes: derive packages and apps for
      # every system from a single package selector.
      mkHutianShellFlake = nixpkgs: {
        packages,
        nix ? false,
        systems ? defaultSystems,
        pkgsFor ? (system: nixpkgs.legacyPackages.${system}),
      }:
        let
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
      #   }
      #
      # Or, for a minimal consumer flake:
      #
      #   outputs = { nixpkgs, hutian, ... }:
      #     hutian.lib.mkHutianShellFlake nixpkgs {
      #       packages = pkgs: [ pkgs.git pkgs.jq ];
      #     };
      lib = { inherit mkHutianShell mkHutianShellFlake; };

      packages = forAllSystems (system: {
        default = mkHutianShell {
          pkgs = nixpkgs.legacyPackages.${system};
          packages = [ ];
        };
      });

      apps = forAllSystems (system: {
        default = mkApp "${self.packages.${system}.default}/bin/hutian";
      });
    };
}
