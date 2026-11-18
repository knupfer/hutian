# Add Hutian to an existing project flake.
#
# Copy this file into your project as `flake.nix` together with `hutian.nix`,
# keep your own outputs, and adjust the `hutian` input URL. Then run:
#
#   nix run             # the project's own default app
#   nix run .#hutian    # Hutian
#
# `mkApp` and `mkPackage` add one output each, so the project's existing
# `default` app and package stay in place. The Hutian configuration is shared
# with examples/dedicated.nix through `hutian.nix`.
#
# For a flake that exists only to run Hutian, see examples/dedicated.nix.

{
  description = "A project flake that also exposes Hutian";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    hutian = {
      url = "path:/path/to/this/flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { nixpkgs, hutian, ... }:
    let
      inherit (nixpkgs) lib;

      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      forAllSystems = lib.genAttrs systems;
      pkgsFor = system: nixpkgs.legacyPackages.${system};

      # One Hutian configuration, shared by the app and the package.
      hutianConfig = import ./hutian.nix;

      # The project's own app, kept as the default.
      sampleApp =
        pkgs:
        pkgs.writeShellScriptBin "sample" ''
          set -eu
          echo "Hello from the project's default app."
          echo "Run 'nix run .#hutian' to start Hutian instead."
        '';
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = pkgsFor system;
        in
        {
          default = sampleApp pkgs;
          hutian = hutian.lib.mkPackage pkgs hutianConfig;
        }
      );

      apps = forAllSystems (
        system:
        let
          pkgs = pkgsFor system;
        in
        {
          default = {
            type = "app";
            program = "${sampleApp pkgs}/bin/sample";
          };
          hutian = hutian.lib.mkApp pkgs hutianConfig;
        }
      );
    };
}
