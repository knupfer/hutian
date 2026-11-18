# A dedicated Hutian flake.
#
# Copy this file as `flake.nix` next to `hutian.nix`. The `hutian` input points
# at the published flake; pin it to a tag or revision for reproducibility. The
# Hutian configuration lives in `hutian.nix`; this file only connects it to the
# flake. Run:
#
#   nix run .#hutian
#
# The selected packages, plus their runtime closures, are mounted under
# /nix/store in the sandbox, and their bin/ outputs are added to PATH.
#
# To add Hutian to an existing project flake instead, see
# examples/existing-project.nix.

{
  description = "A project sandboxed with Hutian";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    hutian = {
      url = "github:knupfer/hutian";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { nixpkgs, hutian, ... }:
    hutian.lib.mkFlake nixpkgs (import ./hutian.nix);
}
