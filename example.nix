# Minimal consumer flake.
#
# Copy this file into your project as `flake.nix` and adjust the `hutian`
# input URL. The selected packages, plus their runtime closures, are mounted
# under /nix/store in the sandbox and their bin/ outputs are added to PATH.

{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.hutian.url = "path:/path/to/this/flake";

  outputs = { nixpkgs, hutian, ... }:
    hutian.lib.mkHutianShellFlake nixpkgs {
      packages = pkgs: with pkgs; [ coreutils git ];

      # Include Nix and connect it to the host Nix daemon:
      # nix = true;
    };
}
