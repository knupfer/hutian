# Example consumer flake.
#
# Copy this file into your project as `flake.nix` and adjust the
# `gvisor-shell` input URL.
#
# The selected packages, plus their full runtime closures, are mounted under
# /nix/store in the sandbox and their bin/ outputs are added to PATH.
#
# Add `nix = true;` below to include Nix and connect it to the host Nix daemon.

{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.gvisor-shell.url = "path:/path/to/this/flake";

  outputs = { nixpkgs, gvisor-shell, ... }:
    gvisor-shell.lib.mkHutianShellFlake nixpkgs {
      packages = pkgs: with pkgs; [ coreutils git ];
    };
}
