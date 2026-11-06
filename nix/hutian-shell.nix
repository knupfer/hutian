{ pkgs }:

pkgs.haskell.lib.dontHaddock (
  pkgs.haskellPackages.callCabal2nix "hutian-shell" ../haskell-shell { }
)
