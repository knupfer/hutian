{ pkgs, src }:

let
  inherit (pkgs) haskellPackages;
  inherit (pkgs.haskell.lib) dontHaddock overrideCabal;
  lint = import ./lint.nix { inherit pkgs src; };
  base = dontHaddock (haskellPackages.callCabal2nix "hutian-shell" src { });
in
overrideCabal base (old: {
  # Build the lint checks alongside the test suite.
  testDepends = (old.testDepends or [ ]) ++ builtins.attrValues lint;
  # Keep the sandboxed binary stripped. Nix strips by default; being explicit
  # prevents a future packaging change from silently shipping symbols.
  dontStrip = false;
})
