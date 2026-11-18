{ pkgs, src }:

let
  inherit (pkgs.haskell.lib) dontHaddock overrideCabal;
  lint = import ./lint.nix { inherit pkgs src; };
  base = dontHaddock (pkgs.haskellPackages.callCabal2nix "hutian-shell" src { });
in
overrideCabal base (old: {
  # Run the lint checks as part of the package build.
  testDepends = (old.testDepends or [ ]) ++ builtins.attrValues lint;
  # Nix strips by default; being explicit keeps a future packaging change from
  # silently shipping a binary with symbols or debug sections.
  dontStrip = false;
})
