{ pkgs, src }:

let
  inherit (pkgs.haskell.lib) justStaticExecutables overrideCabal;
  lint = import ./lint.nix { inherit pkgs src; };
  base = pkgs.haskellPackages.callCabal2nix "hutian-shell" src { };
in
# The sandbox only runs the executables, so build them as static executables.
# This drops the shared Haskell library, its package database, the propagated
# build inputs, and the dependency doc references from the output, keeping the
# Haskell dependency closure out of the sandbox store.
justStaticExecutables (
  overrideCabal base (old: {
    # Run the lint checks as part of the package build.
    testDepends = (old.testDepends or [ ]) ++ builtins.attrValues lint;
    # Nix strips by default; being explicit keeps a future packaging change from
    # silently shipping a binary with symbols or debug sections.
    dontStrip = false;
  })
)
