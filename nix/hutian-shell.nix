{
  pkgs,
  src ? ../.,
}:

let
  inherit (pkgs) lib;

  lint = import ./lint.nix { inherit pkgs src; };
  base = pkgs.haskell.lib.dontHaddock (pkgs.haskellPackages.callCabal2nix "hutian-shell" src { });
in
pkgs.haskell.lib.overrideCabal base (old: {
  testDepends = (old.testDepends or [ ]) ++ lib.attrValues lint;
})
