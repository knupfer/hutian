{
  pkgs,
  src ? ../.,
}:

let
  source = src;
  lint = import ./lint.nix {
    inherit pkgs;
    src = source;
  };
  base = pkgs.haskellPackages.callCabal2nix "hutian-shell" source { };
in
pkgs.haskell.lib.overrideCabal (pkgs.haskell.lib.dontHaddock base) (old: {
  testDepends = (old.testDepends or [ ]) ++ [
    lint.haskell
    lint.liquid
    lint.nix
  ];
})
