{ pkgs }:

let
  base = pkgs.haskellPackages.callCabal2nix "hutian-shell" ../. { };
in
pkgs.haskell.lib.overrideCabal (pkgs.haskell.lib.dontHaddock base) (old: {
  testToolDepends = (old.testToolDepends or [ ]) ++ [
    pkgs.deadnix
    pkgs.hlint
    pkgs.statix
  ];

  preCheck = ''
    ${pkgs.hlint}/bin/hlint app test
    ${pkgs.statix}/bin/statix check .
    ${pkgs.deadnix}/bin/deadnix --fail .
  ''
  + (old.preCheck or "");
})
