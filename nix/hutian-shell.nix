{ pkgs }:

let
  source = ../.;
  base = pkgs.haskellPackages.callCabal2nix "hutian-shell" source { };

  liquidCheck =
    pkgs.runCommand "hutian-liquid-check"
      {
        nativeBuildInputs = [
          (pkgs.haskellPackages.ghcWithPackages (h: [
            h.filepath
            h.liquidhaskell
          ]))
          pkgs.z3
        ];
      }
      ''
        cp -r ${source} input
        chmod -R u+w input
        cd input
        ghc -XDerivingStrategies -fno-code -iapp \
          -fplugin=LiquidHaskell \
          app/Hutian/Paths.hs \
          app/Hutian/Conversation.hs \
          app/Hutian/Editor.hs \
          app/Hutian/History.hs
        ${pkgs.coreutils}/bin/touch $out
      '';
in
pkgs.haskell.lib.overrideCabal (pkgs.haskell.lib.dontHaddock base) (old: {
  testDepends = (old.testDepends or [ ]) ++ [ liquidCheck ];
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
