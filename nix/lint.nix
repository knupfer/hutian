{ pkgs, src }:

let
  liquidGhc = pkgs.haskellPackages.ghcWithPackages (h: [
    h.filepath
    h.liquidhaskell
  ]);

  mkCheck =
    name: nativeBuildInputs: script:
    pkgs.runCommand name { inherit nativeBuildInputs; } ''
      cp -r ${src} source
      chmod -R u+w source
      cd source
      ${script}
      touch $out
    '';
in
{
  haskell = mkCheck "hutian-hlint" [ pkgs.hlint ] "hlint app test";

  nix =
    mkCheck "hutian-nix-lint"
      [
        pkgs.deadnix
        pkgs.statix
      ]
      ''
        statix check .
        deadnix --fail .
      '';

  liquid =
    mkCheck "hutian-liquid"
      [
        liquidGhc
        pkgs.z3
      ]
      ''
        ghc -XDerivingStrategies -fno-code -iapp \
          -fplugin=LiquidHaskell \
          app/Hutian/Completion.hs \
          app/Hutian/Paths.hs \
          app/Hutian/Conversation.hs \
          app/Hutian/Editor.hs \
          app/Hutian/History.hs
      '';
}
