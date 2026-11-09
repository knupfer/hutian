{ pkgs, src }:

let
  copySource = ''
    cp -r ${src} source
    chmod -R u+w source
    cd source
  '';

  liquidGhc = pkgs.haskellPackages.ghcWithPackages (h: [
    h.filepath
    h.liquidhaskell
  ]);
in
{
  haskell =
    pkgs.runCommand "hutian-hlint"
      {
        nativeBuildInputs = [ pkgs.hlint ];
      }
      ''
        ${copySource}
        hlint app test
        ${pkgs.coreutils}/bin/touch $out
      '';

  nix =
    pkgs.runCommand "hutian-nix-lint"
      {
        nativeBuildInputs = [
          pkgs.deadnix
          pkgs.statix
        ];
      }
      ''
        ${copySource}
        statix check .
        deadnix --fail .
        ${pkgs.coreutils}/bin/touch $out
      '';

  liquid =
    pkgs.runCommand "hutian-liquid"
      {
        nativeBuildInputs = [
          liquidGhc
          pkgs.z3
        ];
      }
      ''
        ${copySource}
        ghc -XDerivingStrategies -fno-code -iapp \
          -fplugin=LiquidHaskell \
          app/Hutian/Paths.hs \
          app/Hutian/Conversation.hs \
          app/Hutian/Editor.hs \
          app/Hutian/History.hs
        ${pkgs.coreutils}/bin/touch $out
      '';
}
