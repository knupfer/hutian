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

  readOnlyWrapper = import ./read-only.nix { inherit pkgs; };
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

  readOnlyMount =
    mkCheck "hutian-read-only-mount"
      [
        pkgs.bash
        pkgs.util-linux
      ]
      ''
        mkdir writable readonly
        echo original > readonly/file
        echo original > writable/file

        # Control: an unconfined write succeeds.
        ${pkgs.bash}/bin/bash -c 'echo changed > "$1/file"' bash "$PWD/writable"
        test "$(cat writable/file)" = changed

        # The read-only wrapper rejects writes and leaves the file intact.
        if ${readOnlyWrapper} "$PWD/readonly" \
             ${pkgs.bash}/bin/bash -c 'echo changed > "$1/file"' bash "$PWD/readonly"; then
          echo "write to a read-only working directory unexpectedly succeeded" >&2
          exit 1
        fi
        test "$(cat readonly/file)" = original
      '';
}
