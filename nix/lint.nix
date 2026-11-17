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

  sandboxLauncher = import ./sandbox.nix { inherit pkgs; };

  # The sandbox launcher has to be exercised inside a private user and mount
  # namespace. This script creates a mountpoint with a nested mount, then checks
  # that the launcher keeps both visible and honours the writable option for both.
  sandboxSourceTest = pkgs.writeShellScript "hutian-sandbox-source-test" ''
    set -euo pipefail

    rootfs=$1
    closure=$2
    work=$3
    sandbox=$4
    src="$work/source dir"

    mkdir -p "$src"
    ${pkgs.util-linux}/bin/mount -t tmpfs tmpfs "$src"
    mkdir -p "$src/child"
    ${pkgs.util-linux}/bin/mount -t tmpfs tmpfs "$src/child"
    echo outer > "$src/file"
    echo inner > "$src/child/file"

    # Writable: nested mounts and writes are preserved.
    "$sandbox" "$rootfs" "$closure" "$src" true false "" \
      ${pkgs.bash}/bin/bash -c '
        set -e
        test "$(cat "$1/file")" = outer
        test "$(cat "$1/child/file")" = inner
        echo changed > "$1/file"
        test "$(cat "$2/file")" = changed
      ' bash "$rootfs/source" "$src"

    # Read-only: writes through the root and the nested mount are rejected.
    "$sandbox" "$rootfs" "$closure" "$src" false false "" \
      ${pkgs.bash}/bin/bash -c '
        set -e
        test "$(cat "$1/file")" = changed
        test "$(cat "$1/child/file")" = inner
        if { echo nope > "$1/file"; } 2>/dev/null; then exit 1; fi
        if { echo nope > "$1/child/file"; } 2>/dev/null; then exit 1; fi
      ' bash "$rootfs/source"

    test "$(cat "$src/file")" = changed
    test "$(cat "$src/child/file")" = inner

    # The relay directory is mounted read-only.
    relay="$work/relay dir"
    mkdir -p "$relay" "$rootfs/run/hutian"
    echo relay > "$relay/file"
    "$sandbox" "$rootfs" "$closure" "$src" false false "$relay" \
      ${pkgs.bash}/bin/bash -c '
        set -e
        test "$(cat "$1/file")" = relay
        if { echo nope > "$1/new"; } 2>/dev/null; then exit 1; fi
      ' bash "$rootfs/run/hutian"
  '';
in
{
  haskell = mkCheck "hutian-hlint" [ pkgs.hlint ] "hlint app exe test";

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
          app/Hutian/Key.hs \
          app/Hutian/Paths.hs \
          app/Hutian/Conversation.hs \
          app/Hutian/Editor.hs \
          app/Hutian/History.hs \
          app/Hutian/Prompt.hs
      '';

  # Check that closure paths of any size and plain files are bind-mounted into
  # the rootfs instead of being passed as one gVisor volume per path.
  sandboxClosure =
    mkCheck "hutian-sandbox-closure"
      [
        pkgs.bash
        pkgs.coreutils
        pkgs.util-linux
      ]
      ''
        mkdir -p rootfs/source closure source
        : > closure/store-paths

        i=0
        while [ "$i" -lt 300 ]; do
          path="$PWD/fake/$i"
          mkdir -p "$path" "rootfs$path"
          printf 'value-%s\n' "$i" > "$path/value"
          printf '%s\n' "$path" >> closure/store-paths
          i=$((i + 1))
        done

        file="$PWD/fake-file"
        printf 'file-value\n' > "$file"
        mkdir -p "rootfs$(dirname "$file")"
        : > "rootfs$file"
        printf '%s\n' "$file" >> closure/store-paths

        ${pkgs.util-linux}/bin/unshare --user --map-root-user --mount --propagation private \
          ${sandboxLauncher} "$PWD/rootfs" "$PWD/closure" "$PWD/source" true false "" \
          ${pkgs.bash}/bin/bash -c '
            set -e
            test "$(cat "$1")" = value-299
            test "$(cat "$2")" = file-value
          ' bash "$PWD/rootfs$PWD/fake/299/value" "$PWD/rootfs$PWD/fake-file"
      '';

  sandboxSource =
    mkCheck "hutian-sandbox-source"
      [
        pkgs.bash
        pkgs.coreutils
        pkgs.util-linux
      ]
      ''
        mkdir -p rootfs/source closure
        : > closure/store-paths

        ${pkgs.util-linux}/bin/unshare --user --map-root-user --mount --propagation private \
          ${sandboxSourceTest} "$PWD/rootfs" "$PWD/closure" "$PWD/work" ${sandboxLauncher}
      '';
}
