{ pkgs }:

# Prepare the sandbox rootfs inside the private user and mount namespace
# created by the launcher, then run the sandbox command. gVisor recursively
# binds the rootfs into the sandbox, so binding paths over their rootfs
# mountpoints makes them visible without passing one volume per path.
#
# gVisor reserves file descriptors starting at 256 for stdio. A volume per store
# path exhausts that range once a closure has more than about 250 paths, and the
# sandbox then fails to start with an opaque EOF error.
let
  inherit (pkgs) util-linux;
  mount = "${util-linux}/bin/mount";
in
pkgs.writeShellScript "hutian-sandbox" ''
  set -euo pipefail

  rootfs=$1
  closure=$2
  sourcedir=$3
  writable=$4
  nix_mode=$5
  shift 5

  if [ "$nix_mode" = true ]; then
    # Nix mode exposes the whole host store, matching the daemon socket mount.
    ${mount} --rbind ${builtins.storeDir} "$rootfs${builtins.storeDir}"
  else
    while IFS= read -r path; do
      ${mount} --bind "$path" "$rootfs$path"
    done < "$closure/store-paths"
  fi

  # Use a recursive bind for the working directory. A plain bind fails with
  # EINVAL when the directory is a mountpoint with locked child mounts, such as
  # /nix (which contains /nix/store), and gVisor reports that failure as an EOF
  # while starting the sandbox. Recursive binds also keep nested mounts visible.
  ${mount} --rbind "$sourcedir" "$rootfs/source"

  if [ "$writable" = false ]; then
    ${mount} -o remount,bind,ro "$rootfs/source"

    # The remount above only covers the root of the bind. Apply it to every
    # nested mount too so read-only mode still holds when the working directory
    # contains mounts of its own. Mount points from /proc/self/mountinfo escape
    # spaces and backslashes, so decode those before remounting.
    while read -r _ _ _ _ target _; do
      target="''${target//\\040/ }"
      target="''${target//\\134/\\}"
      case "$target" in
        "$rootfs/source") ;;
        "$rootfs/source"/*)
          ${mount} -o remount,bind,ro "$target"
          ;;
      esac
    done < /proc/self/mountinfo
  fi

  exec "$@"
''
