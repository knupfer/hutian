# Mount the closure, working directory, and relay directory into the sandbox
# rootfs, then run the sandbox command. The launcher calls this inside a private
# user and mount namespace. gVisor recursively binds the rootfs into the
# sandbox, so binding paths over their rootfs mountpoints makes them visible
# without passing one volume per path.
#
# Usage: hutian-sandbox <rootfs> <closure> <source-dir> <writable> <nix-daemon> <relay-dir> <command...>
#
# gVisor reserves file descriptors starting at 256 for stdio. A volume per store
# path exhausts that range once a closure has more than about 250 paths, and the
# sandbox then fails to start with an opaque EOF error.
{ pkgs }:
let
  inherit (pkgs) util-linux;
  mount = "${util-linux}/bin/mount";
in
pkgs.writeShellScript "hutian-sandbox" ''
  set -euo pipefail

  rootfs=$1
  closure=$2
  source_dir=$3
  writable=$4
  nix_daemon=$5
  relay_dir=$6
  shift 6

  if [ "$nix_daemon" = true ]; then
    # Nix daemon mode exposes the whole host store, matching the daemon socket
    # mount.
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
  ${mount} --rbind "$source_dir" "$rootfs/source"

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

  # The relay socket is connectable from a read-only mount, but a sandboxed
  # command cannot unlink or replace the socket file.
  if [ -n "$relay_dir" ]; then
    ${mount} --rbind "$relay_dir" "$rootfs/run/hutian"
    ${mount} -o remount,bind,ro "$rootfs/run/hutian"
  fi

  exec "$@"
''
