{ pkgs }:

# Bind a directory read-only in a private user and mount namespace, then run
# the given command with the remaining arguments. gVisor's `do` command has no
# per-volume read-only flag, so the sandbox inherits this read-only bind.
pkgs.writeShellScript "hutian-read-only" ''
  exec ${pkgs.util-linux}/bin/unshare --user --map-root-user --mount --propagation private \
    ${pkgs.bash}/bin/bash -c '
      set -e
      ${pkgs.util-linux}/bin/mount --bind "$1" "$1"
      ${pkgs.util-linux}/bin/mount -o remount,bind,ro "$1" "$1"
      shift
      exec "$@"
    ' bash "$@"
''
