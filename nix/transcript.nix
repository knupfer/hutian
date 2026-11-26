# Reserve a fresh transcript directory under the user's state directory.
#
# Usage: hutian-transcript-dir
#
# Prints the base path for the two transcript files. The timestamp gives a
# predictable name; mkdir makes the reservation atomic, and the numeric suffix
# handles two sessions starting in the same second.
{ pkgs }:

pkgs.writeShellApplication {
  name = "hutian-transcript-dir";
  runtimeInputs = with pkgs; [
    coreutils
  ];
  text = ''
    if [ -n "''${XDG_STATE_HOME:-}" ]; then
      state_home=$XDG_STATE_HOME
    elif [ -n "''${HOME:-}" ]; then
      state_home="$HOME/.local/state"
    else
      echo "hutian: cannot determine state directory for transcripts" >&2
      exit 1
    fi
    root="$state_home/hutian/transcripts"
    if ! mkdir -p "$root"; then
      echo "hutian: could not create transcript directory: $root" >&2
      exit 1
    fi

    stamp=$(date -u +%Y%m%dT%H%M%SZ)
    candidate="$root/$stamp"
    suffix=0
    while ! mkdir "$candidate" 2>/dev/null; do
      if [ -d "$candidate" ]; then
        suffix=$((suffix + 1))
        candidate="$root/$stamp-$suffix"
      else
        echo "hutian: could not create transcript directory: $root" >&2
        exit 1
      fi
    done

    printf '%s\n' "$candidate/transcript"
  '';
}
