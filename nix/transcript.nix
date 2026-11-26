# Reserve a fresh transcript directory under the project's .hutian directory.
#
# Usage: hutian-transcript-dir SOURCE_DIR
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
    root="$1/.hutian/transcripts"
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
