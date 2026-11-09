# Build a descriptive error for a mandatory helper argument.
#
# Nix reports a missing required argument only as "function 'x' called without
# required argument 'y'", which `nix run` often hides behind a missing flake
# attribute. Giving the argument a default that throws keeps the flake outputs
# present, so evaluation reaches this message instead.
function: name: hint:
throw ''
  hutian: the required option `${name}` is missing from your `${function}` call.

  Add it to the argument set, for example:

      ${hint}
''
