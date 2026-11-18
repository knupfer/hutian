# Shared Hutian configuration for the example flakes.
#
# `sandbox.packages` is a selector, so the same configuration works with any
# `pkgs` instance. Keep this file next to the flake that imports it.

{
  sandbox = {
    packages = pkgs: [
      pkgs.coreutils
      pkgs.git
    ];

    # Allow writes to /source. The default is false (read-only); set true when
    # the model should be able to modify the project.
    writable = false;

    # Expose the host Nix store and connect to the host Nix daemon.
    nixDaemon = false;

    # Grant direct network access. When false, the sandbox gets gVisor's none
    # network mode and GVISOR_NETWORK cannot re-enable it.
    network = false;
  };

  model = {
    name = "your-model";
    url = "http://127.0.0.1:8080/v1/chat/completions";
    systemPrompt = "You are a helpful coding assistant.";

    # Set apiKeyCommand to a command that prints the key, or null to forward
    # without authentication. For a key file on the host, use
    # `cat /run/secrets/hutian-key`. Keep the command from reading a file under
    # /source or the Nix store, which the sandbox can read.
    # apiKeyCommand = "pass show llama/api-key";
    apiKeyCommand = null;

    # Grant the model's shell tool. Term mode is always available. Use "deny"
    # to hide the tool, "ask" to confirm each call, or "allow" to run
    # model-requested commands automatically.
    shell = "ask";
  };
}
