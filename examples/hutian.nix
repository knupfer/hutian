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

    # Expose the host Nix store and connect to the Nix daemon.
    nixDaemon = false;

    # Grant direct network access. When false, the sandbox gets GVisor's none
    # network mode and GVISOR_NETWORK cannot re-enable it.
    network = false;
  };

  model = {
    name = "your-model";
    url = "http://127.0.0.1:8080/v1/chat/completions";

    # Optional: keep the API key on the host and expose only a private relay
    # socket to the sandbox. Set at most one of these. apiKeyFile must not
    # point into the Nix store, which the sandbox can read.
    # apiKeyFile = "/run/secrets/hutian-key";
    # apiKeyCommand = "pass show llama/api-key";

    # Grant the model's shell tool. Term mode is always available. Use "deny"
    # to hide the tool, "ask" to confirm each call, or "allow" to run
    # model-requested commands automatically.
    shell = "ask";

    systemPrompt = "You are a helpful coding assistant.";
  };
}
