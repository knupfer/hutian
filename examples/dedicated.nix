# A dedicated Hutian flake.
#
# Copy this file into a project as `flake.nix` and adjust the `hutian` input
# URL. Then run:
#
#   nix run .#hutian
#
# The selected packages, plus their runtime closures, are mounted under
# /nix/store in the sandbox, and their bin/ outputs are added to PATH.
#
# To add Hutian to an existing project flake instead, see
# examples/existing-project.nix.

{
  description = "A project sandboxed with Hutian";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    hutian = {
      url = "path:/path/to/this/flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { nixpkgs, hutian, ... }:
    hutian.lib.mkFlake nixpkgs {
      sandbox = {
        packages = pkgs: [
          pkgs.coreutils
          pkgs.git
        ];

        # Allow writes to /source. The default is false (read-only); set true
        # when the model should be able to modify the project.
        writable = false;

        # Expose the host Nix store and connect to the Nix daemon.
        nixDaemon = false;

        # Grant direct network access. When false, the sandbox gets GVisor's
        # none network mode and GVISOR_NETWORK cannot re-enable it.
        network = false;
      };

      model = {
        name = "Qwen3.8-Flash-Next-IQ4_XS";
        url = "http://127.0.0.1:8080/v1/chat/completions";

        # Optional: keep the API key on the host and expose only a private
        # relay socket to the sandbox. Set at most one of these. apiKeyFile
        # must not point into the Nix store, which the sandbox can read.
        # apiKeyFile = "/run/secrets/hutian-key";
        # apiKeyCommand = "pass show llama/api-key";

        # Grant the model's shell tool. Term mode is always available. Use
        # "deny" to hide the tool, "ask" to confirm each call, or "allow" to
        # run model-requested commands automatically.
        shell = "ask";

        systemPrompt = "You are a coding agent.";
      };
    };
}
