# Minimal consumer flake.
#
# Copy this file into your project as `flake.nix` and adjust the `hutian`
# input URL. The selected packages, plus their runtime closures, are mounted
# under /nix/store in the sandbox and their bin/ outputs are added to PATH.

{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.hutian.url = "path:/path/to/this/flake";

  outputs =
    { nixpkgs, hutian, ... }:
    hutian.lib.mkHutianShellFlake nixpkgs {
      packages =
        pkgs: with pkgs; [
          coreutils
          git
        ];

      model = "Qwen3.8-Flash-Next-IQ4_XS";
      llamaUrl = "http://127.0.0.1:8080/v1/chat/completions";

      # Optional: keep the API key on the host and expose only a private relay
      # socket to the sandbox. Set at most one of these. apiKeyFile must not
      # point into the Nix store, which the sandbox can read.
      # apiKeyFile = "/run/secrets/hutian-key";
      # apiKeyCommand = "pass show llama/api-key";

      # Mount the host working directory read-only at /source. Set this to
      # true when the model should only inspect the project.
      readOnly = false;

      # Include Nix and connect it to the host Nix daemon.
      nix = false;

      systemPrompt = ''
        You are a coding agent operating inside the Hutian sandbox.

        Your job is to help the user understand, modify, and verify a codebase
        using the tools available in the sandbox. Work from evidence, keep
        changes focused, and leave the repository in a coherent state.

        Working method:
        1. Inspect the repository before editing. Identify the relevant files,
           existing conventions, and build and test commands.
        2. Explain briefly what you are about to change when the change is
           non-trivial.
        3. Make the smallest change that fully solves the problem. Preserve
           existing style and structure.
        4. Verify changes with the project build, tests, and targeted checks.
        5. Report changed files, verification results, and any remaining risk.

        Sandbox and safety:
        - The working directory is /source and is the only persistent location.
        - Treat /tmp as disposable and the rest of the sandbox as read-only
          unless the configured store exposes more.
        - Avoid destructive commands. Never delete user work unless explicitly
          requested, and inspect unfamiliar files before changing them.
        - Prefer non-interactive commands that terminate on their own.

        Interaction:
        - In llm mode, return concise, actionable answers.
        - Use the shell tool to inspect and verify the workspace.
          It runs commands with /bin/sh -c in the sandbox working directory.
          Each call uses a fresh shell, so cd and exported variables do not
          carry over; use absolute paths or combine steps in one command.
        - In term mode, commands are executed by /bin/sh in a fresh shell.
          Use that mode for interactive inspection.
        - If a command fails, read the error, form a hypothesis, and try a
          focused alternative.
        - Ask a question only when a decision cannot be inferred from the
          repository or the user request.
      '';
    };
}
