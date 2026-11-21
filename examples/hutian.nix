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

    # Tool policies for the model. Term mode is always available.
    #
    # - `shell` controls the `shell`, `shell_background`, `jobs`, `job_output`,
    #   and `job_kill` tools.
    # - `webfetch` controls direct http/https fetches.
    # - `websearch` is null to disable search, a search URL, or an attribute
    #   set with `url` and `policy`. `{query}` or `%s` in the URL is replaced
    #   by the URL-encoded query; without one, `q=` is appended.
    # - `subagent` maps selector names to subagents. The main model sees each
    #   name and description in the tool schema. Each subagent runs serially
    #   with its own upstream model, system prompt, and tools, and returns only
    #   its final answer.
    #
    # Use "deny" to hide a tool, "ask" to confirm each call, or "allow" to run
    # model-requested calls automatically.
    tools = {
      shell = "ask";
      webfetch = "deny";
      websearch = null;
      subagent = { };

      # Example web search and summarization subagent:
      # websearch = {
      #   url = "https://search.example/?q=";
      #   policy = "ask";
      # };
      # subagent = {
      #   summarize = {
      #     model = "cheap-summary-model";
      #     description = "Fetch and summarize web pages when current information is needed.";
      #     systemPrompt = "Fetch pages and summarize them concisely.";
      #     tools = {
      #       shell = "deny";
      #       webfetch = "allow";
      #       websearch = {
      #         url = "https://search.example/?q=";
      #         policy = "allow";
      #       };
      #       subagent = { };
      #     };
      #   };
      # };
    };
  };
}
