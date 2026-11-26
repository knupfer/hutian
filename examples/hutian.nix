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

  endpoint = {
    url = "http://127.0.0.1:8080/v1/chat/completions";

    # Set apiKeyCommand to a command that prints the key, or null to forward
    # without authentication. For a key file on the host, use
    # `cat /run/secrets/hutian-key`. Keep the command from reading a file under
    # /source or the Nix store, which the sandbox can read.
    # apiKeyCommand = "pass show llama/api-key";
    apiKeyCommand = null;
  };

  output = {
    verbosity = "high";
    # Record each session under .hutian/transcripts/<timestamp>/.
    transcript = false;
  };

  agent = {
    model = "your-model";
    systemPrompt = "You are a helpful coding assistant.";

    # Tool policies for the agent. Term mode is always available.
    #
    # - `shell` controls the `shell`, `shell_background`, `jobs`, `job_output`,
    #   and `job_kill` tools.
    # - `webfetch` controls direct http/https fetches.
    # - `subagent` maps selector names to subagents. The main agent sees each
    #   name and description in the tool schema. Each subagent runs serially
    #   with its own upstream model, system prompt, and tools, and returns only
    #   its final answer.
    # - `mcp` maps names to MCP servers. Hutian discovers each server's tools at
    #   startup and advertises them with the API-safe name
    #   `mcp__<server>__<tool>`; prompts display `mcp.<server>.<tool>`. Each
    #   server requires a `url`, a `policy`, and a `handlers` map; use
    #   `handlers = { };` when none of its tools needs a result handler. The Exa
    #   server works without a key for search and fetch; add an explicit API key
    #   through a local MCP proxy if you need higher limits. MCP runs in the
    #   sandbox, so `sandbox.network = true` is required.
    #
    # Use "deny" to hide a tool, "ask" to confirm each call, or "allow" to run
    # model-requested calls automatically.
    tools = {
      shell = "ask";
      webfetch = "deny";
      subagent = { };
      mcp = { };

      # Example MCP server:
      # mcp = {
      #   exa = {
      #     url = "https://mcp.exa.ai/mcp";
      #     policy = "ask";
      #
      #     # Route a concrete MCP tool result through a subagent configuration.
      #     # The handler gets the raw result and returns only its answer. Its
      #     # description is appended to the tool description; the terminal
      #     # label is `mcp.<server>.<tool>.handler`.
      #     handlers.search = {
      #       model = "cheap-summarizer";
      #       description = "Summarize search results; preserve facts and links.";
      #       systemPrompt = "You summarize tool output for another LLM. Be terse.";
      #       tools = {
      #         shell = "deny";
      #         webfetch = "deny";
      #         subagent = { };
      #         mcp = { };
      #       };
      #     };
      #   };
      # };

      # Example web fetch and summarization subagent:
      # subagent = {
      #   summarize = {
      #     model = "cheap-summary-model";
      #     description = "Fetch and summarize web pages when current information is needed.";
      #     systemPrompt = "Fetch pages and summarize them concisely.";
      #     tools = {
      #       shell = "deny";
      #       webfetch = "allow";
      #       subagent = { };
      #       mcp = { };
      #     };
      #   };
      # };
    };
  };
}
