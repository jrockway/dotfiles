# Global Claude Code permissions, merged into ~/.claude/settings.json on every
# `hms` activation (see home.activation.claudeMergePermissions in home.nix).
#
# Claude Code merges permission allow/deny/ask lists across all settings scopes,
# so anything here applies to every project. This file is the source of truth for
# the *global* allow/ask lists — entries added at runtime with user scope are
# reset to this list on the next switch.
#
# Consolidated from the per-project .claude/settings.local.json files. Two
# hash-pinned /nix/store/<hash> entries were intentionally dropped because the
# hash changes on every rebuild, so they could never match again.
{
  allow = [
    # Shell / build
    "Bash(go build *)"
    "Bash(go test *)"
    "Bash(go vet *)"
    "Bash(grep *)"
    "Bash(col -b)"
    "Bash(dpkg -l)"
    "Bash(echo \"exit=$?\")"

    # jj
    "Bash(jj show *)"
    "Bash(jj diff *)"
    "Bash(jj st *)"
    "Bash(jj log *)"
    "Bash(jj log:*)"
    "Bash(jj new *)"
    "Bash(jj describe *)"

    # gh (read-only; mutating calls are gated by the ask list below)
    "Bash(gh pr view *)"
    "Bash(gh api:*)"

    # nix / home-manager
    "Bash(nix build *)"
    "Bash(nix eval *)"
    "Bash(home-manager generations *)"
    "Bash(nix --extra-experimental-features 'nix-command flakes' build 'path:/home/vscode/.dotfiles/config/home-manager#homeConfigurations.vscode.activationPackage' --no-link --print-build-logs)"

    # baseten / local tooling
    "Bash(/usr/bin/dockerd --version)"
    "Bash(/usr/bin/containerd --version)"
    "Bash(grafana-server -v)"
    "Bash(ls -la /workspace 2>&1 | head -50)"
    "Bash(moon run beefeater:lint)"

    # Reads
    "Read(//usr/bin/**)"
    "Read(//tmp/**)"
    "Read(//home/vscode/.dotfiles/**)"
    "Read(//home/jrockway/.dotfiles/**)"
    "Read(//home/vscode/.local/state/home-manager/gcroots/**)"
    "Read(//home/jrockway/.local/state/home-manager/gcroots/**)"
    "Read(//home/vscode/.local/state/nix/profiles/**)"
    "Read(//home/jrockway/.local/state/nix/profiles/**)"
    "Read(//workspace/helm/charts/**)"

    # Web
    "WebSearch"
    "WebFetch(domain:github.com)"
    "WebFetch(domain:mynixos.com)"
    "WebFetch(domain:raw.githubusercontent.com)"

    # MCP — Honeycomb
    "mcp__claude_ai_Honeycomb__get_workspace_context"
    "mcp__claude_ai_Honeycomb__list_spans"
    "mcp__claude_ai_Honeycomb__get_span_details"
    "mcp__claude_ai_Honeycomb__run_query"
    "mcp__claude_ai_Honeycomb__get_query_results"
    "mcp__claude_ai_Honeycomb__get_trace"
    "mcp__claude_ai_Honeycomb__get_environment"

    # MCP — Grafana
    "mcp__grafana__list_datasources"
    "mcp__grafana__query_prometheus"
    "mcp__grafana__list_prometheus_metric_names"
    "mcp__grafana__generate_deeplink"
    "mcp__grafana__search_folders"
    "mcp__grafana-staging__list_datasources"
    "mcp__grafana-staging__list_prometheus_metric_names"
    "mcp__grafana-staging__query_prometheus"
    "mcp__grafana-staging__search_folders"
    "mcp__grafana-staging__generate_deeplink"

    # MCP — Notion / Linear
    "mcp__claude_ai_Notion__notion-fetch"
    "mcp__claude_ai_Linear__list_issues"
  ];

  # Mutating gh api calls always prompt, even though `gh api:*` is allowed above.
  ask = [
    "Bash(gh api -X *)"
    "Bash(gh api * -X *)"
    "Bash(gh api --method *)"
    "Bash(gh api * --method *)"
    "Bash(gh api -f *)"
    "Bash(gh api * -f *)"
    "Bash(gh api -F *)"
    "Bash(gh api * -F *)"
    "Bash(gh api --field *)"
    "Bash(gh api * --field *)"
    "Bash(gh api --raw-field *)"
    "Bash(gh api * --raw-field *)"
  ];
}
