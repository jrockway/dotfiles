# Auto mode classifier customization, merged into ~/.claude/settings.json
# alongside permissions.nix (see home.activation.claudeMergePermissions in
# home.nix).
#
# The deterministic `Bash(git *)` deny rule in permissions.nix already blocks
# plain git invocations in every mode; the hard_deny rule here makes the auto
# mode classifier catch disguised forms (`sh -c 'git ...'`, `env git ...`, git
# buried mid-pipeline). hard_deny rather than soft_deny so apparent user
# intent in the transcript never clears it — git is never OK here.
{
  hard_deny = [
    "$defaults"
    "Any invocation of the `git` CLI — directly (git status, git commit, git push, git fetch, etc.) or wrapped (`sh -c 'git ...'`, `env git ...`, `xargs git ...`, git anywhere in a pipeline or compound command). Version control on this machine is jj (jujutsu) only. NOT covered by this rule: `jj git push` / `jj git fetch` (those are jj subcommands) and the `gh` CLI."
  ];
}
