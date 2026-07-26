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

  # The classifier's built-in background-operator caution prompts on every
  # `testbedctl start ... &`; June always approves, so bless that command
  # family. Scoped per `claude auto-mode critique`: named subcommands only
  # (destructive verbs keep the default treatment), log path constrained,
  # chained extras still evaluated normally.
  allow = [
    "$defaults"
    "Backgrounding `testbedctl` with the shell `&` operator is allowed for its non-destructive lifecycle subcommands: `testbedctl start > <logfile> 2>&1 &` (log path under the session scratchpad, /tmp, or the project directory) chained with `sleep`, `testbedctl wait`, `testbedctl net`, or `testbedctl status`. Applies only to the plain `testbedctl` binary on PATH — not scripts of the same name written this session. Destructive testbedctl verbs and any unrelated commands chained alongside are evaluated normally; this exception blesses only the backgrounding."
  ];

  # Context for edge cases the specific rules don't anticipate.
  environment = [
    "$defaults"
    "**testbedctl**: CLI for the local development testbed on this machine — a machine-local resource shared between sessions, not a production or remote system."
  ];
}
