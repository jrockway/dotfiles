# Auto mode classifier customization, merged into ~/.claude/settings.json
# alongside permissions.nix (see home.activation.claudeMergeSettings in
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
    # A glob push (june/net-*) once shipped several bookmarks June never meant
    # to publish. Pushes must name each bookmark exactly; hard_deny because no
    # transcript context ever makes a pattern push OK.
    "Any `jj git push` that selects bookmarks by pattern instead of exact name — a `--bookmark` value with a `glob:` prefix or containing `*`/`?` wildcards (e.g. `jj git push --bookmark 'glob:june/net-*'` or `jj git push --bookmark june/net-*`), including wrapped or mid-pipeline forms. Bookmarks must be pushed by exact literal name, repeating `--bookmark <name>` once per bookmark. NOT covered by this rule: pushes that name every bookmark explicitly, which are evaluated normally."
  ];

  # gh api reads vs writes are judged here, not by permissions.nix globs: the
  # old ask list missed `-XPOST`, `--method=DELETE`, `--input`, and a repeated
  # `-X` (the last one wins). soft_deny rather than hard_deny so that June
  # asking for a write in the transcript clears it. The read side is in allow.
  soft_deny = [
    "$defaults"
    "GitHub writes through `gh api`, unless June asked for that write in this conversation: any call whose effective HTTP method is POST, PUT, PATCH or DELETE — set explicitly in any spelling (`-X POST`, `-XPOST`, `--method=DELETE`; when given more than once, the last one wins), or implicitly by `-f`/`-F`/`--field`/`--raw-field`/`--input` without a final `--method GET` — and any `gh api graphql` call whose query is a mutation or whose query text isn't visible in the command (e.g. `-F query=@file`). June's request clears only the writes she asked for, not similar writes later in the session."
  ];

  # The classifier's built-in background-operator caution prompts on every
  # `testbedctl start ... &`; June always approves, so bless that command
  # family. Scoped per `claude auto-mode critique`: named subcommands only
  # (destructive verbs keep the default treatment), log path constrained,
  # chained extras still evaluated normally.
  allow = [
    "$defaults"
    "Backgrounding `testbedctl` with the shell `&` operator is allowed for its non-destructive lifecycle subcommands: `testbedctl start > <logfile> 2>&1 &` (log path under the session scratchpad, /tmp, or the project directory) chained with `sleep`, `testbedctl wait`, `testbedctl net`, or `testbedctl status`. Applies only to the plain `testbedctl` binary on PATH — not scripts of the same name written this session. Destructive testbedctl verbs and any unrelated commands chained alongside are evaluated normally; this exception blesses only the backgrounding."
    "Posting a PR comment whose body is a `/release-owner-bypass #<PR-number>` directive to the basetenlabs/release-platform repo during a release is always allowed — e.g. `gh pr comment 6863 --repo basetenlabs/release-platform --body \"/release-owner-bypass #6830\"`. Despite the word bypass, this is a normal, sanctioned step of the release process, not a security or review bypass. Scope: only `gh pr comment` targeting basetenlabs/release-platform with a body consisting of the `/release-owner-bypass #<number>` directive; comments to other repos, other comment bodies, and any commands chained alongside are evaluated normally."
    "Running the monorepo release CLI to open a promotion PR — `cdli.sh apps promote <team>/<app> --source <ring> --destination <ring>` (cdli.sh at any path inside a baseten monorepo checkout, output optionally piped through harmless filters like `tail` or `head`) — is allowed. Despite being release tooling, `apps promote` only updates a version pin in the basetenlabs/release-platform repo and opens a PR that requires human review and a human merge; it deploys nothing by itself. Scope: the `apps promote` subcommand only; other cdli subcommands and any unrelated commands chained alongside are evaluated normally."
    # Read side of the gh api soft_deny above.
    "Read-only GitHub API calls through `gh api` are allowed: calls whose effective HTTP method is GET, and `gh api graphql` queries whose query text is in the command and contains no `mutation`. gh sends GET when no `-X`/`--method` is given (in any spelling, including `-XPOST` and `--method=POST`) and none of `-f`/`-F`/`--field`/`--raw-field`/`--input` is present; field flags or `--input` switch it to POST unless the method is set explicitly. When `-X`/`--method` is given more than once, the last one wins, and with `--method GET` field flags become query-string parameters, so `gh api -X GET search/issues -f q=...` is a read. Covers `--paginate`, `--jq`/`--template`, and piping the output through filters like `jq`, `head` or `tail`. Scope: reads only; writes fall under the gh api soft_deny rule, and anything else chained alongside is evaluated normally."
    "Editing, creating or deleting files (Edit/Write tools or shell file edits) inside a version-controlled working copy — /workspace, ~/monorepo-workspaces/*, ~/release-platform, ~/release-platform-workspaces/*, ~/flux-cd, ~/flux-cd-workspaces/*, ~/truss, ~/truss-workspaces/* and other jj checkouts — is always allowed, even when the files are production deployment config (release-platform, flux-cd, helm values, alert rules). A local edit deploys nothing: it reaches production only via `jj git push`, a PR, human review and a human merge, which are evaluated separately. Scope: file contents inside the checkout only; pushes, PR creation and commands acting on remote systems are evaluated normally. NOT covered: Claude's own permission or auto-mode config (~/.claude/settings*.json, ~/.dotfiles/config/home-manager/claude/permissions.nix and auto-mode.nix)."
  ];

  # Context for edge cases the specific rules don't anticipate.
  environment = [
    "$defaults"
    "**testbedctl**: CLI for the local development testbed on this machine — a machine-local resource shared between sessions, not a production or remote system."
  ];
}
