---
description: Open a jj workspace (by name or by PR) in a new tmux window
model: haiku
argument-hint: <workspace-name | PR number | PR URL>
---

June wants the jj workspace identified by "$1" opened in a new tmux window
(tab) of her current tmux session.

0. tmux is required: if `$TMUX` is unset, say so and stop. Creating a window in
   June's default tmux server is fine here — she asked for it; the
   private-socket rule (`tmux -L claude`) governs sessions Claude creates for
   its own use, not windows June requests in hers.

1. Resolve the target workspace name and root:
   - **No argument**: this session's current workspace — the pinned one if
     `~/.claude/jj-workspace-pins/$CLAUDE_CODE_SESSION_ID` exists (it contains
     the root path), else the shell cwd's workspace.
   - **Workspace name**: map name to root via

     ```
     jj -R /workspace --ignore-working-copy workspace list -T 'self.name() ++ "\t" ++ self.root() ++ "\n"'
     ```

   - **PR number or GitHub PR URL**: map PR → head bookmark → the workspace
     whose working copy sits on top of it:

     ```
     bookmark=$(gh pr view <number-or-url> --json headRefName -q .headRefName)
     jj -R /workspace --ignore-working-copy log --no-graph \
       -r "working_copies() & descendants($bookmark)" -T 'working_copies ++ "\n"'
     ```

     The output contains entries like `name@` — strip the trailing `@`, then
     map the name to its root with the workspace list above. If the revset
     errors with an unknown revision, run `jj -R /workspace git fetch` once
     and retry. Several matches: open the first, mention the rest. No match:
     no workspace's working copy descends from that PR's bookmark — tell June
     and offer `/workspace` to create one; never invent a directory.

2. Open the tab without stealing focus (June is mid-conversation; the new tab
   shows up in her tmux status bar):

   ```
   tmux new-window -d -c <root> -n <workspace-name>
   ```

   `-n` pins the window name so the tab stays identifiable.

3. Report the new window's index and name plus the workspace root, as plain
   text (bare paths, no markdown links).
