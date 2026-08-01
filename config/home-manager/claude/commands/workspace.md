---
description: Operate on the named jj workspace for the rest of this session
model: haiku
argument-hint: <workspace-name>
---

June wants this session to operate on the jj workspace named "$1".

1. Resolve workspaces. Try the current directory's repo first; if it isn't a jj
   repo, fall back to the baseten monorepo with `-R /workspace`:

   ```
   jj --ignore-working-copy workspace list -T 'self.name() ++ "\t" ++ self.root() ++ "\n"'
   ```

2. If a workspace named "$1" exists: `cd` the shell to its root and stay there —
   run all jj commands and file edits against that root for the rest of the
   session unless told otherwise. Then pin the session to it, so the statusline
   and the per-prompt context line show this workspace even when the shell cwd
   drifts (e.g. `gh` runs from the main checkout):

   - Run `mkdir -p ~/.claude/jj-workspace-pins && echo $CLAUDE_CODE_SESSION_ID`
     and use the Write tool to put the workspace root path (one line) in
     `~/.claude/jj-workspace-pins/<session-id>`.
   - Prune old pins: `find ~/.claude/jj-workspace-pins -type f -mtime +14 -delete`

   After any excursion off the pinned root (gh from the main checkout, edits
   in another repo), cd back. Finally run `jj st` from the root and report the
   workspace name, root, and current change (id + first line of description)
   so June can confirm.

3. If it doesn't exist: show the available workspace names and ask June whether
   to create it. Never invent a location — new baseten monorepo workspaces
   always go in `~/monorepo-workspaces/<name>` (June kept losing workspaces
   created at ad-hoc paths). On yes:

   ```
   mkdir -p ~/monorepo-workspaces
   jj -R /workspace workspace add --name $1 -r <base> ~/monorepo-workspaces/$1
   ```

   Base the new working copy on master unless June says otherwise. Then cd
   there, pin, and report as in step 2.

4. If no name was given, report the current workspace and list the others.
