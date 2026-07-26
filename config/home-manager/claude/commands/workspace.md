---
description: Operate on the named jj workspace for the rest of this session
argument-hint: <workspace-name>
---

June wants this session to operate on the jj workspace named "$1".

1. Resolve workspaces. Try the current directory's repo first; if it isn't a
   jj repo, fall back to the baseten monorepo with `-R /workspace`:

   ```
   jj --ignore-working-copy workspace list -T 'self.name() ++ "\t" ++ self.root() ++ "\n"'
   ```

2. If a workspace named "$1" exists: `cd` the shell to its root and stay
   there — run all jj commands and file edits against that root for the rest
   of the session unless told otherwise. Then run `jj st` from the root and
   report the workspace name, root, and current change (id + first line of
   description) so June can confirm. The statusline and the per-prompt
   workspace context line both key off the shell cwd, so they will reflect
   the switch automatically.

3. If it doesn't exist: show the available workspace names and ask June
   whether to create it. Never invent a location — new baseten monorepo
   workspaces always go in `~/monorepo-workspaces/<name>` (June kept losing
   workspaces created at ad-hoc paths). On yes:

   ```
   mkdir -p ~/monorepo-workspaces
   jj -R /workspace workspace add --name $1 -r <base> ~/monorepo-workspaces/$1
   ```

   Base the new working copy on master unless June says otherwise. Then cd
   there and report as in step 2.

4. If no name was given, report the current workspace and list the others.
