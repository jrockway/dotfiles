# Global Claude Code Instructions

## Version control

Always use `jj` for all version control operations. Never run `git` commands — not even `git fetch`, `git log`, `git status`, `git diff`, `git push`, etc.

- `jj log` instead of `git log`
- `jj diff` instead of `git diff`
- `jj status` instead of `git status`
- `jj git push` to push branches
- `gh pr create` (gh CLI) for PRs after `jj git push`
- `jj git fetch` instead of `git fetch`

When moving changes between commits, prefer `jj squash --from <src> --into <dst>` over `jj restore --from <src> --to <dst>`. Squash moves changes (removing them from the source), which is idiomatic jj.

## Home Manager

To apply home-manager changes, run `hms` (alias for `nh home switch`). Do not suggest `home-manager switch` or `nixos-rebuild`.

## PR preparation flow

When asked to "make a PR", "prep a PR", "set up a PR commit", or similar:

1. **Check if `@` is directly on master.** Run `jj log -r '@ | ancestors(@, 5) | master'` to see the graph. If `@`'s parent is master, do nothing further.

2. **If not directly on master**, create a new commit on master:
   ```
   jj new master --no-edit
   ```
   Note the new change ID (e.g. `lxwrxttm`).

3. **Move changes from `@` into the new commit** (omitting anything private/internal):
   ```
   jj squash --from @ --into <new_change_id>
   ```
   Use `jj squash --from @ --into <id> -- <paths>` to select specific files if needed.

4. **Set description** (verify it was copied by jj squash):
   ```
   jj describe -r <new_change_id> -m "<message>"
   ```

5. **Search Linear for a relevant open ticket** assigned to the user (query by keywords from the commit message). Use the ticket's `gitBranchName` field for the bookmark name, or construct `june/<ticket-id>-<slug>`.

6. **Create bookmark** (do NOT push):
   ```
   jj bookmark create june/<ticket-slug> -r <new_change_id>
   ```

7. **Rebase `@` onto its original parent AND the new PR commit**:
   ```
   jj rebase -r @ -d <original_parent_change_id> -d <new_change_id>
   ```
