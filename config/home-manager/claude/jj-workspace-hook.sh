#!/bin/sh
# Claude Code UserPromptSubmit hook: inject the current jj workspace into
# Claude's context each prompt. Sessions are often started from the default
# workspace (/workspace, for shared project memory) while actually operating
# on another workspace, so the statusline alone isn't enough — Claude can't
# see it. Silent (exit 0, no output) outside multi-workspace jj repos.
input=$(cat)
dir=$(echo "$input" | jq -r '.cwd // empty')
if [ -n "$dir" ] && [ -d "$dir" ]; then
    cd "$dir" 2>/dev/null || exit 0
fi

ws_root=$(jj --no-pager --ignore-working-copy workspace root 2>/dev/null)
[ -n "$ws_root" ] || exit 0

ws_list=$(jj --no-pager --ignore-working-copy workspace list \
    -T 'self.name() ++ "\t" ++ self.root() ++ "\n"' 2>/dev/null)
[ "$(printf '%s\n' "$ws_list" | grep -c .)" -gt 1 ] || exit 0

ws_name=$(printf '%s\n' "$ws_list" |
    awk -F'\t' -v root="$ws_root" '$2 == root {print $1}')

printf 'Current jj workspace: %s (root: %s). Run jj commands and file edits against this workspace root unless told otherwise. Other workspaces of this repo:\n' \
    "${ws_name:-?}" "$ws_root"
printf '%s\n' "$ws_list" | awk -F'\t' -v root="$ws_root" \
    '$2 != root {printf "  - %s: %s\n", $1, $2}'
