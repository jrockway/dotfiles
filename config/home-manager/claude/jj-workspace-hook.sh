#!/bin/sh
# Claude Code UserPromptSubmit hook: inject the current jj workspace into
# Claude's context each prompt. Sessions are often started from the default
# workspace (/workspace, for shared project memory) while actually operating
# on another workspace, so the statusline alone isn't enough — Claude can't
# see it. A /workspace pin (see commands/workspace.md) declares the session's
# workspace; when the shell cwd has drifted off the pinned root (gh runs from
# the main checkout), report the pinned workspace and say so, instead of
# silently reporting whatever workspace the cwd landed in. Silent (exit 0, no
# output) outside multi-workspace jj repos.
input=$(cat)
cwd=$(echo "$input" | jq -r '.cwd // empty')
session_id=$(echo "$input" | jq -r '.session_id // empty')

pin_root=""
if [ -n "$session_id" ] && [ -f "$HOME/.claude/jj-workspace-pins/$session_id" ]; then
    pin_root=$(head -n1 "$HOME/.claude/jj-workspace-pins/$session_id")
    [ -d "$pin_root" ] || pin_root=""
fi

drifted=0
dir="$cwd"
if [ -n "$pin_root" ]; then
    dir="$pin_root"
    case "$cwd/" in
    "$pin_root"/*) ;;
    *) drifted=1 ;;
    esac
fi

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

if [ "$drifted" = 1 ]; then
    printf 'This session is pinned to jj workspace %s (root: %s) but the shell cwd is %s — cd back to the pinned root before jj commands or file edits unless June said otherwise.\n' \
        "${ws_name:-?}" "$ws_root" "$cwd"
fi
printf 'Current jj workspace: %s (root: %s). Run jj commands and file edits against this workspace root unless told otherwise. Other workspaces of this repo:\n' \
    "${ws_name:-?}" "$ws_root"
printf '%s\n' "$ws_list" | awk -F'\t' -v root="$ws_root" \
    '$2 != root {printf "  - %s: %s\n", $1, $2}'
