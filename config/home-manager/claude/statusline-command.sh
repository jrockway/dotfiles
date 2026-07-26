#!/bin/sh
# Claude Code statusLine — jj-centric.
# Format: ws:<name> <id> [bookmark] <desc...> +N-M ↑K [⚠ conflict|(empty)] | model ctx:X%
input=$(cat)
model=$(echo "$input" | jq -r '.model.display_name // empty')
used=$(echo "$input" | jq -r '.context_window.used_percentage // empty')

# Run jj from the session's live cwd, not the launch dir: sessions started in
# the default workspace often operate on a different jj workspace entirely.
current_dir=$(echo "$input" | jq -r '.workspace.current_dir // .cwd // empty')
if [ -n "$current_dir" ] && [ -d "$current_dir" ]; then
    cd "$current_dir" 2>/dev/null
fi

# Color escapes (printf-interpreted).
C_RESET='\033[0m'
C_DIM='\033[90m'
C_GREEN='\033[32m'
C_RED='\033[31m'
C_YELLOW='\033[33m'
C_CYAN='\033[36m'
C_MAGENTA='\033[35m'
C_BOLD_RED='\033[1;31m'
C_BOLD_GREEN='\033[1;32m'

parts=""

# Workspace segment, only in multi-workspace repos. Name is resolved by
# matching `jj workspace root` against the list — the directory basename is
# not reliable (workspace ttfb lives in baseten-ttfb).
ws_root=$(jj --no-pager --ignore-working-copy workspace root 2>/dev/null)
if [ -n "$ws_root" ]; then
    ws_list=$(jj --no-pager --ignore-working-copy workspace list \
        -T 'self.name() ++ "\t" ++ self.root() ++ "\n"' 2>/dev/null)
    if [ "$(printf '%s\n' "$ws_list" | grep -c .)" -gt 1 ]; then
        ws_name=$(printf '%s\n' "$ws_list" |
            awk -F'\t' -v root="$ws_root" '$2 == root {print $1}')
        parts="${parts}${C_MAGENTA}ws:${ws_name:-?}${C_RESET} "
    fi
fi

# One jj call: id\tdesc\tbookmarks\tconflict\tempty
jj_info=$(jj --no-pager --ignore-working-copy log -r @ --no-graph \
    -T 'change_id.shortest(8) ++ "\t" ++ description.first_line() ++ "\t" ++ bookmarks.map(|b| b.name()).join(",") ++ "\t" ++ if(conflict, "1", "0") ++ "\t" ++ if(empty, "1", "0")' \
    2>/dev/null)

if [ -n "$jj_info" ]; then
    change=$(printf '%s' "$jj_info" | cut -f1)
    desc=$(printf '%s' "$jj_info" | cut -f2)
    bookmarks=$(printf '%s' "$jj_info" | cut -f3)
    is_conflict=$(printf '%s' "$jj_info" | cut -f4)
    is_empty=$(printf '%s' "$jj_info" | cut -f5)

    parts="${parts}${C_DIM}${change}${C_RESET}"

    if [ -n "$bookmarks" ]; then
        parts="${parts} ${C_CYAN}[${bookmarks}]${C_RESET}"
    fi

    if [ "$is_conflict" = "1" ]; then
        parts="${parts} ${C_BOLD_RED}⚠ conflict${C_RESET}"
    fi

    if [ -n "$desc" ]; then
        # Truncate description to 40 chars with ellipsis.
        if [ "${#desc}" -gt 40 ]; then
            desc=$(printf '%s' "$desc" | cut -c1-39)…
        fi
        parts="${parts} ${desc}"
    elif [ "$is_empty" = "1" ]; then
        parts="${parts} ${C_DIM}(empty)${C_RESET}"
    fi

    # Diff stats (skip when change is empty).
    if [ "$is_empty" != "1" ]; then
        diff_stat=$(jj --no-pager --ignore-working-copy diff --stat 2>/dev/null | tail -n1)
        ins=$(printf '%s' "$diff_stat" | grep -oE '[0-9]+ insertion' | grep -oE '[0-9]+')
        del=$(printf '%s' "$diff_stat" | grep -oE '[0-9]+ deletion' | grep -oE '[0-9]+')
        ins=${ins:-0}
        del=${del:-0}
        parts="${parts} ${C_GREEN}+${ins}${C_RED}-${del}${C_RESET}"
    fi

    # Commits ahead of master.
    ahead=$(jj --no-pager --ignore-working-copy log -r 'master..@' --no-graph -T '"x\n"' 2>/dev/null | wc -l | tr -d ' ')
    if [ -n "$ahead" ] && [ "$ahead" -gt 0 ]; then
        parts="${parts} ${C_YELLOW}↑${ahead}${C_RESET}"
    fi
fi

# Model + ctx.
tail=""
if [ -n "$model" ]; then
    tail="${tail} ${C_BOLD_GREEN}${model}${C_RESET}"
fi
if [ -n "$used" ]; then
    used_int=$(printf '%.0f' "$used")
    tail="${tail} ${C_DIM}ctx:${used_int}%%${C_RESET}"
fi

if [ -n "$tail" ]; then
    parts="${parts} ${C_DIM}|${C_RESET}${tail}"
fi

printf "${parts}\n"
