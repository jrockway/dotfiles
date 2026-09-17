#!/bin/sh
# Claude Code statusLine — jj-centric.
# Format: ws:<name> [cwd:<dir>] <id> [bookmark] <desc...> +N-M ↑K [⚠ conflict|(empty)] | model ctx:X%
input=$(cat)
model=$(echo "$input" | jq -r '.model.display_name // empty')
used=$(echo "$input" | jq -r '.context_window.used_percentage // empty')
effort=$(echo "$input" | jq -r '.effort.level // empty')
fast=$(echo "$input" | jq -r '.fast_mode // false')
session_id=$(echo "$input" | jq -r '.session_id // empty')
current_dir=$(echo "$input" | jq -r '.workspace.current_dir // .cwd // empty')

# /workspace pins the session's declared jj workspace (root path, one line, in
# the pin file — see commands/workspace.md). The shell cwd drifts (gh runs
# from the main checkout), so jj info renders from the pin when one exists,
# with a cwd marker when the shell is elsewhere.
pin_root=""
if [ -n "$session_id" ] && [ -f "$HOME/.claude/jj-workspace-pins/$session_id" ]; then
    pin_root=$(head -n1 "$HOME/.claude/jj-workspace-pins/$session_id")
    [ -d "$pin_root" ] || pin_root=""
fi

# Run jj from the pinned root, else the session's live cwd — sessions started
# in the default workspace often operate on a different jj workspace entirely.
dir="${pin_root:-$current_dir}"
if [ -n "$dir" ] && [ -d "$dir" ]; then
    cd "$dir" 2>/dev/null
fi

# Color escapes (printf %b-interpreted). Fall 2026 palette, 256-color, picked
# for the dark theme: plum for the workspace, pumpkin for anything that wants a
# glance (model, cwd drift), gold for bookmarks, moss/maple for the diff, and
# bold maple for the two states that need a fix now.
C_RESET='\033[0m'
C_DIM='\033[38;5;245m'
C_PLUM='\033[38;5;139m'
C_PUMPKIN='\033[38;5;208m'
C_AMBER='\033[38;5;214m'
C_GOLD='\033[38;5;178m'
C_MOSS='\033[38;5;107m'
C_MAPLE='\033[38;5;167m'
C_BOLD_MAPLE='\033[1;38;5;160m'
C_BOLD_PUMPKIN='\033[1;38;5;208m'

parts="${C_DIM}🍂${C_RESET} "

ws_root=$(jj --no-pager --ignore-working-copy workspace root 2>/dev/null)

# Every jj read below uses --ignore-working-copy, so plain file edits don't
# show up (+N-M, description, (empty)) until something snapshots. Refresh via
# a rate-limited background snapshot — the next render picks it up. Snapshot
# stderr is kept: a working copy rewritten from another workspace can't
# snapshot until `jj workspace update-stale`, which deserves a marker rather
# than a silently frozen statusline.
if [ -n "$ws_root" ]; then
    snap_dir="$HOME/.claude/cache"
    snap_key=$(printf '%s' "$ws_root" | cksum | cut -d' ' -f1)
    snap_marker="$snap_dir/jj-snapshot-$snap_key"
    snap_err="$snap_dir/jj-snapshot-$snap_key.err"
    mkdir -p "$snap_dir"
    now=$(date +%s)
    last=$(stat -c %Y "$snap_marker" 2>/dev/null || echo 0)
    if [ $((now - last)) -ge 15 ]; then
        touch "$snap_marker"
        (jj --no-pager util snapshot >/dev/null 2>"$snap_err.tmp"; mv -f "$snap_err.tmp" "$snap_err") &
    fi
    if [ -s "$snap_err" ] && grep -qi 'stale' "$snap_err" 2>/dev/null; then
        parts="${parts}${C_BOLD_MAPLE}⚠ needs update-stale${C_RESET} "
    fi
fi

# Workspace segment, only in multi-workspace repos. Name is resolved by
# matching `jj workspace root` against the list — the directory basename is
# not reliable (workspace ttfb lives in baseten-ttfb).
if [ -n "$ws_root" ]; then
    ws_list=$(jj --no-pager --ignore-working-copy workspace list \
        -T 'self.name() ++ "\t" ++ self.root() ++ "\n"' 2>/dev/null)
    if [ "$(printf '%s\n' "$ws_list" | grep -c .)" -gt 1 ]; then
        ws_name=$(printf '%s\n' "$ws_list" |
            awk -F'\t' -v root="$ws_root" '$2 == root {print $1}')
        parts="${parts}${C_PLUM}ws:${ws_name:-?}${C_RESET} "
    fi
fi

# Drift marker: pinned, but the shell cwd is off the pinned root.
if [ -n "$pin_root" ] && [ -n "$current_dir" ]; then
    case "$current_dir/" in
    "$pin_root"/*) ;;
    *) parts="${parts}${C_PUMPKIN}cwd:$(basename "$current_dir")${C_RESET} " ;;
    esac
fi

# One jj call: id\tdesc\tbookmarks\tconflict\tempty
jj_info=$(jj --no-pager --ignore-working-copy log -r @ --no-graph \
    -T 'change_id.shortest(8) ++ "\t" ++ description.first_line() ++ "\t" ++ bookmarks.map(|b| b.name()).join(",") ++ "\t" ++ if(conflict, "1", "0") ++ "\t" ++ if(empty, "1", "0")' \
    2>/dev/null)

if [ -n "$jj_info" ]; then
    change=$(printf '%s' "$jj_info" | cut -f1)
    # Strip backslashes: the trailing printf renders with %b, which would
    # otherwise interpret them as escapes.
    desc=$(printf '%s' "$jj_info" | cut -f2 | tr -d '\\')
    bookmarks=$(printf '%s' "$jj_info" | cut -f3)
    is_conflict=$(printf '%s' "$jj_info" | cut -f4)
    is_empty=$(printf '%s' "$jj_info" | cut -f5)

    parts="${parts}${C_DIM}${change}${C_RESET}"

    if [ -n "$bookmarks" ]; then
        parts="${parts} ${C_GOLD}[${bookmarks}]${C_RESET}"
    fi

    if [ "$is_conflict" = "1" ]; then
        parts="${parts} ${C_BOLD_MAPLE}⚠ conflict${C_RESET}"
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
        parts="${parts} ${C_MOSS}+${ins}${C_MAPLE}-${del}${C_RESET}"
    fi

    # Commits ahead of master.
    ahead=$(jj --no-pager --ignore-working-copy log -r 'master..@' --no-graph -T '"x\n"' 2>/dev/null | wc -l | tr -d ' ')
    if [ -n "$ahead" ] && [ "$ahead" -gt 0 ]; then
        parts="${parts} ${C_AMBER}↑${ahead}${C_RESET}"
    fi
fi

# Model + ctx.
tail=""
if [ -n "$model" ]; then
    tail="${tail} ${C_BOLD_PUMPKIN}${model}${C_RESET}"
    # Effort as a dim suffix, and a bolt when fast mode is on: both change what a
    # turn costs and neither is visible anywhere else on screen.
    [ -n "$effort" ] && tail="${tail}${C_DIM}·${effort}${C_RESET}"
    [ "$fast" = "true" ] && tail="${tail} ${C_AMBER}⚡${C_RESET}"
fi
if [ -n "$used" ]; then
    used_int=$(printf '%.0f' "$used")
    # The leaves turn as the context window fills: moss, then amber, then maple.
    if [ "$used_int" -ge 90 ]; then ctx_color="$C_MAPLE"
    elif [ "$used_int" -ge 70 ]; then ctx_color="$C_AMBER"
    else ctx_color="$C_DIM"; fi
    tail="${tail} ${ctx_color}ctx:${used_int}%${C_RESET}"
fi

if [ -n "$tail" ]; then
    parts="${parts} ${C_DIM}|${C_RESET}${tail}"
fi

# %b interprets the color escapes but not stray % in descriptions (a bare
# printf "$parts" would treat the whole line as a format string).
printf '%b\n' "$parts"
