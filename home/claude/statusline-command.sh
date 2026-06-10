#!/usr/bin/env bash
set -euo pipefail

input=$(cat)
model=$(echo "$input" | jq -r '.model.display_name // empty')
used=$(echo "$input" | jq -r '.context_window.used_percentage // empty')
session=$(echo "$input" | jq -r '.rate_limits.five_hour.used_percentage // empty | round')
session_reset=$(echo "$input" | jq -r '.rate_limits.five_hour.resets_at // empty | strflocaltime("%H:%M")')
week=$(echo "$input" | jq -r '.rate_limits.seven_day.used_percentage // empty | round')

parts=()

if [ -n "${WORKSPACE_DIR:-}" ]; then
  parts+=("ws:$WORKSPACE_DIR")
fi

if [ -n "$model" ]; then
  parts+=("$model")
fi

if [ -n "$used" ]; then
  parts+=("ctx: ${used}%")
fi

if [ -n "$session" ]; then
  part="5h: ${session}%"
  if [ -n "$session_reset" ]; then
    part+=" reset: ${session_reset}"
  fi
  parts+=("$part")
fi

if [ -n "$week" ]; then
  parts+=("wk: ${week}%")
fi

printf '%s' "$(IFS='   '; echo "${parts[*]}")"
