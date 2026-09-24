#!/usr/bin/env bash
set -euo pipefail

input=$(cat)
model=$(echo "$input" | jq -r '.model.display_name // empty')
effort=$(echo "$input" | jq -r '.effort.level // empty')
used=$(echo "$input" | jq -r 'if .context_window.total_input_tokens then ((.context_window.total_input_tokens / 1000) | round | tostring) + "k" else empty end')
session=$(echo "$input" | jq -r '.rate_limits.five_hour.used_percentage // empty | round')
session_reset=$(echo "$input" | jq -r '.rate_limits.five_hour.resets_at // empty | strflocaltime("%H:%M")')
week=$(echo "$input" | jq -r '.rate_limits.seven_day.used_percentage // empty | round')

# Colors a percentage on a green -> yellow -> red gradient, brightening from
# 35% at 0 to full at 100.
color_pct() {
  local p=$1 c r g
  c=$(( p > 100 ? 100 : p ))
  if (( c < 50 )); then
    r=$(( c * 255 / 50 )); g=255
  else
    r=255; g=$(( (100 - c) * 255 / 50 ))
  fi
  local bright=$(( 35 + c * 65 / 100 ))
  r=$(( r * bright / 100 )); g=$(( g * bright / 100 ))
  printf '\e[38;2;%d;%d;0m%d%%\e[0m' "$r" "$g" "$p"
}

parts=()

if [ -n "${WORKSPACE_DIR:-}" ]; then
  parts+=("ws:$WORKSPACE_DIR")
fi

if [ -n "$model" ]; then
  parts+=("$model")
fi

if [ -n "$effort" ]; then
  parts+=("effort: ${effort}")
fi

if [ -n "$used" ]; then
  parts+=("ctx: ${used}")
fi

if [ -n "$session" ]; then
  part="5h: $(color_pct "$session")"
  if [ -n "$session_reset" ]; then
    part+=" reset: ${session_reset}"
  fi
  parts+=("$part")
fi

if [ -n "$week" ]; then
  parts+=("wk: $(color_pct "$week")")
fi

printf '%s' "$(IFS='   '; echo "${parts[*]}")"
