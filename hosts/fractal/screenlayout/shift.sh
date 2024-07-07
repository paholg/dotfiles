#!/usr/bin/env bash
set -euo pipefail

xrandr --output HDMI-1 --off \
       --output DP-1 --primary --mode 3840x2160 --pos 0x0 --rotate normal \
       --output DP-2 --off \
       --output DP-3 --off
