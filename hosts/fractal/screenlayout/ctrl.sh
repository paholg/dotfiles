#!/usr/bin/env bash
set -euo pipefail

xrandr --output DP-1 --off \
       --output DP-2 --off \
       --output DP-3 --off \
       --output HDMI-1 --primary --mode 3840x2160 --rate 120
