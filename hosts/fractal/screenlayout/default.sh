#!/usr/bin/env bash
set -euo pipefail

xrandr --output DP-1 --primary --mode 3840x2160 --pos 0x0 --rate 120 \
       --output DP-2 --off
