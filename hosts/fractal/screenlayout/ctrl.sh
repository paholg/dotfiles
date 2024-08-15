#!/usr/bin/env bash
set -euo pipefail

xrandr --output DP-1 --off \
       --output DP-2 --primary --mode 3840x2160 --pos 0x0
