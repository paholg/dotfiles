#!/usr/bin/env bash
set -euo pipefail

xrandr --output HDMI-1 --off \
       --output DP-1 --primary --mode 3840x2160 --rate 144 --pos 0x0 --rotate left \
       --output DP-2 --mode 3840x2160 --rate 60 --pos 2160x0 --rotate right \
       --output DP-3 --off
