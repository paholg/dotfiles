#!/bin/sh
set -ex

xrandr \
    --output DP-0 --off \
    --output DP-1 --off \
    --output DVI-D-0 --mode 2560x1440 --pos 2160x0 --rotate normal \
    --output DVI-I-0 --off \
    --output DVI-I-1 --mode 1680x1050 --pos 2160x1440 --rotate normal \
    --output HDMI-0 --primary --mode 3840x2160 --pos 0x0 --rotate left
