#!/bin/sh
xrandr --output eDP-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal \
       --output HDMI-1 --mode 3840x2160 --pos 4080x0 --rotate right \
       --output DP-1 --mode 3840x2160 --pos 1920x0 --rotate left \
       --output DP-2 --off
