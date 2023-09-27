#!/bin/sh
xrandr --output eDP --primary --mode 1920x1080 --pos 0x0 --rotate normal \
       --output HDMI-A-0 --mode 3840x2160 --pos 4080x0 --rotate right \
       --output DisplayPort-0 --mode 3840x2160 --pos 1920x0 --rotate left \
       --output DisplayPort-1 --off
