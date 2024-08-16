#!/bin/sh

xrandr --output eDP-1 --mode 2256x1504 --pos 0x0 --rotate normal \
       --output DP-1 --off \
       --output DP-2 --off \
       --output DP-3 --primary --mode 3840x2160 --pos 2256x0 \
       --output DP-4 --off \
       --output DP-5 --off \
       --output DP-6 --off \
       --output DP-7 --off \
       --output DP-8 --off
