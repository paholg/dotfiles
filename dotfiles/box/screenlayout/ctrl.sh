#!/bin/sh
set -ex

xrandr --output DVI-I-0 --off --output DVI-I-1 --mode 1680x1050 --pos 0x0 --rotate normal  --panning 0x0+0+0 --output HDMI-0 --off --output DP-0 --off --output DP-1 --off --output DVI-D-0 --off
