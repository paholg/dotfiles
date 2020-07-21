#!/usr/bin/env bash

source ~/.profile &

monitor_switch default &

fixkb &

# Set cursor
xsetroot -cursor_name left_ptr

# startup programs
background 150 &

# Load resources
xrdb -merge .Xresources &

if [ -z "$(pidof -s redshift)" ]; then
    redshift -t 5000K:3300K &> /dev/null &
fi

exec xmonad
