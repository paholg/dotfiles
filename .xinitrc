#!/bin/sh

source ~/.profile &
source ~/.xprofile &

# Load resources
xrdb -merge .Xresources &

# Set cursor
xsetroot -cursor_name left_ptr

exec xmonad
