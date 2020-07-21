#!/bin/sh

source ~/.profile &
source ~/.xprofile &

# Load resources
xrdb -merge .Xresources &

exec xmonad
