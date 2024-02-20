#!/usr/bin/env bash

set -euo pipefail

# Desk
hyprctl keyword monitor HDMI-A-1, disable
hyprctl keyword monitor DP-3, 3840x2160@144, 0x0, 1

# Couch
hyprctl keyword monitor DP-3, disable
hyprctl keyword monitor HDMI-A-1, 3840x2160@120, 0x0, 2

