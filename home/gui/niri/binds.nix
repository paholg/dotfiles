{
  "Alt+Print".action.screenshot-window = { };
  "Ctrl+Print".action.screenshot-screen = { };
  "Print".action.screenshot = { };
  "Super+Alt+Q".action.quit = { };
  "Super+F".action.spawn = "firefox";
  "Super+O".action.spawn = "pavucontrol";
  "Super+BracketLeft".action.consume-or-expel-window-left = { };
  "Super+BracketRight".action.consume-or-expel-window-right = { };
  "Super+C".action.center-column = { };
  "Super+Comma".action.consume-window-into-column = { };
  "Super+Ctrl+H".action.move-column-left = { };
  "Super+Ctrl+I".action.move-column-to-workspace-up = { };
  "Super+Ctrl+J".action.move-window-down = { };
  "Super+Ctrl+K".action.move-window-up = { };
  "Super+Ctrl+L".action.move-column-right = { };
  "Super+Ctrl+N".action.spawn = "locker";
  "Super+Ctrl+Q".action.close-window = { };
  "Super+Ctrl+U".action.move-column-to-workspace-down = { };
  "Super+Equal".action.set-column-width = "+10%";
  "Super+Minus".action.set-column-width = "-10%";
  "Super+Space".action.maximize-column = { };
  "Super+H".action.focus-column-left = { };
  "Super+I".action.focus-workspace-up = { };
  "Super+J".action.focus-window-down = { };
  "Super+K".action.focus-window-up = { };
  "Super+L".action.focus-column-right = { };
  "Super+Period".action.expel-window-from-column = { };
  "Super+R".action.spawn = "fuzzel";
  "Super+Ctrl+R".action.spawn = [
    "/usr/bin/env"
    "bash"
    "-c"
    "notify-send \"$(niri msg focused-window)\""
  ];
  "Super+Shift+Ctrl+H".action.move-column-to-monitor-left = { };
  "Super+Shift+Ctrl+J".action.move-column-to-monitor-down = { };
  "Super+Shift+Ctrl+K".action.move-column-to-monitor-up = { };
  "Super+Shift+Ctrl+L".action.move-column-to-monitor-right = { };
  "Super+Shift+Equal".action.set-window-height = "+10%";
  "Super+Shift+F".action.fullscreen-window = { };
  "Super+Shift+H".action.focus-monitor-left = { };
  "Super+Shift+J".action.focus-monitor-down = { };
  "Super+Shift+K".action.focus-monitor-up = { };
  "Super+Shift+L".action.focus-monitor-right = { };
  "Super+Shift+Minus".action.set-window-height = "-10%";
  "Super+Shift+Slash".action.show-hotkey-overlay = { };
  "Super+Shift+W".action.reset-window-height = { };
  "Super+T".action.spawn = "alacritty";
  "Super+U".action.focus-workspace-down = { };
  "Super+W".action.switch-preset-column-width = { };
  "Super+Z".action.toggle-window-floating = { };
  "XF86AudioLowerVolume".action.spawn = [
    "wpctl"
    "set-volume"
    "@DEFAULT_AUDIO_SINK@"
    "0.1-"
  ];
  "XF86AudioMicMute".action.spawn = [
    "wpctl"
    "set-mute"
    "@DEFAULT_AUDIO_SOURCE@"
    "toggle"
  ];
  "XF86AudioMute".action.spawn = [
    "wpctl"
    "set-mute"
    "@DEFAULT_AUDIO_SINK@"
    "toggle"
  ];
  "XF86AudioRaiseVolume".action.spawn = [
    "wpctl"
    "set-volume"
    "@DEFAULT_AUDIO_SINK@"
    "0.1+"
  ];

}
