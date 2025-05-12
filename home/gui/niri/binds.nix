{ lib, pkgs, ... }:
let
  nkillPkg = pkgs.writeShellApplication {
    name = "nkill";
    runtimeInputs = with pkgs; [
      niri
      ripgrep
    ];
    text = # bash
      ''
        kill "$@" "$(niri msg focused-window | rg --trim -r '$1' "PID: (\d+)")"
      '';
  };
  nkill = lib.getExe nkillPkg;
in
{
  programs.niri.settings.binds = {
    # Navigation
    "Super+C".action.center-column = { };

    "Super+H".action.focus-column-left = { };
    "Super+J".action.focus-window-down = { };
    "Super+K".action.focus-window-up = { };
    "Super+L".action.focus-column-right = { };

    "Super+Shift+H".action.focus-monitor-left = { };
    "Super+Shift+J".action.focus-monitor-down = { };
    "Super+Shift+K".action.focus-monitor-up = { };
    "Super+Shift+L".action.focus-monitor-right = { };

    "Super+I".action.focus-workspace-up = { };
    "Super+U".action.focus-workspace-down = { };

    "Super+Ctrl+Z".action.switch-focus-between-floating-and-tiling = { };

    # Window Movement
    "Super+Ctrl+H".action.move-column-left = { };
    "Super+Ctrl+J".action.move-window-down = { };
    "Super+Ctrl+K".action.move-window-up = { };
    "Super+Ctrl+L".action.move-column-right = { };

    "Super+Shift+Ctrl+H".action.move-workspace-to-monitor-left = { };
    "Super+Shift+Ctrl+J".action.move-workspace-to-monitor-down = { };
    "Super+Shift+Ctrl+K".action.move-workspace-to-monitor-up = { };
    "Super+Shift+Ctrl+L".action.move-workspace-to-monitor-right = { };

    "Super+Ctrl+I".action.move-column-to-workspace-up = { };
    "Super+Ctrl+U".action.move-column-to-workspace-down = { };

    "Super+BracketLeft".action.consume-or-expel-window-left = { };
    "Super+BracketRight".action.consume-or-expel-window-right = { };

    "Super+Comma".action.consume-window-into-column = { };
    "Super+Period".action.expel-window-from-column = { };

    # Resize
    "Super+Return".action.expand-column-to-available-width = { };
    "Super+Space".action.reset-window-height = { };

    "Super+Q".action.set-column-width = "25%";
    "Super+W".action.set-column-width = "50%";
    "Super+E".action.set-column-width = "75%";
    "Super+R".action.set-column-width = "100%";

    "Super+F".action.fullscreen-window = { };

    "Super+Ctrl+E".action.set-window-height = "+10%";
    "Super+Ctrl+S".action.set-column-width = "-10%";
    "Super+Ctrl+D".action.set-window-height = "-10%";
    "Super+Ctrl+F".action.set-column-width = "+10%";

    # Misc
    "Super+Ctrl+Q".action.close-window = { };
    "Super+Alt+Q".action.quit = { };
    "Super+Shift+Slash".action.show-hotkey-overlay = { };

    "Super+Z".action.toggle-window-floating = { };

    "Print".action.screenshot = { };
    "Alt+Print".action.screenshot-window = { };
    "Ctrl+Print".action.screenshot-screen = { };

    # Spawn
    "Super+G".action.spawn = "fuzzel";
    "Super+T".action.spawn = "kitty";
    "Super+O".action.spawn = "pavucontrol";
    "Super+B".action.spawn = "firefox";
    "Super+Ctrl+N".action.spawn = "locker";
    "Super+Ctrl+B".action.spawn = [
      (lib.getExe pkgs.imv)
      "-f"
    ];
    "Super+Ctrl+R".action.spawn = [
      "/usr/bin/env"
      "bash"
      "-c" # bash
      ''notify-send "$(niri msg focused-window)"''
    ];
    "Super+Ctrl+X".action.spawn = [ nkill ];
    "Super+Ctrl+Alt+X".action.spawn = [
      nkill
      "-9"
    ];

    # System settings
    XF86MonBrightnessUp.action.spawn = [
      "brightnessctl"
      "s"
      "+10%"
    ];
    XF86MonBrightnessDown.action.spawn = [
      "brightnessctl"
      "s"
      "10%-"
    ];
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
  };
}
