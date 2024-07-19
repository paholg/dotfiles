{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.custom;
  mod = "Mod4";
  term = lib.getExe pkgs.alacritty;

  # Lock the session, running swayidle on a fast loop to turn off displays while
  # locked.
  lock_script = pkgs.writeScriptBin "lock" ''
    #!/usr/bin/env bash
    set -euo pipefail

    ${lib.getExe pkgs.swayidle} timeout 10 'swaymsg "output * power off"' resume 'swaymsg "output * power on"' &
    SWAY_IDLE=$!
    ${lib.getExe pkgs.swaylock} -c 008888 -F
    kill $SWAY_IDLE
  '';
  lock = lib.getExe lock_script;

  set_sink = name: "$HOME/dotfiles/bin/list_sinks | jq '.\"${name}\"' | xargs wpctl set-default";
in
{
  options.custom.sway = {
    enable = lib.mkEnableOption "Sway";
    startup = lib.mkOption {
      type = lib.types.listOf lib.types.attrs;
      default = [ ];
    };
    extraConfig = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
    };
  };

  config = lib.mkIf cfg.sway.enable {
    custom.wayland = true;
    custom.x11 = false;
    wayland.windowManager.sway = {
      enable = true;
      config = {
        modifier = mod;
        terminal = term;
        # fonts = {};
        bars = [ { position = "top"; } ];
        assigns = {
          "0:`" = [ { class = "^discord$"; } ];
          "1" = [ { title = "Steam Big Picture Mode"; } ];
          "2" = [ { app_id = "^firefox$"; } ];
        };
        startup = cfg.sway.startup;
        floating = { };
        keybindings = {
          # *******************************************************************
          # Workspaces
          "${mod}+grave" = "[workspace=`] move workspace to output current; workspace 0:`";
          "${mod}+1" = "[workspace=1] move workspace to output current; workspace 1";
          "${mod}+2" = "[workspace=2] move workspace to output current; workspace 2";
          "${mod}+3" = "[workspace=3] move workspace to output current; workspace 3";
          "${mod}+4" = "[workspace=4] move workspace to output current; workspace 4";
          "${mod}+5" = "[workspace=5] move workspace to output current; workspace 5";
          "${mod}+6" = "[workspace=6] move workspace to output current; workspace 6";
          "${mod}+7" = "[workspace=7] move workspace to output current; workspace 7";
          "${mod}+8" = "[workspace=8] move workspace to output current; workspace 8";
          "${mod}+9" = "[workspace=9] move workspace to output current; workspace 9";
          "${mod}+0" = "[workspace=0] move workspace to output current; workspace 10:0";

          "${mod}+Shift+grave" = "move container to workspace 0:`";
          "${mod}+Shift+1" = "move container to workspace 1";
          "${mod}+Shift+2" = "move container to workspace 2";
          "${mod}+Shift+3" = "move container to workspace 3";
          "${mod}+Shift+4" = "move container to workspace 4";
          "${mod}+Shift+5" = "move container to workspace 5";
          "${mod}+Shift+6" = "move container to workspace 6";
          "${mod}+Shift+7" = "move container to workspace 7";
          "${mod}+Shift+8" = "move container to workspace 8";
          "${mod}+Shift+9" = "move container to workspace 9";
          "${mod}+Shift+0" = "move container to workspace 10:0";

          # *******************************************************************
          # Change setup
          "${mod}+Shift+h" = "exec ${set_sink "KT USB Audio"}";
          "${mod}+Shift+s" = "exec ${set_sink "Audioengine HD3"}";
          # Work mode:
          "${mod}+Shift+m" = ''
            exec swaymsg output HDMI-A-1 disable; \
            exec swaymsg output DP-2 enable pos 2160 0 mode 3840x2160@60Hz transform 90 bg "#000000" solid_color; \
            exec swaymsg output DP-3 enable pos 0 0 mode 3840x2160@144Hz transform 270 bg "#000000" solid_color;
          '';
          # Game mode:
          "${mod}+Ctrl+m" = ''
            exec swaymsg output HDMI-A-1 disable; \
            exec swaymsg output DP-2 disable; \
            exec swaymsg output DP-3 enable pos 0 0 mode 3840x2160@144Hz transform 0 bg "#000000" solid_color;
          '';
          # Couch mode:
          "${mod}+m" = ''
            exec swaymsg output HDMI-A-1 enable pos 0 0 mode 2840x2160@60Hz bg "#000000" solid_color; \
            exec swaymsg output DP-2 disable;
            exec swaymsg output DP-3 disable;
            exec ${set_sink "HDA ATI HDMI"};
          '';

          # *******************************************************************
          # Layouts
          "${mod}+space" = "fullscreen toggle";
          "${mod}+z" = "floating toggle";
          "${mod}+v" = "splitv";
          "${mod}+s" = "splith";
          "${mod}+e" = "layout toggle split";
          "${mod}+x" = "focus mode_toggle";

          # *******************************************************************
          # Launch programs
          "${mod}+t" = "exec ${term}";
          "${mod}+f" = "exec firefox";
          "${mod}+o" = "exec pavucontrol";
          "${mod}+r" = "exec tofi-run | xargs swaymsg exec --";

          "${mod}+Ctrl+q" = "kill";
          "${mod}+Alt+q" = "exec swaymsg exit";

          "${mod}+n" = "exec ${lock}";
        };
      };
      extraConfig =
        ''
          input "type:keyboard" {
            repeat_delay 200
            repeat_rate 50
          }
        ''
        + builtins.concatStringsSep "" (map (item: "${item}\n") cfg.sway.extraConfig);
      swaynag.enable = true;
    };

    programs.mangohud = {
      enable = true;
      enableSessionWide = true;
      settings = {
        toggle_hud = "Super_L+slash";

        fps_limit = 0;
      };
    };

    home.sessionVariables = {
      # Hide mangohud by default
      MANGOHUD_CONFIG = "no_display";
    };

    home.packages = with pkgs; [
      tofi # App launcher
      wl-clipboard
      wlr-randr
    ];

    # Idle management
    services.swayidle = {
      enable = lib.mkDefault true;
      events = [
        {
          event = "before-sleep";
          command = lock;
        }
        {
          event = "lock";
          command = lock;
        }
      ];
      timeouts = [
        {
          timeout = 285;
          command = ''${lib.getExe pkgs.libnotify} -t 15000 -- "LOCKING in 15"'';
        }
        {
          timeout = 300;
          command = lock;
        }
      ];
    };

    # Notification daemon.
    services.mako = {
      enable = true;
    };
  };
}
