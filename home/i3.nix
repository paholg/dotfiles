{
  config,
  lib,
  pkgs,
  ...
}:
let
  mod = "Mod4";
  term = lib.getExe pkgs.alacritty;
in
{
  options.custom.i3 = {
    enable = lib.mkEnableOption "i3";
    startup = lib.mkOption {
      type = lib.types.listOf lib.types.attrs;
      default = [ ];
    };
    extraConfig = lib.mkOption {
      type = lib.types.lines;
      default = "";
    };
  };

  config = lib.mkIf config.custom.i3.enable {
    custom.wayland = false;
    custom.x11 = true;
    custom.x11_lock.enable = true;

    xsession.enable = true;
    xsession.windowManager.i3 = {
      enable = true;
      extraConfig = config.custom.i3.extraConfig;
    };
    xsession.windowManager.i3.config = {
      modifier = mod;
      terminal = term;
      bars = [ { position = "top"; } ];
      # colors = { };
      assigns = {
        "0:`" = [ { class = "^discord$"; } ];
        "1" = [ { title = "Steam"; } ];
        "2" = [ { class = "^firefox$"; } ];
      };
      startup = config.custom.i3.startup;
      floating = {
        modifier = mod;
        titlebar = true;
        criteria = [ ];
      };
      focus = {
        followMouse = true;
        mouseWarping = true;
        newWindow = "smart";
        wrapping = "yes";
      };
      # fonts = { };
      gaps = {
        horizontal = 0;
        vertical = 0;
        inner = 4;
        outer = 0;
        bottom = 0;
        left = 0;
        right = 0;
        top = 0;
        smartBorders = "off";
        smartGaps = true;
      };
      window = {
        border = 0;
        commands = [ ];
        hideEdgeBorders = "smart";
        titlebar = true;
      };
      workspaceAutoBackAndForth = true;
      workspaceLayout = "default";

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
        # "${mod}+Shift+h" = "exec ${set_sink "KT USB Audio"}";
        # "${mod}+Shift+s" = "exec ${set_sink "Audioengine HD3"}";

        # *******************************************************************
        # Layouts
        "${mod}+space" = "fullscreen toggle";
        "${mod}+z" = "floating toggle";
        "${mod}+v" = "splitv";
        "${mod}+s" = "splith";
        "${mod}+e" = "layout toggle split";
        "${mod}+x" = "focus mode_toggle";

        # *******************************************************************
        # Monitors
        "${mod}+m" = "exec monitor_switch";
        "${mod}+Shift+m" = "exec monitor_switch shift";
        "${mod}+Contorl+m" = "exec monitor_switch ctrl";

        # *******************************************************************
        # Navigation
        "${mod}+a" = "focus parent";

        "${mod}+h" = "focus left";
        "${mod}+j" = "focus down";
        "${mod}+k" = "focus up";
        "${mod}+l" = "focus right";

        # "${mod}+Shift+a" = "move parent";
        "${mod}+Shift+h" = "move left";
        "${mod}+Shift+j" = "move down";
        "${mod}+Shift+k" = "move up";
        "${mod}+Shift+l" = "move right";

        # *******************************************************************
        # STUFF
        "${mod}+Shift+q" = "reload";
        "${mod}+Alt+q" = "exit";

        # *******************************************************************
        # Launch programs
        "${mod}+t" = "exec ${term}";
        "${mod}+r" = "splith; exec ${term}";
        "${mod}+g" = "splitv; exec ${term}";
        "${mod}+f" = "exec firefox";
        "${mod}+o" = "exec pavucontrol";
        # "${mod}+r" = "exec tofi-run | xargs swaymsg exec --";

        "${mod}+Ctrl+q" = "kill";

        "${mod}+Ctrl+l" = "exec xset dpms force off";
      };
      # menu = "";
      # modes = {};

      # keybindings = {
      #   # Work mode:
      #   "${mod}+Shift+m" = ''
      #     exec swaymsg output HDMI-A-1 disable; \
      #     exec swaymsg output DP-2 enable pos 2160 0 mode 3840x2160@60Hz transform 90 bg "#000000" solid_color; \
      #     exec swaymsg output DP-3 enable pos 0 0 mode 3840x2160@144Hz transform 270 bg "#000000" solid_color;
      #   '';
      #   # Game mode:
      #   "${mod}+Ctrl+m" = ''
      #     exec swaymsg output HDMI-A-1 disable; \
      #     exec swaymsg output DP-2 disable; \
      #     exec swaymsg output DP-3 enable pos 0 0 mode 3840x2160@144Hz transform 0 bg "#000000" solid_color;
      #   '';
      #   # Couch mode:
      #   "${mod}+m" = ''
      #     exec swaymsg output HDMI-A-1 enable pos 0 0 mode 2840x2160@60Hz bg "#000000" solid_color; \
      #     exec swaymsg output DP-2 disable;
      #     exec swaymsg output DP-3 disable;
      #     exec ${set_sink "HDA ATI HDMI"};
      #   '';

      # };
    };

    services = {
      dunst = {
        enable = true;
        settings = {
          global = {
            follow = "keyboard";
            geometry = "800x3-0+24";
            indicate_hidden = true;
            shrink = true;
            horizontal_padding = 24;
            font = "Monospace 12";
            format = "%a: %s";
            alignment = "right";
            word_wrap = true;
            dmenu = lib.getExe pkgs.dmenu;
            browser = lib.getExe pkgs.firefox;
            mouse_left_click = "do_action";
            mouse_middle_click = "close_all";
            mouse_right_click = "close_current";
            show_indicators = true;
          };
        };
      };

      picom = {
        enable = true;
        vSync = true;
      };
    };

    home.file.".xinitrc".text = # bash
      ''
        monitor_switch default &
        fixkb &
        background 150 &
        xsetroot -cursor_name left_ptr &

        # FROM https://wiki.nixos.org/wiki/Using_X_without_a_Display_Manager#Setting_up_Xorg_system-wide_but_without_a_Display_Manager
        if test -z "$DBUS_SESSION_BUS_ADDRESS"; then
        	eval $(dbus-launch --exit-with-session --sh-syntax)
        fi
        systemctl --user import-environment DISPLAY XAUTHORITY

        if command -v dbus-update-activation-environment >/dev/null 2>&1; then
          dbus-update-activation-environment DISPLAY XAUTHORITY
        fi

        exec $HOME/.xsession
      '';
  };

}
