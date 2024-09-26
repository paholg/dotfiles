{
  config,
  lib,
  pkgs,
  ...
}:
let
  mod = "Mod4";
  term = lib.getExe pkgs.alacritty;
  rofi = lib.getExe config.programs.rofi.finalPackage;
  pamixer = lib.getExe pkgs.pamixer;

  zoom_sink_titles = [
    "Zoom - Free Account"
    "Zoom - Licensed Account"
    "Zoom Meeting"
  ];
  zoom_commands =
    [
      {
        command = "floating enable";
        criteria = {
          class = "zoom";
        };
      }
    ]
    ++ lib.map (title: {
      command = "floating disable";
      criteria = {
        class = "zoom";
        title = title;
      };
    }) zoom_sink_titles;
in
{
  options.custom.i3 = {
    enable = lib.mkEnableOption "i3";
    customConfig = lib.mkOption {
      type = lib.types.attrs;
      default = { };
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

    programs.rofi = {
      enable = true;
      terminal = term;
    };

    xsession.enable = true;
    xsession.windowManager.i3 = {
      enable = true;
      extraConfig = config.custom.i3.extraConfig;
    };
    xsession.windowManager.i3.config = {
      modifier = mod;
      terminal = term;
      bars = [
        {
          position = "top";
          statusCommand = lib.getExe' pkgs.i3status-rust "i3status-rs";
          trayOutput = "primary";
        }
      ];
      assigns = {
        "0:`" = [
          { class = "^discord$"; }
          { class = "^slack$"; }
        ];
        "1" = [ { title = "Steam"; } ];
      };
      floating = {
        modifier = mod;
        titlebar = true;
        criteria = [
          { title = "Bluetooth Devices"; }
          { class = "pavucontrol"; }
        ];
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
        commands = [ ] ++ zoom_commands;
        hideEdgeBorders = "smart";
        titlebar = true;
      };
      workspaceAutoBackAndForth = false;
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
        # Layouts
        "${mod}+space" = "fullscreen toggle";
        "${mod}+z" = "floating toggle";
        "${mod}+v" = "splith";
        "${mod}+s" = "splitv";
        "${mod}+e" = "layout toggle split";
        "${mod}+x" = "focus mode_toggle";

        # *******************************************************************
        # Monitors
        "${mod}+m" = "exec monitor_switch default";
        "${mod}+Shift+m" = "exec monitor_switch shift";
        "${mod}+Contorl+m" = "exec monitor_switch ctrl";

        # *******************************************************************
        # Navigation
        "${mod}+a" = "focus parent";
        "${mod}+d" = "focus child";

        "${mod}+h" = "focus left";
        "${mod}+j" = "focus down";
        "${mod}+k" = "focus up";
        "${mod}+l" = "focus right";

        "${mod}+Shift+h" = "move left";
        "${mod}+Shift+j" = "move down";
        "${mod}+Shift+k" = "move up";
        "${mod}+Shift+l" = "move right";

        "${mod}+Control+h" = "resize shrink width 5 px or 5 ppt";
        "${mod}+Control+j" = "resize shrink height 5 px or 5 ppt";
        "${mod}+Control+k" = "resize grow height 5 px or 5 ppt";
        "${mod}+Control+l" = "resize grow width 5 px or 5 ppt";

        # *******************************************************************
        # Audio modes
        "${mod}+Shift+a" = "exec set_sink 'KT USB Audio'";
        "${mod}+Shift+s" = "exec set_sink 'HDA ATI HDMI'";

        # *******************************************************************
        # Media
        "XF86AudioMute" = "exec ${pamixer} -t";
        "${mod}+Left" = "exec ${pamixer} -t";
        "XF86AudioMicMute" = "exec pactl set-source-mute @DEFAULT_SOURCE@ toggle";
        "${mod}+Right" = "exec pactl set-source-mute @DEFAULT_SOURCE@ toggle";
        "XF86AudioLowerVolume" = "exec ${pamixer} -d5";
        "XF86AudioRaiseVolume" = "exec ${pamixer} -i5";
        "${mod}+Down" = "exec ${pamixer} -d5";
        "${mod}+Up" = "exec ${pamixer} -i5";
        "XF86MonBrightnessUp" = "exec brightnessctl s +10%";
        "XF86MonBrightnessDown" = "exec brightnessctl s 10%-";

        # *******************************************************************
        # STUFF
        "${mod}+Shift+q" = "reload";
        "${mod}+Alt+q" = ''exec "i3-nagbar -t warning -m 'Really exit?' -B 'Yes' 'i3-msg exit'"'';

        # *******************************************************************
        # Launch programs
        "${mod}+t" = "exec ${term}";
        "${mod}+f" = "exec firefox";
        "${mod}+o" = "exec pavucontrol";
        "${mod}+r" = "exec ${rofi} -show run";
        "${mod}+w" = "exec ${rofi} -show window";

        "${mod}+Ctrl+q" = "kill";

        "${mod}+Ctrl+n" = "exec xset dpms force off";
      };
    } // config.custom.i3.customConfig;

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
        background 60 &
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
