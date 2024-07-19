{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.custom;
  # Simple script that calls i3lock, first running a background task to put the
  # screen to sleep every few seconds.
  lock_script = pkgs.writeShellApplication {
    name = "lock";
    runtimeInputs = with pkgs; [
      xidlehook
      i3lock
      xorg.xset
    ];
    text = # bash
      ''
        xidlehook --timer 5 "xset dpms force off" ""&
        IDLE_PID=$!
        i3lock -c 11aaaa
        kill $IDLE_PID
      '';
  };
  lock = lib.getExe lock_script;
in
{
  options.custom.xmonad = {
    enable = lib.mkEnableOption "Xmonad";
  };

  config = lib.mkIf cfg.xmonad.enable {
    custom.wayland = false;
    custom.x11 = true;

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
            dmenu = "/usr/bin/env dmenu";
            browser = "/usr/bin/env firefox";
            mouse_left_click = "do_action";
            mouse_middle_click = "close_all";
            mouse_right_click = "close_current";
            show_indicators = true;
          };
        };
      };
    };

    home.file.".xinitrc".text = # bash
      ''
        monitor_switch default &
        fixkb &
        background 150 &
        xrdb -merge .Xresources &
        xsetroot -cursor_name left_ptr &

        # FROM https://wiki.nixos.org/wiki/Using_X_without_a_Display_Manager#Setting_up_Xorg_system-wide_but_without_a_Display_Manager
        if test -z "$DBUS_SESSION_BUS_ADDRESS"; then
        	eval $(dbus-launch --exit-with-session --sh-syntax)
        fi
        systemctl --user import-environment DISPLAY XAUTHORITY

        if command -v dbus-update-activation-environment >/dev/null 2>&1; then
          dbus-update-activation-environment DISPLAY XAUTHORITY
        fi

        exec xmonad
      '';

    home.packages = [ lock_script ];

    services.picom = {
      enable = true;
    };

    services.xidlehook = {
      enable = true;
      timers = [
        {
          delay = 285;
          command = ''${lib.getExe pkgs.libnotify} -t 15000 -- "LOCKING in 15"'';
        }
        {
          delay = 300;
          command = lock;
        }
      ];
    };

    xsession = {
      enable = true;

      windowManager = {
        xmonad = {
          enable = true;
          config = ./xmonad.hs;
          # enableContribAndExtras = true; TODO: Switch back to this when xmoand-contrib 18.1 releases.
          extraPackages = haskellPackages: [ haskellPackages.xmonad-contrib ];
        };
      };
    };
  };
}
