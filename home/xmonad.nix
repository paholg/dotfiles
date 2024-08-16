{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.custom.xmonad = {
    enable = lib.mkEnableOption "Xmonad";
  };

  config = lib.mkIf config.custom.xmonad.enable {
    custom.wayland = false;
    custom.x11 = true;
    custom.x11_lock.enable = true;

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

    services.picom = {
      enable = true;
      vSync = true;
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
