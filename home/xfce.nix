{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.custom;
in
{
  options.custom.xfce = {
    enable = lib.mkEnableOption "Xfce";
  };

  config = lib.mkIf cfg.xfce.enable {
    custom.wayland = false;
    custom.x11 = true;
    # TODO: Probably don't need this stuff here.
    home.file.".xinitrc".text = # bash
      ''
        monitor_switch ctrl &
        fixkb &
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

        # . $HOME/.xsession

        exec startxfce4
      '';

    home.packages = with pkgs; [
      xorg.xauth
      xterm
    ];
  };
}
