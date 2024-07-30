{
  config,
  lib,
  pkgs,
  ...
}:
let
  # These scripts are ugly, but the goal is simple:
  # * Lock and blank the screen on a timeout.
  # * Lock the screen due to xss-lock hooks suchs as suspend.
  # * While locked, the screen should blank on a short timeout.
  # It's this last condition that proved surprisingly tricky.
  timeout_mins = 10;
  timeout_secs = timeout_mins * 60;
  inner_locker = pkgs.writeShellApplication {
    name = "inner_locker";
    runtimeInputs = with pkgs; [
      i3lock
      xorg.xset
    ];
    text = # bash
      ''
        xset s on
        xset s 5
        i3lock -nc 11aaaa
        xset s ${builtins.toString timeout_secs}
      '';
  };

  locker = pkgs.writeShellApplication {
    name = "locker";
    runtimeInputs = [
      inner_locker
      pkgs.flock
      pkgs.xorg.xset
    ];
    text = # bash
      ''
        # If already locked, blank screen. Otherwise lock.
        flock -n "$HOME/.i3lock" inner_locker || xset dpms force off
      '';
  };
in
{
  options.custom.x11_lock = {
    enable = lib.mkEnableOption "X11 locker";
  };

  config = lib.mkIf config.custom.x11_lock.enable {
    home.packages = [ locker ];

    services.screen-locker = {
      enable = true;
      lockCmd = lib.getExe locker;
      inactiveInterval = timeout_mins;
      xautolock.enable = false;
      xss-lock.extraOptions = [
        "--transfer-sleep-lock"
        "--verbose"
      ];
    };
  };
}
