{
  config,
  lib,
  pkgs,
  ...
}:
let

  settings = {
    # XSECURELOCK_BACKGROUND_COLOR = "";
    # XSECURELOCK_AUTH_BACKGROUND_COLOR = "";
    # XSECURELOCK_AUTH_FOREGROUND_COLOR = "";
    # XSECURELOCK_AUTH_WARNING_COLOR = "";
    XSECURELOCK_AUTH_CURSOR_BLINK = "0";
    XSECURELOCK_AUTH_TIMEOUT = "10"; # seconds

    XSECURELOCK_BLANK_TIMEOUT = "10"; # seconds
    XSECURELOCK_BLANK_DPMS_STATE = "off";

    # TODO: Can use to switch to guest while locked!
    # XSECURELOCK_KEY_[X11 kysym]_COMMAND = "";

    XSECURELOCK_PASSWORD_PROMPT = "cursor";
    XSECURELOCK_SHOW_DATETIME = "1";
    XSECURELOCK_SHOW_KEYBOARD_LAYOUT = "0";

    # Without this, can't lock when running wayland in a different VTT.
    XSECURELOCK_DEBUG_ALLOW_LOCKING_IF_INEFFECTIVE = "1";
    # XSECURELOCK_SHOW_HOSTNAME = "0";
    # XSECURELOCK_SHOW_USERNAME = "0";

  } // config.custom.x11_lock.settings;

  vars = lib.strings.concatStrings (
    builtins.map (setting: "export ${setting.name}=${setting.value}\n") (lib.attrsToList settings)
  );

  locker = pkgs.writeShellApplication {
    name = "locker";
    runtimeInputs = with pkgs; [ xsecurelock ];
    text = # bash
      ''
        ${vars}
        exec xsecurelock
      '';
  };
in
{
  options.custom.x11_lock = {
    enable = lib.mkEnableOption "X11 locker";
    settings = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
    };
  };

  config = lib.mkIf config.custom.x11_lock.enable {
    services.screen-locker = {
      enable = true;
      lockCmd = lib.getExe locker;
      inactiveInterval = 10; # minutes
      xautolock.enable = false;
      xss-lock.extraOptions = [
        "--transfer-sleep-lock"
        "--verbose"
      ];
    };
  };
}
