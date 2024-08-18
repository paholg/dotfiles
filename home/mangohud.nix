{ config, lib, ... }:
{
  options.custom.mangohud = {
    enable = lib.mkEnableOption "Mangohud";
  };

  config = lib.mkIf config.custom.mangohud.enable {
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
      # MANGOHUD_CONFIG = "no_display";
    };
  };
}
