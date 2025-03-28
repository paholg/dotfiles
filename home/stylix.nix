{ config, lib, ... }:
{
  options.custom = {
    swaylock.color = lib.mkOption {
      type = lib.types.str;
      default = "888888";
    };
  };

  config = {
    stylix.targets.helix.enable = false;
    stylix.targets.firefox.profileNames = [ "default" ];

    # Program overrides
    programs = {
      alacritty.settings.colors.primary.background = lib.mkForce "0x000000";
      swaylock.settings.color = lib.mkForce config.custom.swaylock.color;
    };

  };
}
