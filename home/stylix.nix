{ lib, ... }:
{
  programs.alacritty.settings.colors.primary.background = lib.mkForce "0x000000";

  stylix.targets = {
    helix.enable = false;
  };
  # stylix.targets.firefox.profileNames = [ "default" ];
}
