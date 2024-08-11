{
  lib,
  pkgs,
  config,
  ...
}:
{
  options.custom.amd-graphics = lib.mkEnableOption "AMD Graphics";
  config = lib.mkIf config.custom.amd-graphics {
    hardware.graphics = {
      enable = true;

      enable32Bit = true;
      # amdvlk: open-source Vulkan driver from AMD
      extraPackages = [ pkgs.amdvlk ];
      extraPackages32 = [ pkgs.driversi686Linux.amdvlk ];
    };
  };
}
