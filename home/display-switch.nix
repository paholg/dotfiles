{
  lib,
  pkgs,
  ...
}: {
  systemd.user.services.display-switch = {
    Unit = {
      Description = "Display switch via USB switch";
    };
    Service = {
      ExecStart = lib.getExe' pkgs.display-switch "display_switch";
      Type = "simple";
      StandardOutput = "journal";
      Restart = "always";
    };
    Install.WantedBy = ["default.target"];
  };
}
