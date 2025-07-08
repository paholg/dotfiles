{
  lib,
  pkgs,
  ...
}:
let
  background = pkgs.writeShellApplication {
    name = "background";
    runtimeInputs = with pkgs; [
      fd
      swaybg
    ];
    text = # bash
      ''
        bg() {
          IMG=$(fd ".+\..+" ~/wallpapers/ | shuf -n1 | tee -a ~/wallpapers/history)
          swaybg -m fill -i "$IMG" &> /dev/null &
          echo "$!"
        }

        cleanup() {
          kill "$PID"
        }
        trap cleanup EXIT

        PID=$(bg)
        while true; do
          sleep 59
          NEW_PID=$(bg)
          sleep 1 # Give time for the background to be set
          cleanup
          PID="$NEW_PID"
        done
      '';
  };
in
{
  config = {
    home.file."wallpapers/.keep".text = "";

    home.packages = [ background ];

    systemd.user.services.background = {
      Unit = {
        Description = "Set background";
        PartOf = [ "graphical-session.target" ];
        After = [ "graphical-session.target" ];
      };

      Service = {
        Type = "simple";
        ExecStart = lib.getExe background;
        Restart = "always";
      };

      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };
  };

}
