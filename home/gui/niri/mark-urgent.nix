{ lib, pkgs, ... }:
let
  mark-urgent = pkgs.writeShellApplication {
    name = "mark-urgent";
    runtimeInputs = with pkgs; [
      choose
      jq
      libnotify
      niri
    ];
    text = ''
      read -r pid msg

      # Walk up the process tree until we find a PID that owns a niri window.
      windows=$(niri msg -j windows)
      while [ "$pid" -gt 1 ]; do
        id=$(echo "$windows" | jq -r ".[] | select(.pid == $pid) | .id")
        [ -n "$id" ] && break
        pid=$(grep '^PPid:' "/proc/$pid/status" | choose 1) || exit 1
      done

      [ -z "$id" ] && { echo "No niri window found" >&2; exit 1; }
      echo "window $pid"
      niri msg action set-window-urgent --id "$id"

      [ -n "$msg" ] && notify-send -t 5000 "$msg"
    '';
  };

in
{
  home.packages = [ mark-urgent ];
  systemd.user.sockets.mark-urgent = {
    Unit.Description = "mark-urgent activation socket";
    Socket = {
      ListenStream = "%t/mark-urgent.sock";
      SocketMode = "0666";
      Accept = true;
    };
    Install.WantedBy = [ "graphical-session.target" ];
  };

  systemd.user.services."mark-urgent@" = {
    Unit.Description = "Mark the calling terminal's niri window as urgent";
    Service = {
      ExecStart = lib.getExe mark-urgent;
      StandardInput = "socket";
    };
  };
}
