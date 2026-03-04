{ pkgs, ... }:
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
      # Walk up the process tree until we find a PID that owns a niri window.
      windows=$(niri msg -j windows)
      pid=$1
      while [ "$pid" -gt 1 ]; do
        id=$(echo "$windows" | jq -r ".[] | select(.pid == $pid) | .id")
        [ -n "$id" ] && break
        pid=$(grep '^PPid:' "/proc/$pid/status" | choose 1) || exit 1
      done

      [ -z "$id" ] && { echo "No niri window found for PID $1" >&2; exit 1; }
      echo "PID $1 -> window $pid"
      niri msg action set-window-urgent --id "$id"

      [ -n "''${2:-}" ] && notify-send -t 5000 "$2"
    '';
  };

  listener = pkgs.writers.writePython3 "mark-urgent-listen" { } ''
    import socket
    import struct
    import subprocess
    import sys

    LISTEN_FDS_START = 3


    def main():
        srv = socket.fromfd(
            LISTEN_FDS_START, socket.AF_UNIX, socket.SOCK_STREAM
        )
        srv.setblocking(True)

        print("mark-urgent: listening")
        while True:
            conn, _ = srv.accept()
            try:
                cred = conn.getsockopt(
                    socket.SOL_SOCKET,
                    socket.SO_PEERCRED,
                    struct.calcsize("iII"),
                )
                peer_pid = struct.unpack("iII", cred)[0]
                msg = conn.makefile().readline().strip()
                cmd = [sys.argv[1], str(peer_pid)]
                if msg:
                    cmd.append(msg)
                subprocess.run(cmd, check=True)
                conn.close()
            except Exception as e:
                print(f"Error: {e}")


    main()
  '';
in
{
  home.packages = [ mark-urgent ];
  systemd.user.sockets.mark-urgent = {
    Unit.Description = "mark-urgent activation socket";
    Socket = {
      ListenStream = "%t/mark-urgent.sock";
      SocketMode = "0666";
    };
    Install.WantedBy = [ "graphical-session.target" ];
  };

  systemd.user.services.mark-urgent = {
    Unit = {
      Description = "Mark the calling terminal's niri window as urgent";
      Requires = [ "mark-urgent.socket" ];
      After = [ "mark-urgent.socket" ];
    };
    Service = {
      ExecStart = "${listener} ${mark-urgent}/bin/mark-urgent";
      Restart = "on-failure";
    };
  };
}
