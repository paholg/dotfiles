{ pkgs, ... }:
{
  imports = [ ../../home ];
  home.stateVersion = "20.09";

  custom = {
    username = "paho";

    starship.host_color = "purple";
  };

  home.packages = [

    (pkgs.writeShellApplication {
      name = "healthcheck";
      text = ''
        echo "=== Failed/stuck services ==="
        systemctl list-units --type=service --state=failed,auto-restart,deactivating --no-pager

        echo "=== Application health checks ==="
        curl -sf --connect-timeout 3 --max-time 5 -o /dev/null \
          http://localhost:8099/zigbee/ \
          || echo "  zigbee2mqtt: frontend not responding (port 8099)"

        echo "=== ZFS pool health ==="
        zpool status -x

        echo "=== Drive health (smartd alerts, past 30 days) ==="
        if systemctl is-active --quiet smartd; then
          alerts=$(journalctl -u smartd --since "30 days ago" --priority=warning --no-pager)
          if [[ -n "$alerts" ]]; then
            echo "$alerts"
          else
            echo "  No recent drive alerts"
          fi
        else
          echo "  smartd not running"
        fi

        echo "=== VPN health ==="
        if [[ -f /run/wireguard-handshake ]]; then
          last=$(cat /run/wireguard-handshake)
          now=$(date +%s)
          age=$((now - last))
          if [[ "$last" -eq 0 ]]; then
            echo "  VPN: no handshake recorded"
          elif [[ $age -gt 300 ]]; then
            echo "  VPN: tunnel DOWN (last handshake ''${age}s ago)"
          else
            echo "  VPN: tunnel up (last handshake ''${age}s ago)"
          fi
        else
          echo "  VPN: status file missing (wireguard-status timer not running?)"
        fi
      '';
    })
  ];
}
