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
      '';
    })
  ];
}
