{
  config,
  lib,
  pkgs,
  ...
}:
let
  subdomain = "home";
  domain = "paholg.com";
  key_path = config.age.secrets.gandi.path;

  ddns = pkgs.writeShellApplication {
    name = "ddns";
    runtimeInputs = with pkgs; [
      curl
      jq
      dig
    ];
    text = # bash
      ''
        key=$(cat ${key_path})
        ext_ip=$(curl -s ifconfig.me)
        cur_ip=$(dig +short ${subdomain}.${domain})

        if [ "$ext_ip" = "$cur_ip" ]; then
          echo "IP $cur_ip current, exiting."
          exit 0
        fi

        echo "${subdomain}.${domain} is $cur_ip, should be $ext_ip. Updating"

        current_zone_href=$(curl -s -H "X-Api-Key: $key" https://dns.api.gandi.net/api/v5/domains/${domain} | jq -r '.zone_records_href')

        curl -D- -X PUT -H "Content-Type: application/json" \
                -H "X-Api-Key: $key" \
                -d "{\"rrset_name\": \"${subdomain}\",
                     \"rrset_type\": \"A\",
                     \"rrset_ttl\": 1200,
                     \"rrset_values\": [\"$ext_ip\"]}" \
                "$current_zone_href/${subdomain}/A"
      '';

  };
in
{
  services.cron = {
    enable = true;
    systemCronJobs = [ "* * * * * root ${lib.getExe ddns} &> /tmp/ddns.log" ];
  };
}
