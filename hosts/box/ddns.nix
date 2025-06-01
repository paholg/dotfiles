{
  config,
  lib,
  pkgs,
  ...
}:
let
  domain = "paholg.com";
  subdomain = "home";
  id = "474697120";
  key_path = config.age.secrets.porkbun_api.path;
  base = "https://api.porkbun.com/api/json/v3/dns";

  ddns = pkgs.writeShellApplication {
    name = "ddns";
    runtimeInputs = with pkgs; [
      curl
      jq
    ];
    text = # bash
      ''
        api=$(cat ${key_path})
        ext_ip=$(curl -s ifconfig.me)
        cur_ip=$(curl -X POST ${base}/retrieve/${domain}/${id} -d "$api" | jq -r .records.[0].content)

        if [ "$ext_ip" = "$cur_ip" ]; then
          echo "IP $cur_ip current, exiting."
          exit 0
        fi

        echo "${subdomain}.${domain} is $cur_ip, should be $ext_ip. Updating."

        body="$(jq -c ".name = \"${subdomain}\" | .type = \"A\" | .content = \"$ext_ip\" | .ttl = \"600\"" ${key_path})"
        curl -X POST ${base}/edit/${domain}/${id} -d "$body"
      '';

  };
in
{
  services.cron = {
    enable = true;
    systemCronJobs = [ "*/10 * * * * root ${lib.getExe ddns} | logger -t ddns" ];
  };
}
