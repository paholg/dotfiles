{ config, ... }:
{
  services.ddclient = {
    enable = true;
    interval = "1min";
    configFile = config.age.secrets.porkbun_ddclient_config.path;
  };
}
