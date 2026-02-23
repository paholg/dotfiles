{ config, ... }:
{
  # Grant nginx access to kanidm group for ACME certificates
  users.users.nginx.extraGroups = [ "kanidm" ];

  services.kanidm.server = {
    enable = true;
    settings = {
      version = "2";
      bindaddress = "0.0.0.0:${toString config.custom.ports.kanidm}";
      ldapbindaddress = "0.0.0.0:${toString config.custom.ports.kanidm_ldap}";
      db_fs_type = "zfs";

      tls_chain = "/var/lib/acme/auth.paholg.com/fullchain.pem";
      tls_key = "/var/lib/acme/auth.paholg.com/key.pem";

      domain = "auth.paholg.com";
      origin = "https://auth.paholg.com";

      http_client_address_info.x-forward-for = [ "127.0.0.1" ];

      online_backup = {
        schedule = "00 00 * * 1";
        versions = 7;
      };
    };
  };

  # The kanidm module won't let us set the db_path directly.
  systemd.services.kanidm.serviceConfig.BindPaths = [
    "${config.custom.drives.data}/kanidm:/var/lib/kanidm"
  ];

  # Grant kanidm access to ACME certificates
  security.acme.certs."auth.paholg.com".group = "kanidm";

  services.nginx.virtualHosts."auth.paholg.com" = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "https://127.0.0.1:${toString config.custom.ports.kanidm}";
      extraConfig = ''
        proxy_ssl_verify off;
        proxy_ssl_server_name on;
        proxy_ssl_name auth.paholg.com;
      '';
    };
  };
}
