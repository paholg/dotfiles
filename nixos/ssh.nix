{ ... }:
{
  config = {
    services.openssh = {
      enable = true;
      settings = {
        PasswordAuthentication = false;
        PermitRootLogin = "no";
      };
    };
    services.fail2ban.enable = true;

    networking.firewall.allowedUDPPortRanges = [
      # Ports for Mosh
      {
        from = 60001;
        to = 60999;
      }
    ];

    users.users.paho.openssh.authorizedKeys.keys = map (attrs: attrs.paho) (
      builtins.attrValues (import ../keys.nix)
    );
  };
}
