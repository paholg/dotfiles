{ ... }:

{
  imports = [ ./packages-linux.nix ];

  services = {
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
    };
  };
}
