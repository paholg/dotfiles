{ ... }:

{
  imports = [ ./packages-linux.nix ];

  services = {
    emacs.enable = true;
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
    };
  };
}
