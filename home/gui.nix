{...}: {
  imports = [./packages-gui.nix];

  nixpkgs.config.allowUnfree = true;

  services = {
    redshift = {
      enable = true;
      provider = "geoclue2";
      temperature = {
        day = 5000;
        night = 3300;
      };
    };
  };
}
