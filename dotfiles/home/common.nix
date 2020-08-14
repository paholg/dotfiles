{ pkgs, ... }:

{
  imports = [ ./emacs.nix ];

  home.packages = with pkgs; [ exa ];

  programs = {
    git = {
      enable = true;
      userName = "Paho Lurie-Gregg";
      userEmail = "paho@paholg.com";
      delta = {
        enable = true;
        options = { line-numbers = true; };
      };
      extraConfig = {
        push.default = "current";
        rebase.autosquash = true;
      };
    };

    home-manager = {
      enable = true;
      path = "...";
    };

    ssh = { enable = true; };

    zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = true;
      # enableVteIntegration = true;
      defaultKeymap = "emacs";
      history = {
        expireDuplicatesFirst = true;
        ignoreDups = true;
        save = 100000;
        size = 100000;
        share = true;
      };
      zplug = {
        enable = true;
        plugins = [{ name = "agkozak/zsh-z"; }];
      };
      initExtra = (builtins.readFile ./zsh_extra.sh);
    };
  };
}
