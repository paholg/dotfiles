{ pkgs, ... }:

{
  imports = [ ./emacs.nix ];

  home.packages = with pkgs; [ exa gitAndTools.delta ];
  home.stateVersion = "20.09";

  home = {
    keyboard.options = { };
    sessionVariables = {
      EDITOR = "emacsclient -c";
      RUST_NEW_ERROR_FORMAT = true;
      CARGO_HOME = "$HOME/.cargo";
    };
  };

  programs = {
    git = {
      enable = true;
      userName = "Paho Lurie-Gregg";
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
      shellAliases = {
        ls = "exa";
        la = "ls -la";
        ll = "ls -l";
      };
      zplug = {
        enable = true;
        plugins = [{ name = "agkozak/zsh-z"; }];
      };
      initExtra = (builtins.readFile ./zsh_extra.sh);
    };
  };

  services = {
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
    };
  };
}
