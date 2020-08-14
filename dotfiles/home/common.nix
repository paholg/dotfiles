{ pkgs, ... }:

{
  imports = [ ./emacs.nix ];

  home.packages = with pkgs; [ diffr exa ];

  programs.home-manager = {
    enable = true;
    path = "...";
  };

  programs.zsh = {
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
}
