{ pkgs, ... }:

{
  home.packages = with pkgs; [
    diffr
    exa
  ];

  programs.emacs = {
    enable = true;
  };

  # programs.firefox = {
  #   enable = true;
  # };

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
    plugins = [
      {
        name = "z";
        src = ./zsh-z.plugin.zsh;
      }
    ];
    initExtra = (builtins.readFile ./zsh_extra.sh);
  };
}
