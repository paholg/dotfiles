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

  home.file = {
    ".tvnamer.json" = {
      text = (builtins.readFile ./tvnamer.json);
    };
  };

  manual = {
    html.enable = true;
    json.enable = true;
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

    tmux = {
      enable = true;
      newSession = true;
      shortcut = "t";
      terminal = "xterm-256color";
      extraConfig = ''
        bind-key 'i' select-pane -U
        bind-key 'k' select-pane -D
        bind-key 'j' select-pane -L
        bind-key 'l' select-pane -R
      '';
    };

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
