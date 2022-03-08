{ ... }:

{
  imports = [ ./emacs.nix ./packages.nix ./starship.nix ];

  home.stateVersion = "20.09";

  fonts.fontconfig.enable = true;

  home = {
    sessionVariables = {
      EDITOR = "emacsclient -t";
      ALTERNATE_EDITOR = "emacs";
      RUST_NEW_ERROR_FORMAT = true;
      CARGO_HOME = "$HOME/.cargo";
      MANPAGER = "sh -c 'col -bx | bat -l man -p'";
    };
  };

  home.file = {
    ".config/rustfmt/rustfmt.toml".text = ''
      edition = "2018"
    '';
    ".config/nix/nix.conf".text = ''
      experimental-features = nix-command flakes
    '';
    ".profile".text = ''
      # For non-NixOs, single-user:
      if test -f $HOME/.nix-profile/etc/profile.d/nix.sh; then
        . $HOME/.nix-profile/etc/profile.d/nix.sh
      fi

      # For non-NixOs, multi-user:
      if test -f '/nix/var/nix/profiles/default/etc/profile.d/nix.sh'; then
        . '/nix/var/nix/profiles/default/etc/profile.d/nix.sh'
      fi

      path_dirs="
          $HOME/.cargo/bin
          $HOME/dotfiles/bin
          $HOME/bin
      "

      for dir in $(echo $path_dirs); do
          export PATH=$dir:$PATH
      done

      if `command -v rustc >/dev/null 2>&1`; then
          export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
      fi
    '';

    ".zprofile".text = ''
      . ~/.profile
    '';

    ".Xresources".text = ''
      Xft.dpi: 120
      Xft.autohint: 0
      Xft.lcdfilter: lcddefault
      Xft.hintstyle: hintfull
      Xft.hinting: 1
      Xft.antialias: 1
      Xcursor.size: 16
    '';
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
        pull.rebase = false;
        push.default = "current";
        rebase.autosquash = true;
        init.defaultBranch = "main";
        credential.helper = "store";
      };
      aliases = {
        b = "branch";
        bt = "branch -v --sort=-committerdate";
        c = "commit";
        co = "checkout";
        d = "diff";
        dc = "diff --cached";
        l = "log";
        s = "status";
        sw = "switch";
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

    zoxide = {
      enable = true;
      enableZshIntegration = true;
    };

    zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = true;
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

        em = "emacsclient -c";
        emt = "emacsclient -t";

        branches = "git branch -v --sort=-committerdate | head -n10";
        ipinfo = "curl ipinfo.io 2> /dev/null | jq .";

        sudop = "sudo env PATH=$PATH";

        t = "tmux attach";
      };
      initExtra = (builtins.readFile ./zsh_extra.sh);
    };
  };
}
