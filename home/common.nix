{
  lib,
  pkgs,
  ...
}: {
  imports = [./helix.nix ./packages.nix ./starship.nix];

  home.stateVersion = "20.09";

  fonts.fontconfig.enable = true;

  home = {
    sessionVariables = {
      EDITOR = "hx";
      RUST_NEW_ERROR_FORMAT = "true";
      CARGO_HOME = "$HOME/.cargo";
      MANPAGER = "sh -c 'col -bx | bat -l man -p'";
      MANROFFOPT = "-c";
      CARGO_TARGET_DIR = "$HOME/.cargo/cache";
    };

    shellAliases = {
      cb = ''cargo build --color always 2>&1 | less -R'';
      cc = ''cargo check --color always 2>&1 | less -R'';
      ct = ''cargo test --color always 2>&1 | less -R'';
      cw = ''cargo watch -s "cargo check --colow always 2>&1 | less -R"'';

      check_sync = ''watch grep -e Dirty: -e Writeback: /proc/meminfo'';

      hx = "env CARGO_TARGET_DIR=$HOME/.cargo/cache2 ${lib.getExe' pkgs.helix "hx"}";

      ls = "eza";
      la = "ls -la";
      ll = "ls -l";

      g = "git";
      gbt = "git bt | head -n10";
      gsw = ''
        git switch $(git branch --sort=-committerdate | fzf | cut -c3- | cut -d " " -f1)'';

      ipinfo = "curl ipinfo.io 2> /dev/null | jq .";

      ns = "nix search nixpkgs";

      own = "fd --no-ignore-vcs -Ho root | xargs -d'\n' sudo chown -h paho:paho";

      sudop = "sudo env PATH=$PATH";

      sw = "home-manager --flake $HOME/dotfiles switch";
      # TODO: Make pure.
      swn = "sudo nixos-rebuild --flake $HOME/dotfiles switch --impure";
      t = "tmux attach";
    };
  };

  home.file = {
    ".cargo/config.toml".source = (pkgs.formats.toml {}).generate "" {
      target.x86_64-unknown-linux-gnu = {
        # TODO: Need to swap or fix for ubuntu.
        # linker = "clang";
        linker = "${lib.getExe pkgs.clang}";
        rustflags = ["-C" "link-arg=-fuse-ld=${lib.getExe' pkgs.mold "mold"}"];
      };
    };
    ".config/inlyne/inlyne.toml".text = ''
      theme = "Dark"
    '';

    ".config/helix/themes/paho-theme.toml".source = ./paho-theme.toml;

    ".taplo.toml".source = (pkgs.formats.toml {}).generate "" {
      formatting = {
        align_comments = false;
        array_auto_collapse = false;
        array_auto_expand = false;
        reorder_keys = false;
      };
    };

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

  nix.gc = {
    automatic = true;
    frequency = "weekly";
    options = "--delete-older-than 30d";
  };

  programs = {
    atuin = {
      enable = true;
      enableZshIntegration = true;
      enableFishIntegration = true;
      flags = ["--disable-up-arrow"];
    };

    direnv = {
      enable = true;
      enableZshIntegration = true;
    };

    fish = {
      enable = true;

      functions = {
        # nshell = "";
        # e = "";
      };
    };

    fzf = {
      enable = true;
      enableZshIntegration = true;
      enableFishIntegration = true;
    };

    git = {
      enable = true;
      userName = "Paho Lurie-Gregg";
      delta = {
        enable = true;
        options = {line-numbers = true;};
      };
      extraConfig = {
        diff.external = "difft";
        pull.rebase = false;
        push.default = "current";
        rebase.autosquash = true;
        init.defaultBranch = "main";
        credential.helper = "store";
      };
      lfs.enable = true;
      aliases = {
        b = "branch";
        bt = "branch -v --sort=-committerdate";
        c = "commit";
        co = "checkout";
        d = "diff";
        dc = "diff --cached";
        fixup = "!git commit -a --amend --no-edit && git push -f";
        l = "log";
        rsw = "restore --staged --worktree";
        s = "status";
        sw = "switch";
      };
    };

    home-manager = {
      enable = true;
    };

    ssh = {enable = true;};

    tmux = {
      enable = true;
      newSession = true;
      terminal = "xterm-256color";
    };

    zoxide = {
      enable = true;
      enableZshIntegration = true;
      enableFishIntegration = true;
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
      initExtra = builtins.readFile ./zsh_extra.sh;
    };
  };
}
