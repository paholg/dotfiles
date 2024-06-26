{
  config,
  lib,
  pkgs,
  ...
}: let
  helix = lib.getExe' pkgs.helix "hx";
in {
  imports = [./helix.nix ./packages.nix ./starship.nix];

  home.stateVersion = "20.09";

  fonts.fontconfig.enable = true;

  home = {
    sessionVariables = {
      EDITOR = helix;
      RUST_NEW_ERROR_FORMAT = "true";
      CARGO_HOME = "$HOME/.cargo";
      MANPAGER = "sh -c 'col -bx | bat -l man -p'";
      MANROFFOPT = "-c";
      CARGO_TARGET_DIR = "$HOME/.cargo/cache";
    };

    sessionPath = [
      "$HOME/dotfiles/bin"
      "$HOME/dotfiles/hosts/$(hostname)/bin"
      "$HOME/.cargo/bin"
      "$HOME/go/bin"
    ];

    shellAliases = {
      cb = ''cargo build --color always 2>&1 | less -R'';
      cc = ''cargo check --color always 2>&1 | less -R'';
      ct = ''cargo test --color always 2>&1 | less -R'';
      cw = ''cargo watch -s "cargo check --colow always 2>&1 | less -R"'';

      check_sync = ''watch grep -e Dirty: -e Writeback: /proc/meminfo'';

      d = ''just -f $HOME/dotfiles/justfile'';

      hx = "env CARGO_TARGET_DIR=$HOME/.cargo/cache2 ${helix}";

      ls = "eza";
      la = "ls -la";
      ll = "ls -l";

      g = "git";
      gbt = "git bt | head -n10";
      gsw = ''
        git switch $(git branch --sort=-committerdate | fzf | cut -c3- | cut -d " " -f1)'';

      ipinfo = "curl ipinfo.io 2> /dev/null | jq .";

      ns = "nh search";

      own = "fd --no-ignore-vcs -Ho root | xargs -d'\n' sudo chown -h paho:paho";

      sudop = ''sudo env "PATH=$PATH"'';

      t = "tmux attach";
    };
  };

  home.file = {
    ".cargo/config.toml".source = (pkgs.formats.toml {}).generate "" {
      target.x86_64-unknown-linux-gnu = {
        linker = "clang";
        # linker = "${lib.getExe pkgs.clang}";
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

    bash = {
      enable = true;
      # From nixos wiki, set bash to launch fish unless the parent is fish:
      # https://nixos.wiki/wiki/Fish
      initExtra = ''
        if [[ $(${pkgs.procps}/bin/ps --no-header --pid=$PPID --format=comm) != "fish" && -z ''${BASH_EXECUTION_STRING} ]]
        then
          shopt -q login_shell && LOGIN_OPTION='--login' || LOGIN_OPTION=""
          exec ${pkgs.fish}/bin/fish $LOGIN_OPTION
        fi
      '';
    };

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    fish = {
      enable = true;

      # TODO: Temporary fix for fish completions.
      # Remove once this is merged:
      # https://github.com/nix-community/home-manager/pull/5199
      interactiveShellInit = ''
        set fish_complete_path "${config.home.path}/share/fish/vendor_completions.d" $fish_complete_path
      '';

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

    ssh = {
      enable = true;

      matchBlocks = {
        box = {
          hostname = "10.0.0.4";
          user = "paho";
        };
        home = {
          hostname = "home.paholg.com";
          user = "paho";
        };
        fractal = {
          hostname = "10.0.0.5";
          user = "paho";
        };
      };
    };

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
      autosuggestion.enable = true;
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
