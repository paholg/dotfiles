{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.custom;
  helix = getExe config.custom.helix.pkg;
in {
  imports = [
    ./alacritty.nix
    ./display-switch.nix
    ./firefox.nix
    ./helix.nix
    ./hyprland.nix
    ./packages.nix
    ./starship.nix
    ./sway.nix
    ./xfce.nix
    ./xmonad.nix
  ];

  options.custom = {
    username = mkOption {
      type = types.str;
      default = "paho";
    };

    gui = mkOption {
      type = types.bool;
      default = false;
      description = "Set true to enable gui functionality";
    };

    linux = mkOption {
      type = types.bool;
      default = true;
      description = "Set true iff on linux";
    };

    nixos = mkOption {
      type = types.bool;
      default = false;
      description = "Set true iff on NixOs";
    };

    fishInit = mkOption {
      type = types.str;
      default = "";
      description = "Extra fish shell init";
    };
  };

  config = {
    custom.alacritty.enable = mkDefault cfg.gui;
    custom.firefox.enable = mkDefault cfg.gui;
    custom.helix.enable = mkDefault true;
    custom.starship.enable = mkDefault true;
    targets.genericLinux.enable = mkDefault (!cfg.nixos);

    fonts.fontconfig.enable = true;

    # Themes
    gtk = {
      enable = true;
      theme = {
        name = "Adwaita-dark";
        package = pkgs.gnome.gnome-themes-extra;
      };
    };
    qt = {
      enable = true;
      platformTheme = "gnome";
      style.name = "adwaita-dark";
    };

    home = {
      username = cfg.username;
      homeDirectory = "/home/${cfg.username}";

      keyboard.options = ["caps:backspace"];

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
        cb = "cargo build --color always 2>&1 | less -R";
        cc = "cargo check --color always 2>&1 | less -R";
        ct = "cargo test --color always 2>&1 | less -R";
        cw = ''cargo watch -s "cargo check --colow always 2>&1 | less -R"'';

        check_sync = "watch grep -e Dirty: -e Writeback: /proc/meminfo";

        d = "just -f $HOME/dotfiles/justfile";

        hx = "env CARGO_TARGET_DIR=$HOME/.cargo/cache2 ${helix}";

        ls = "eza";
        la = "ls -la";
        ll = "ls -l";

        g = "git";
        gbt = "git bt | head -n10";
        gsw =
          # fish
          ''git switch $(git branch --sort=-committerdate | fzf | cut -c3- | cut -d " " -f1)'';

        ipinfo = "curl ipinfo.io 2> /dev/null | jq .";

        ns = "nh search";

        own =
          # fish
          ''
            fd --no-ignore-vcs -Ho root | xargs -d'
            ' sudo chown -h ${cfg.username}:${cfg.username}'';

        sudop = ''sudo env "PATH=$PATH"'';

        t = "tmux attach";
      };
    };

    home.file = {
      ".cargo/config.toml".source = (pkgs.formats.toml {}).generate "" {
        target.x86_64-unknown-linux-gnu = {
          # linker = "clang";
          linker = "${lib.getExe pkgs.clang}";
          rustflags = [
            "-C"
            "link-arg=-fuse-ld=${lib.getExe' pkgs.mold "mold"}"
          ];
        };
      };
      ".config/inlyne/inlyne.toml".text = ''
        theme = "Dark"
      '';

      ".taplo.toml".source = (pkgs.formats.toml {}).generate "" {
        formatting = {
          align_comments = false;
          align_entries = true;
          array_auto_collapse = true;
          array_auto_expand = false;
          reorder_keys = true;
        };
      };
    };

    manual = {
      html.enable = true;
      json.enable = true;
    };

    nix = {
      package = mkDefault pkgs.nix;
      settings.auto-optimise-store = mkDefault true;
      settings.experimental-features = mkDefault [
        "nix-command"
        "flakes"
      ];

      gc = {
        automatic = true;
        frequency = "weekly";
        options = "--delete-older-than 30d";
      };
    };

    services.redshift = mkIf cfg.gui {
      enable = true;
      provider = "geoclue2";
      temperature = {
        day = 5000;
        night = 3300;
      };
    };

    services.ssh-agent.enable = true;

    xresources.properties = {
      "Xft.dpi" = 120;
      "Xft.autohint" = 0;
      "Xft.lcdfilter" = "lcddefault";
      "Xft.hintstyle" = "hintfull";
      "Xft.hinting" = 1;
      "Xft.antialias" = 1;
      "Xcursor.size" = 24;
    };

    programs = {
      atuin = {
        enable = true;
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

        interactiveShellInit =
          # fish
          ''
            set fish_greeting # disable
            # TODO: Temporary fix for fish completions.
            # Remove once this is merged:
            # https://github.com/nix-community/home-manager/pull/5199
            set fish_complete_path "${config.home.path}/share/fish/vendor_completions.d" $fish_complete_path
          ''
          + cfg.fishInit;

        functions = {
          # nshell = "";
          # e = "";
        };
      };

      fzf = {
        enable = true;
        enableFishIntegration = true;
      };

      git = mkIf (cfg.username == "paho") {
        enable = true;
        userName = "Paho Lurie-Gregg";
        userEmail = mkDefault "paho@paholg.com";
        delta = {
          enable = true;
          options = {
            line-numbers = true;
          };
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

        addKeysToAgent = "yes";

        matchBlocks = {
          box = {
            user = cfg.username;
            hostname = "10.0.0.4";
          };
          home = {
            user = cfg.username;
            hostname = "home.paholg.com";
          };
          fractal = {
            user = cfg.username;
            hostname = "10.0.0.5";
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
        enableFishIntegration = true;
      };
    };
  };
}
