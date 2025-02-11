{
  config,
  lib,
  pkgs,
  ...
}:
let
  homeDir = config.home.homeDirectory;
in
{
  imports = [
    ./i3.nix
    ./alacritty.nix
    ./display-switch.nix
    ./firefox.nix
    ./helix.nix
    ./mangohud.nix
    ./packages.nix
    ./starship.nix
    ./sway.nix
    ./x11_lock.nix
    ./xfce.nix
    ./xmonad.nix
  ];

  options.custom = {
    fish_extra_init = lib.mkOption {
      type = lib.types.str;
      default = "";
    };
    gui = lib.mkOption { type = lib.types.bool; };
    wayland = lib.mkOption { type = lib.types.bool; };
    x11 = lib.mkOption { type = lib.types.bool; };
    linux = lib.mkOption { type = lib.types.bool; };
    nixos = lib.mkOption { type = lib.types.bool; };
    username = lib.mkOption { type = lib.types.str; };
  };

  config = {
    targets.genericLinux.enable = !config.custom.nixos;

    custom = lib.mkIf (!config.custom.gui || !config.custom.linux) {
      wayland = false;
      x11 = false;
    };

    fonts.fontconfig.enable = true;

    xdg.userDirs = {
      enable = true;
      createDirectories = false;
      desktop = "${homeDir}/.";
      documents = "${homeDir}/docs";
      download = "${homeDir}/downloads";
      music = "${homeDir}/docs/music";
      pictures = "${homeDir}/docs/pics";
      publicShare = "${homeDir}/docs/public";
      templates = "${homeDir}/docs/templates";
      videos = "${homeDir}/docs/videos";
    };

    # Themes
    gtk = {
      enable = config.custom.gui;
      theme = {
        name = "Adwaita-dark";
        package = pkgs.gnome-themes-extra;
      };
    };
    qt = {
      enable = config.custom.gui;
      platformTheme.name = "adwaita";
      style.name = "adwaita-dark";
    };

    home = {
      username = config.custom.username;
      homeDirectory = "/home/${config.custom.username}";

      keyboard.options = [ "caps:backspace" ];

      sessionVariables = {
        RUST_NEW_ERROR_FORMAT = "true";
        CARGO_HOME = "$HOME/.cargo";
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
        bathelp = "bat --plain --language help";

        check_sync = "watch grep -e Dirty: -e Writeback: /proc/meminfo";

        d = "just -f $HOME/dotfiles/justfile";

        just = "${lib.getExe pkgs.just} --command-color=blue";

        ls = "eza";
        la = "ls -la";
        ll = "ls -l";

        g = "git";
        gsw =
          # fish
          ''git switch $(git branch --sort=-committerdate | fzf | cut -c3- | cut -d " " -f1)'';

        ipinfo = "curl ipinfo.io 2> /dev/null | jq .";

        ns = "nh search";

        own =
          # fish
          ''
            fd --no-ignore-vcs -Ho root | xargs -d'
            ' sudo chown -h ${config.custom.username}:${config.custom.username}'';
      };
    };

    home.file = {
      ".cargo/config.toml".source = (pkgs.formats.toml { }).generate "" {
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

      ".taplo.toml".source = (pkgs.formats.toml { }).generate "" {
        formatting = {
          align_comments = false;
          align_entries = true;
          array_auto_collapse = true;
          array_auto_expand = false;
          reorder_keys = false;
        };
      };
    };

    manual = {
      html.enable = true;
      json.enable = true;
    };

    nix = {
      package = lib.mkDefault pkgs.nix;
      settings.auto-optimise-store = true;
      settings.experimental-features = [
        "nix-command"
        "flakes"
      ];

      gc = {
        automatic = true;
        frequency = "weekly";
        options = "--delete-older-than 30d";
      };
    };

    services.blueman-applet = lib.mkIf config.custom.gui {
      enable = true;
    };

    services.redshift = lib.mkIf config.custom.gui {
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
        flags = [ "--disable-up-arrow" ];
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

        interactiveShellInit = # fish
          ''
            set fish_greeting # disable
            # TODO: Temporary fix for fish completions.
            # Remove once this is merged:
            # https://github.com/nix-community/home-manager/pull/5199
            set fish_complete_path "${config.home.path}/share/fish/vendor_completions.d" $fish_complete_path

            batman --export-env | source
            eval (batpipe)
          ''
          + config.custom.fish_extra_init;

        functions = {
          nshell = {
            wraps = "nix shell";
            body = # fish
              ''
                set pkgs (string replace -ar '([\S]+)' 'nixpkgs#$0' $argv)
                nix shell $pkgs --command fish
              '';
          };
          h = {
            description = "Render the --help for a command with bat";
            body = # fish
              ''
                $argv --help 2>&1 | bathelp
              '';
          };
        };
      };

      fzf = {
        enable = true;
        enableFishIntegration = true;
      };

      git = lib.mkIf (config.custom.username == "paho") {
        enable = true;
        userName = "Paho Lurie-Gregg";
        userEmail = lib.mkDefault "paho@paholg.com";
        delta = {
          enable = true;
          options = {
            line-numbers = true;
          };
        };
        extraConfig = {
          diff.external = "difft";
          pull.rebase = true;
          push.default = "current";
          rebase.autosquash = true;
          init.defaultBranch = "main";
          credential.helper = "store";
          log.date = "local";
        };
        aliases = {
          b = "branch";
          bt = "branch -v --sort=-committerdate";
          c = "commit";
          co = "checkout";
          d = "diff";
          dc = "diff --cached";
          fixup = "!git commit -a --amend --no-edit && git push -f";
          l = "log";
          rs = "restore --staged";
          rsw = "restore --staged --worktree";
          s = "status";
          sw = "switch";
        };
      };

      jujutsu = lib.mkIf (config.custom.username == "paho") {
        enable = true;
        settings = {
          user = {
            name = "Paho Lurie-Gregg";
            email = "paho@paholg.com";
          };

          ui = {
            diff.tool = [
              "difft"
              "--color=always"
              "$left"
              "$right"
            ];

            pager = ":builtin";
          };
        };
      };

      home-manager.enable = true;

      ssh = {
        enable = true;

        addKeysToAgent = "yes";

        matchBlocks =
          lib.mapAttrs
            (name: host: {
              user = config.custom.username;
              hostname = host;
            })
            {
              home = "home.paholg.com";
              t14s = "10.0.0.3";
              box = "10.0.0.4";
              fractal = "10.0.0.5";
              frame-wifi = "10.0.0.6";
              frame-eth = "10.0.0.7";
            };
      };

      tmux = {
        enable = true;
        newSession = true;
        terminal = "xterm-256color";
      };

      zellij = {
        enable = false;
        settings = {
          theme = "custom";
          themes.custom = {
            fg = "#a3a3a3";
            bg = "#1f2022";
            black = "#1f2022";
            red = "#f2241f";
            green = "#67b11d";
            yellow = "#b1951d";
            blue = "#4f97d7";
            magenta = "#a31db1";
            cyan = "#2d9574";
            white = "#a3a3a3";
            orange = "#ff5733";

          };
        };
      };

      zoxide = {
        enable = true;
        enableFishIntegration = true;
      };
    };
  };
}
