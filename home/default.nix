{
  gui,
  nixos,
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
    ./bacon.nix
    ./helix.nix
    ./packages.nix
    ./script.nix
    ./starship.nix
    ./stylix.nix
  ]
  ++ (if gui then [ ./gui ] else [ ]);

  options.custom = {
    fish_extra_init = lib.mkOption {
      type = lib.types.str;
      default = "";
    };
    swaylock.color = lib.mkOption {
      type = lib.types.str;
      default = "888888";
    };
    username = lib.mkOption { type = lib.types.str; };
  };

  config = {
    targets.genericLinux.enable = !nixos;

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
        "$HOME/.cargo/bin"
      ];

      shellAliases = {
        audio = "systemctl --user restart pipewire";

        bathelp = "bat --plain --language help";
        batlog = "bat -pp -l log";

        bg = "systemctl --user restart background";

        check_sync = "watch grep -e Dirty: -e Writeback: /proc/meminfo";

        icat = "kitten icat";

        j = "journalctl -e";
        ju = "journalctl -e --user";
        s = "systemctl";
        su = "systemctl --user";
        just = "${lib.getExe pkgs.just} --command-color=blue";

        ls = "eza";
        la = "ls -la";
        ll = "ls -l";

        g = "git";
        gsw =
          # fish
          ''git switch $(git branch --sort=-committerdate | fzf | cut -c3- | cut -d " " -f1)'';

        ipinfo = "curl ipinfo.io 2> /dev/null | jq .";

        ns = "nix-search";

        own =
          # fish
          ''
            fd --no-ignore-vcs -Ho root | xargs -d'
            ' sudo chown -h ${config.custom.username}:${config.custom.username}'';
        y = "yazi";
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
        dates = "weekly";
        options = "--delete-older-than 30d";
      };
    };

    services.blueman-applet = lib.mkIf gui {
      enable = true;
    };

    services.ssh-agent.enable = true;

    xresources.properties = {
      "Xft.dpi" = 120;
      "Xft.autohint" = 0;
      "Xft.lcdfilter" = "lcddefault";
      "Xft.hintstyle" = "hintfull";
      "Xft.hinting" = 1;
      "Xft.antialias" = 1;
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

      bat = {
        enable = true;
        extraPackages = with pkgs.bat-extras; [
          batdiff
          # batgrep # broken
          batman
          batpipe
          batwatch
          prettybat
        ];
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

          batman --export-env | source
          eval (batpipe)

          envswitch setup fish | source
        ''
        + config.custom.fish_extra_init;

        functions = {
          e = {
            description = "Bring to foreground or open helix";
            body = # fish
              ''
                if test "$(jobs -lc)" = "hx"
                  fg
                else
                  hx
                end 
              '';
          };
          h = {
            description = "Render the --help for a command with bat";
            body = # fish
              ''
                if not isatty stdin
                  bathelp
                else
                  $argv --help 2>&1 | bathelp
                end
              '';
          };
          nshell = {
            wraps = "nix shell";
            body = # fish
              ''
                set pkgs (string replace -ar '([\S]+)' 'nixpkgs#$0' $argv)
                nix shell $pkgs --command fish
              '';
          };
        };
      };

      fzf = {
        enable = true;
        enableFishIntegration = true;
      };

      delta = {
        enable = true;
        options = {
          line-numbers = true;
        };
      };

      git = lib.mkIf (config.custom.username == "paho") {
        enable = true;
        settings = {
          brannch.autoSetupMerge = "always";
          core.pager = "delta";
          credential.helper = "store";
          diff.external = "difft";
          init.defaultBranch = "main";
          interactive.diffFilter = "delta --color-only";
          log.date = "local";
          pull.rebase = true;
          push.autoSetupRemote = "true";
          push.default = "current";
          rebase.autosquash = true;

          user = {
            name = "Paho Lurie-Gregg";
            email = "paho@paholg.com";
          };

          alias = {
            b = "branch";
            bt = "branch -v --sort=-committerdate";
            c = "commit";
            co = "checkout";
            d = "diff";
            dc = "diff --cached";
            fixup = "!git commit -a --amend --no-edit && git push -f";
            l = ''!l() { git log "$@" | bat -n; }; l'';
            rs = "restore --staged";
            rsw = "restore --staged --worktree";
            s = "status";
            sw = "switch";
          };
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
        enableDefaultConfig = false;

        matchBlocks =
          (lib.mapAttrs
            (name: host: {
              user = config.custom.username;
              hostname = host;
            })
            {
              home = "home.paholg.com";
              box = "10.0.0.4";
              fractal = "10.0.0.5";
              frame-eth = "10.0.0.6";
              frame-wifi = "10.0.0.7";
            }
          )
          // {
            router = {
              user = "admin";
              hostname = "10.0.0.1";
            };
            "*" = {
              addKeysToAgent = "yes";
            };
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
