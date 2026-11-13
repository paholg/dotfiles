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
    ./devconcurrent.nix
    ./claude
    ./easyeffects.nix
    ./helix.nix
    ./packages.nix
    ./starship.nix
    ./stylix.nix
    ./zed.nix
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
    _module.args.symlink =
      path: config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/symlink/${path}";

    targets.genericLinux.enable = !nixos;

    fonts.fontconfig.enable = true;

    xdg.userDirs = {
      enable = true;
      setSessionVariables = false;
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

    xdg.configFile = lib.listToAttrs (
      map (
        f:
        let
          rel = lib.removePrefix (toString ../config + "/") (toString f);
        in
        lib.nameValuePair rel {
          source = config.lib.file.mkOutOfStoreSymlink "${homeDir}/dotfiles/config/${rel}";
        }
      ) (lib.filesystem.listFilesRecursive ../config)
    );

    home = {
      username = config.custom.username;
      homeDirectory = "/home/${config.custom.username}";

      keyboard.options = [ "caps:backspace" ];

      sessionVariables = {
        RUST_NEW_ERROR_FORMAT = "true";
        CARGO_HOME = "$HOME/.cargo";
        MANROFFOPT = "-c";

        SCCACHE_DIR = "$HOME/.cache/sccache";
        SCCACHE_CACHE_SIZE = "100G";
      };

      sessionPath = [
        "$HOME/dotfiles/bin"
        "$HOME/.cargo/bin"
      ];

      shellAliases = {
        audio = "systemctl --user restart wireplumber";

        bathelp = "bat --plain --language help";
        batlog = "bat -pp -l log";

        bg = "systemctl --user restart background";

        clo = "x claude --model opus";
        clf = "x claude --model fable";

        check_sync = "watch grep -e Dirty: -e Writeback: /proc/meminfo";

        icat = "kitten icat";

        j = "journalctl -e";
        ju = "journalctl -e --user";
        just = "${lib.getExe pkgs.just} --command-color=blue";

        lg = "lazygit";

        ls = "eza";
        la = "ls -la";
        ll = "ls -l";

        g = "git";
        gsw =
          # fish
          ''git switch $(git branch --sort=-committerdate | fzf | cut -c3- | cut -d " " -f1)'';
        gr = "git-review";

        ipinfo = "curl ipinfo.io 2> /dev/null | jq .";

        own =
          # fish
          ''
            fd --no-ignore-vcs -Ho root | xargs -d'
            ' sudo chown -h ${config.custom.username}:${config.custom.username}'';

        s = "kitten ssh";

        y = "yazi";

        # Devconcurrent aliases
        d = "dc go";

        x = "dc x";

        u = "dc up --go --from main -p ";
        ud = "u dotfiles";
        udev = "u devconcurrent";
        um = "u models";
        us = "u scholarly";
      };
    };

    home.file = {
      ".cargo/config.toml".source = (pkgs.formats.toml { }).generate "" {
        build.rustc-wrapper = lib.getExe pkgs.sccache;
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
        dates = "daily";
        options = "--delete-older-than 7d";
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
        config = {
          global.hide_env_diff = true;
          whitelist.prefix = [
            "${config.home.homeDirectory}/src/"
            "${config.xdg.dataHome}/devconcurrent/"
          ];
        };
      };

      fish = {
        enable = true;

        interactiveShellInit = # fish
        ''
          set fish_greeting # disable

          batman --export-env | source
          eval (batpipe)

          COMPLETE=fish devconcurrent | source 
          argc --argc-completions fish git-review | source

          complete -c ws -f -n __fish_use_subcommand -a "track untrack go"
          complete -c ws -n "__fish_seen_subcommand_from untrack" -fa "(jq -r 'keys[]' $HOME/.local/state/ws.json 2>/dev/null)"
          complete -c ws -n "__fish_seen_subcommand_from track" -F

          # A fresh terminal starts in ~; move it to its niri workspace's directory.
          test "$PWD" = "$HOME"; and ws go 2>/dev/null
        ''
        + config.custom.fish_extra_init;

        functions = {
          fish_user_key_bindings = {
            body = # fish
              ''
                bind \cf forward-bigword
                bind \cb backward-bigword
                # Erase fzf's shift-tab binding
                bind --erase shift-tab
                bind --erase -M insert shift-tab
              '';
          };
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
          ns = {
            description = "Fuzzy search nix packages and options";
            body = # fish
              ''
                nix-search-tv print | fzf --preview 'nix-search-tv preview {}' --scheme history
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
          wname = {
            body = # fish
              ''
                if set -q argv[1]
                  niri msg action set-workspace-name "$argv[1]"
                  ws track
                else
                  ws untrack
                  niri msg action unset-workspace-name
                end
              '';
          };

          ws = {
            description = "Track a directory for each niri workspace";
            body = # fish
              ''
                set -q NIRI_SOCKET; or return 0

                # Map of niri workspace name -> directory.
                set -l file ~/.local/state/ws.json
                test -s $file; or echo '{}' >$file
                set -l name (niri msg --json workspaces 2>/dev/null | jq -r '.[] | select(.is_focused) | .name // empty')

                switch "$argv[1]"
                  case track
                    set -l dir $PWD
                    set -q argv[2]; and set dir (path resolve $argv[2])
                    jq --arg n "$name" --arg d $dir '.[$n] = $d' $file >$file.tmp; and mv $file.tmp $file
                  case untrack
                    set -q argv[2]; and set name $argv[2]
                    jq --arg n "$name" 'del(.[$n])' $file >$file.tmp; and mv $file.tmp $file
                  case go
                    cd (jq -r --arg n "$name" '.[$n]' $file)
                  case '*'
                    echo "Usage: ws track [PATH] | ws untrack [NAME] | ws go"
                    return 1
                end
              '';
          };
        };
      };

      delta = {
        enable = true;
        options = {
          line-numbers = true;
        };
      };

      fzf = {
        enable = true;
        enableFishIntegration = true;

        historyWidget.command = "";
      };

      git = lib.mkIf (config.custom.username == "paho") {
        enable = true;
        signing.format = "openpgp";
        settings = {
          branch.autoSetupMerge = "true";
          core.pager = lib.getExe pkgs.delta;
          credential.helper = "store";
          diff.external = lib.getExe pkgs.difftastic;
          init.defaultBranch = "main";
          interactive.diffFilter = "${lib.getExe pkgs.delta} --color-only";
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
            l = ''!l() { git log "$@" | bat -p; }; l'';
            push-remote = ''
              !push-remote() {
                # Hack to push to a remote branch when using `gh pr checkout`.
                branch=$(git branch --show-current)
                git push $(git config branch."$branch".pushremote) HEAD:$(git config branch."$branch".merge | sd "refs/heads/" "")
              }
              push-remote
            '';
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
              (lib.getExe pkgs.difftastic)
              "--color=always"
              "$left"
              "$right"
            ];

            pager = ":builtin";
          };
        };
      };

      home-manager.enable = true;

      nix-search-tv = {
        enable = true;
        settings.indexes = [
          "nixpkgs"
          "home-manager"
          "nixos"
        ];
      };

      ssh = {
        enable = true;
        enableDefaultConfig = false;

        settings =
          (lib.mapAttrs
            (name: host: {
              User = config.custom.username;
              HostName = host;
            })
            (
              {
                home = "home.paholg.com";
              }
              // lib.mapAttrs (_: host: host.address) (
                lib.getAttrs [ "box" "fractal" "frame-eth" "frame-wifi" ] (import ../router/hosts.nix)
              )
            )
          )
          // {
            router = {
              User = "admin";
              HostName = "10.0.0.1";
            };
            fractal-home = {
              User = "paho";
              HostName = "10.0.0.5";
              ProxyJump = "home";
            };
            "*" = {
              AddKeysToAgent = "yes";
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
