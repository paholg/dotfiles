{ ... }:

{
  imports = [ ./helix.nix ./packages.nix ./starship.nix ];

  home.stateVersion = "20.09";

  fonts.fontconfig.enable = true;

  home = {
    sessionVariables = {
      EDITOR = "hx";
      RUST_NEW_ERROR_FORMAT = "true";
      CARGO_HOME = "$HOME/.cargo";
      MANPAGER = "sh -c 'col -bx | bat -l man -p'";
      CARGO_TARGET_DIR = "$HOME/.cargo/cache";
    };
  };

  home.file = {
    ".cargo/config.toml".text = ''
      [registries.crates-io]
      protocol = "sparse"

      [target.x86_64-unknown-linux-gnu]
      linker = "clang"
      rustflags = ["-C", "link-arg=-fuse-ld=mold"]
    '';

    ".config/helix/themes/paho-theme.toml".text =
      builtins.readFile ./paho-theme.toml;

    ".config/nix/nix.conf".text = ''
      experimental-features = nix-command flakes

      substituters = https://cache.nixos.org https://nix-community.cachix.org
      trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=
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

      if `command -v rustc >/dev/null 2>&1`; then
          export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
      fi

      path_dirs="
        $HOME/.cargo/bin
        $HOME/dotfiles/bin
        $HOME/bin
        $HOME/go/bin
      "

      for dir in $(echo $path_dirs); do
        export PATH=$dir:$PATH
      done
    '';

    ".taplo.toml".text = ''
      [formatting]
      align_comments = false
      array_auto_collapse = false
      array_auto_expand = false
      reorder_keys = false
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

  nixpkgs.config = import ./nixpkgs-config.nix;
  xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs-config.nix;

  programs = {
    atuin.enable = true;
    direnv.enable = true;
    git = {
      enable = true;
      userName = "Paho Lurie-Gregg";
      delta = {
        enable = true;
        options = { line-numbers = true; };
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
      path = "...";
    };

    nix-index = {
      enable = true;
      enableZshIntegration = true;
    };

    ssh = { enable = true; };

    tmux = {
      enable = true;
      newSession = true;
      terminal = "xterm-256color";
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
        cb = ''cargo build --color always 2>&1 | less -R'';
        cc = ''cargo check --color always 2>&1 | less -R'';
        ct = ''cargo test --color always 2>&1 | less -R'';
        cw = ''cargo watch -s "cargo check --colow always 2>&1 | less -R"'';

        hx = "CARGO_TARGET_DIR=~/.cargo/cache2 hx";

        ls = "exa";
        la = "ls -la";
        ll = "ls -l";

        em = "emacsclient -c";
        emt = "emacsclient -t";

        g = "git";
        gbt = "git bt | head -n10";
        gsw = ''
          git switch $(git branch --sort=-committerdate | fzf | cut -c3- | cut -d " " -f1)'';

        hx-install = ''
          pushd ~/git/helix && \
          git pull && \
          nix develop -c "cargo install --path helix-term" && \
          hx --grammar fetch && \
          hx --grammar build && \
          ln -s $PWD/runtime ~/.config/helix/runtime; \
          popd'';

        ipinfo = "curl ipinfo.io 2> /dev/null | jq .";

        own = "fd --no-ignore-vcs -Ho root | xargs -d'\n' sudo chown -h paho:paho";

        sudop = "sudo env PATH=$PATH";

        t = "tmux attach";
      };
      initExtra = (builtins.readFile ./zsh_extra.sh);
    };
  };
}
