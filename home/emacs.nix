{ pkgs, ... }:

let
  nur-no-pkgs = import (builtins.fetchTarball
    "https://github.com/nix-community/NUR/archive/master.tar.gz") { };

in {
  imports = [ nur-no-pkgs.repos.rycee.hmModules.emacs-init ];

  services.emacs.enable = true;
  programs.emacs.enable = true;
  programs.emacs.init = {
    enable = true;
    recommendedGcSettings = true;
    earlyInit = "(setq package-enable-at-startup t)";

    prelude = (builtins.readFile ./emacs_prelude.el);
    postlude = (builtins.readFile ./emacs_postlude.el);

    usePackage = {
      cargo = { enable = true; };

      company = {
        enable = true;
        diminish = [ "company-mode" ];
        bind = { "C-<tab>" = "company-complete"; };
        hook = [ "(after-init . global-company-mode)" ];
      };

      # conf-unix-mode = {
      #   enable = true;
      #   mode = [
      #     ''"\\.service\\'"''
      #     ''"\\.timer\\'"''
      #     ''"\\.target\\'"''
      #     ''"\\.mount\\'"''
      #     ''"\\.automount\\'"''
      #     ''"\\.slice\\'"''
      #     ''"\\.socket\\'"''
      #     ''"\\.path\\'"''
      #   ];
      # };

      default-text-scale = {
        enable = true;
        config = "(default-text-scale-mode 1)";
      };

      direnv = {
        enable = true;
        config = "(direnv-mode)";
      };

      eldoc = {
        enable = true;
        config = ''
          (setq eldoc-idle-delay 0)
        '';
      };

      enh-ruby-mode = {
        enable = true;
        # interpreter = [
        #   "ruby"
        # ];
        mode = [ ''"\\.rb\\'"'' ];
      };

      f = { enable = true; };

      flycheck = {
        enable = true;
        diminish = [ "flycheck-mode" ];
        config = "(global-flycheck-mode)";
      };

      flycheck-rust = {
        enable = true;
        config = ''
          (with-eval-after-load 'rust-mode
            (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
        '';
      };

      gnuplot = { enable = true; };

      go-mode = { enable = true; };

      haskell-mode = { enable = true; };

      helm = {
        enable = true;
        bind = {
          "M-x" = "helm-M-x";
          "C-x r b" = "helm-filtered-bookmarks";
          "C-x C-f" = "helm-find-files";
        };
        config = ''
          (require 'helm-config)
          (setq helm-split-window-default-side 'other)
          (helm-mode 1)
        '';
      };

      helm-descbinds = {
        enable = true;
        bind = { "C-h b" = "helm-descbinds"; };
      };

      helm-lsp = {
        enable = true;
        command = [ "helm-lsp-workspace-symbol" ];
      };

      helm-projectile = {
        enable = true;
        diminish = [ "projectile-mode" ];
        config = "(helm-projectile-on)";
      };

      helm-rg = { enable = true; };

      helm-swoop = { enable = true; };

      # highlight-parentheses = {
      #   enable = true;
      # };

      jsonnet-mode = { enable = true; };

      json-mode = {
        enable = true;
        mode = [ ''"\\.json\\'"'' ];
      };

      # Auctex fails to install every update because the nixos version lags the
      # upstream version, and old versions are very quickly removed from elpa :(
      # latex = {
      #   enable = true;
      #   package = epkgs: epkgs.auctex;
      #   mode = [ ''("\\.tex\\'" . latex-mode)'' ];
      # };

      linum = {
        enable = true;
        config = "(global-linum-mode)";
      };

      lsp-java = { enable = true; };

      lsp-mode = {
        enable = true;
        command = [ "lsp" ];
        hook = [ "(prog-mode . lsp)" ];
        config = ''
          (define-key lsp-mode-map (kbd "C-o") lsp-command-map)
          (setq
            lsp-eldoc-enable-hover t
            lsp-eldoc-render-all nil
            lsp-idle-delay 0
            lsp-enable-snippet nil
            company-minimum-prefix-length 1
            company-idle-delay 0
            gc-cons-threshold 104857600 ;; 100 MB
            read-process-output-max (* 1024 1024)
          )'';
      };

      lsp-python-ms = {
        enable = true;
        init = ''
          (setq lsp-python-ms-executable (executable-find "python-language-server"))
        '';
      };

      lsp-ui = {
        enable = true;
        command = [ "lsp-ui-mode" ];
        hook =
          [ "(lsp-mode . lsp-ui-mode)" "(lsp-ui-mode . lsp-ui-peek-mode)" ];
        bind = {
          "C-i" = "lsp-ui-doc-glance";
          "M-/" = "lsp-ui-peek-find-references";
        };
        config = ''
          (setq
            lsp-ui-peek-fontify 'always ;; highlight usage inside peek
            lsp-ui-doc-enable nil
            lsp-ui-sideline-enable nil
          )'';
      };

      lua-mode = { enable = true; };

      markdown-mode = {
        enable = true;
        mode = [ ''"\\.mdwn\\'"'' ''"\\.markdown\\'"'' ''"\\.md\\'"'' ];
      };

      nix-mode = {
        enable = true;
        mode = [ ''"\\.nix\\'"'' ];
        bind = { "C-c C-f" = "nix-format-buffer"; };
      };

      projectile = {
        enable = true;
        bindKeyMap = { "C-c p" = "projectile-command-map"; };
        config = ''
          (projectile-mode)
          (setq projectile-indexing-method 'alien
                projectile-enable-caching t)
        '';
      };

      ripgrep = {
        enable = true;
        command = [ "ripgrep-regexp" ];
      };

      rust-mode = {
        enable = true;
        mode = [ ''"\\.rs\\'"'' ];
        bind = { "C-c C-c" = "rust-test"; };
        hook = [ "( rust-mode . lsp-rust-analyzer-inlay-hints-mode )" ];
        config = ''
          (setq
            lsp-rust-server 'rust-analyzer
            rust-format-on-save t
            rust-format-show-buffer nil
            rust-format-goto-problem nil
            lsp-rust-analyzer-server-display-inlay-hints t
            lsp-rust-analyzer-display-parameter-hints t
            lsp-rust-analyzer-display-chaining-hints t
          )
        '';
      };

      sqlformat = { enable = true; };

      # Temporarily disabled due to issues.
      # spacemacs-theme = {
      #   enable = true;
      #   defer = true;
      #   init = ''
      #     (load-theme 'spacemacs-dark t)
      #   '';
      # };

      toml-mode = { enable = true; };

      tramp = { enable = true; };

      yaml-mode = { enable = true; };
    };
  };
}
