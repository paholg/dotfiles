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

    prelude = (builtins.readFile ./emacs_prelude.el);
    postlude = ''
      ;; lsp-ui tries to steal TAB. Rebinding it after all other package settings seems to work.
      (global-set-key (kbd "TAB") 'indent-for-tab-command)
    '';

    usePackage = {
      cargo = { enable = true; };

      color-theme-modern = { enable = true; };

      company = {
        enable = true;
        diminish = [ "company-mode" ];
        bind = { "C-<tab>" = "company-lsp"; };
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

      gnuplot = { enable = true; };

      go-mode = { enable = true; };

      haskell-mode = { enable = true; };

      helm = {
        enable = true;
        diminish = [ "helm-mode" ];
        bind = {
          "M-x" = "helm-M-x";
          "C-x r b" = "helm-filtered-bookmarks";
          "C-x C-f" = "helm-find-files";
        };
        config = ''
          (helm-mode 1)
          (setq helm-split-window-default-side 'other)
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

      latex = {
        enable = true;
        package = epkgs: epkgs.auctex;
        mode = [ ''("\\.tex\\'" . latex-mode)'' ];
      };

      linum = {
        enable = true;
        config = "(global-linum-mode)";
      };

      lsp-java = {
        enable = true;
        hook = [ "(java-mode . lsp)" ];
      };

      lsp-mode = {
        enable = true;
        command = [ "lsp" ];
        config = ''
          (define-key lsp-mode-map (kbd "C-o") lsp-command-map)
          (setq lsp-eldoc-enable-hover t
                lsp-eldoc-render-all nil
                lsp-idle-delay 0
                lsp-prefer-capf t
                company-minimum-prefix-length 1
                company-idle-delay 0
                gc-cons-threshold 104857600 ;; 100 MB
                read-process-output-max (* 1024 1024))
        '';
      };

      lsp-ui = {
        enable = true;
        command = [ "lsp-ui-mode" ];
        hook = [ "(lsp-ui-mode . lsp-ui-peek-mode)" ];
        bind = {
          "C-i" = "lsp-ui-doc-glance";
          "M-/" = "lsp-ui-peek-find-references";
        };
        config = ''
          (setq lsp-ui-doc-header t
                lsp-ui-doc-include-signature t
                lsp-ui-doc-enable nil
                lsp-ui-doc-position 'bottom
                lsp-ui-doc-alignment 'frame
                lsp-ui-peek-fontify 'always ;; highlight usage inside peek
                lsp-ui-sideline-enable t
                lsp-ui-sideline-show-diagnostics t
                lsp-ui-sideline-diagnostic-max-line-length 40
                lsp-ui-sideline-show-code-actions t
                lsp-ui-sideline-delay 0.0)
        '';
      };

      lua-mode = { enable = true; };

      markdown-mode = {
        enable = true;
        mode = [ ''"\\.mdwn\\'"'' ''"\\.markdown\\'"'' ''"\\.md\\'"'' ];
      };

      nix-mode = {
        enable = true;
        mode = [ ''"\\.nix\\'"'' ];
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

      rustic = { enable = true; };

      sqlformat = { enable = true; };

      spacemacs-theme = {
        enable = true;
        defer = true;
        init = ''
          (load-theme 'spacemacs-dark t)
        '';
      };

      toml-mode = { enable = true; };

      tramp = { enable = true; };

      yaml-mode = { enable = true; };
    };
  };
}
