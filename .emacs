;; -----------------------------------------------------------------------------
;; Package setup
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap 'use-package'
(eval-after-load 'gnutls
  '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(setq use-package-always-ensure t)

;; -----------------------------------------------------------------------------
;; Per-host settings
(let ((host (substring (shell-command-to-string "hostname") 0 -1)))
  (let ((host_file ( concat "~/dotfiles/" host "/emacs.el")))
    (if (file-exists-p host_file) (load host_file))))

;; -----------------------------------------------------------------------------
;; Use-package
(use-package spacemacs-theme
  :defer t
  :init
  (load-theme 'spacemacs-dark t))
(use-package auto-package-update
  :init (auto-package-update-at-time "20:00"))
(use-package color-theme-modern)
(use-package cargo)
(use-package company
  :bind
  ("C-<tab>" . company-lsp)
  :config
  (global-company-mode)
  (setq company-tooltip-align-annotations t))
(use-package default-text-scale
  :config
  (default-text-scale-mode 1))
(use-package eldoc
  :config
  (setq eldoc-idle-delay 0)
  (setq eldoc-echo-area-use-multiline-p nil)
)
(use-package elixir-mode)
(use-package enh-ruby-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
  (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode)))
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))
(use-package expand-region
  :bind ("C-=" . er/expand-region))
(use-package f)
(use-package go-mode
  :config
  (defun go-save-hook ()
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook 'gofmt-before-save))
  (add-hook 'go-mode-hook 'go-save-hook))
(use-package gnuplot)
(use-package haskell-mode)
(use-package helm
  :config
  (helm-mode 1)
  (setq helm-split-window-default-side 'other)
  :bind
  ("M-x" . helm-M-x)
  ("C-x r b" . helm-filtered-bookmarks)
  ("C-x C-f" . helm-find-files))
(use-package helm-descbinds
  :bind ("C-h b" . helm-descbinds))
(use-package helm-projectile
  :config
  (helm-projectile-on))
(use-package helm-rg)
(use-package helm-swoop)
(use-package jsonnet-mode)
(use-package json-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode)))
(use-package linum
  :config
  (global-linum-mode))
(use-package nix-mode
  :mode "\\.nix\\'")
(use-package sqlformat)

;; lsp things
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-o")
  :hook
  (rust-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  :config

  (setq lsp-eldoc-enable-hover t)
  (setq lsp-eldoc-render-all nil)
  (setq lsp-idle-delay 0)
  (setq lsp-prefer-capf t)

  (setq company-minimum-prefix-length 1
        company-idle-delay 0)
  (setq gc-cons-threshold 104857600) ;; 100 MB
  (setq read-process-output-max (* 1024 1024))

  :commands lsp)
(use-package lsp-ui
  :hook (lsp-ui-mode . lsp-ui-peek-mode)
  :bind
  ("C-i" . lsp-ui-doc-glance)
  ("M-/" . lsp-ui-peek-find-references)
  :config
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-position 'bottom)
  (setq lsp-ui-doc-alignment 'frame)

  (setq lsp-ui-peek-fontify 'always) ;; highlight usage inside peek

  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-diagnostic-max-line-length 40)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-delay 0.0)
  :commands lsp-ui-mode
  )
(use-package lsp-java
  :hook (java-mode . lsp))
(use-package lua-mode)
(use-package flycheck
  :config (global-flycheck-mode))
(use-package flycheck-rust
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package yasnippet
  :config (yas-global-mode 1))
(use-package markdown-mode)
(use-package projectile
  :config
  (projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t))
(use-package projectile-rails
  :config
  (projectile-rails-global-mode)
  (add-hook 'ruby-mode-hook 'projectile-mode)
  (add-hook 'enh-ruby-mode-hook 'projectile-mode))
(use-package recentf
  :config
  (recentf-mode 1))
(use-package robe
  :config
  (add-hook 'enh-ruby-mode-hook 'robe-mode))
(use-package rjsx-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))
  :config
  (setq js-indent-level 2)
  (setq tab-width 2)
  (setq js2-basic-offset 2))
(use-package rust-mode
  :config
  (setq lsp-rust-server 'rust-analyzer)
  (setq rust-format-on-save t)
  (setq rust-format-show-buffer nil)
  (setq rust-format-goto-problem nil))
(use-package scss-mode)
(use-package tex-site
  :ensure auctex)
(use-package terraform-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.tf$" . terraform-mode))
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))
(use-package toml-mode)
(use-package typescript-mode)
(use-package which-key
  :config (which-key-mode))
(use-package yaml-mode)
(use-package yari
  :bind
  ("M-n" . yari))

;; -----------------------------------------------------------------------------
;; Config options
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-unset-key (kbd "C-z"))
;; lsp-ui tries to steal TAB. Rebinding it after all other package settings seems to work.
(global-set-key (kbd "TAB") 'indent-for-tab-command)

(blink-cursor-mode 0)
(setq visible-bell t)
(setq-default fill-column 100)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default show-trailing-whitespace t)
(setq-default major-mode 'text-mode)

(setq indent-line-function 'insert-tab)

;; Because of daemon mode, all graphical settings must go here
(add-to-list 'default-frame-alist '(font . "Monospace-13"))

(setq line-number-mode t)
(setq column-number-mode t)

(setq completions-format 'vertical)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

(setq ruby-insert-encoding-magic-comment nil)
(setq enh-ruby-add-encoding-comment-on-save nil)

(global-auto-revert-mode)

(setq-default
 whitespace-line-column 100
 whitespace-style '(face lines-tall))
(add-hook 'prog-mode-hook #'whitespace-mode)

;; make emacs play nicely with system clipboard
(setq x-select-enable-clipboard t)

;; highlight parentheses
(setq show-paren-delay 0)
(show-paren-mode 1)

;; -----------------------------------------------------------------------------
;; Backup directory
(setq
 backup-by-copying t ; don't clobber symlinks
 backup-directory-alist '(("." . "~/.emacs.d/backups")) ; don't litter!
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

;; -----------------------------------------------------------------------------
;; File modes

;; systemd
(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.timer\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.target\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.mount\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.automount\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.slice\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.socket\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.path\\'" . conf-unix-mode))

;; markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Python - scons and sage
(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))
(add-to-list 'auto-mode-alist '("\\.sage\\'" . python-mode))

