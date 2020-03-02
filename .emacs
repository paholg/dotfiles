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
;; Use-package
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
(use-package elixir-mode)
(use-package enh-ruby-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
  (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode)))
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))
(use-package f)
(use-package go-mode
  :config
  (defun go-save-hook ()
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook 'gofmt-before-save))
  (add-hook 'go-mode-hook 'go-save-hook))
(use-package haskell-mode)
(use-package helm
  :config
  (helm-mode 1)
  (setq helm-split-window-default-side 'other)
  :bind
  ("M-x" . helm-M-x)
  ("C-x r b" . helm-filtered-bookmarks)
  ("C-x C-f" . helm-find-files))
(use-package helm-projectile
  :config
  (helm-projectile-on))
(use-package helm-ag
  :config
  (setq helm-ag-base-command "rg --vimgrep --no-heading --smart-case --hidden" )
  :bind
  ("C-c C-p" . helm-ag))
(use-package helm-swoop)
(use-package jsonnet-mode)
(use-package json-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode)))
(use-package linum
  :config
  (global-linum-mode))
(use-package sqlformat)

;; lsp things
(use-package lsp-mode
  :config (add-hook 'prog-mode-hook 'lsp)
  :commands lsp)
(use-package lua-mode)
;; (use-package flycheck
;;   :config (global-flycheck-mode))
;; (use-package flycheck-inline
;;   :config
;;   (with-eval-after-load 'flycheck
;;     (add-hook 'flycheck-mode-hook #'flycheck-inline-mode)))
;; (use-package flycheck-rust
;;   :config
;;   (with-eval-after-load 'rust-mode
;;     (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))
;; (use-package lsp-ui
;;   :hook ((lsp-mode . lsp-ui-mode))
;;   :commands lsp-ui-mode)
(use-package company-lsp
  :commands company-lsp)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package yasnippet)
;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language


(use-package markdown-mode)
(use-package projectile
  :config
  (projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  ;; (add-hook 'ruby-mode-hook 'projectile-mode)
  ;; (add-hook 'enh-ruby-mode-hook 'projectile-mode)
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
;; (use-package racer
;;   :init
;;   (setq racer-rust-src-path "/home/paho/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")
;;   :config
;;   (add-hook 'rust-mode-hook #'racer-mode))
(use-package rjsx-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))
  :config
  (setq js-indent-level 2)
  (setq tab-width 2)
  (setq js2-basic-offset 2))
(use-package rust-mode
  :config
  (setq rust-format-on-save t)
  )
(use-package scss-mode)
(use-package tex-site
  :ensure auctex)
(use-package terraform-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.tf$" . terraform-mode))
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))
(use-package toml-mode)
(use-package typescript-mode)
(use-package yaml-mode)
(use-package yari
  :bind
  ("M-n" . yari))

;; -----------------------------------------------------------------------------
;; Config options
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
(global-unset-key (kbd "C-z"))

(blink-cursor-mode 0)
(setq visible-bell t)
(setq-default fill-column 100)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default show-trailing-whitespace t)
(setq-default major-mode 'text-mode)

(setq indent-line-function 'insert-tab)

;; Because of daemon mode, all graphical settings must go here
(add-to-list 'default-frame-alist '(font . "Monospace-10"))

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

;; -----------------------------------------------------------------------------
;; Other stuff

;; Insert JIRA link
(defun jira-link()
  "Insert link to JIRA ticket at cursor"
  (interactive)
  (let ((action (read-string "action: "))
        (id (read-string "id: ")))
    (insert "[" action " " id "](https://outreach-io.atlassian.net/browse/"
            id")")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (hober)))
 '(custom-safe-themes
   (quote
    ("31772cd378fd8267d6427cec2d02d599eee14a1b60e9b2b894dd5487bd30978e" default)))
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-peek-enable nil)
 '(package-selected-packages
   (quote
    (helm-ag mustache-mode dockerfile-mode alchemist 0blayout elixir-mode company-tabnine jsonnet-mode stylus-mode lua-mode rbenv yari yaml-mode use-package toml-mode terraform-mode scss-mode rspec-mode robe rjsx-mode rainbow-mode racer projectile-rails mmm-mode markdown-mode json-mode helm-projectile haskell-mode graphviz-dot-mode go-mode fill-column-indicator exec-path-from-shell enh-ruby-mode diminish default-text-scale company-ycmd column-marker column-enforce-mode color-theme coffee-mode cargo auto-complete auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
