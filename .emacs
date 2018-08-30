(set-background-color "black")

;; -----------------------------------------------------------------------------
;; Packages
(require 'package)
(setq package-archives '(
			 ;; ("gnu" . "https://elpa.gnu.org/packages/")
                         ;; ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-refresh-contents)
(package-initialize)

(setq use-package-always-ensure t)

(use-package auto-complete)
(use-package cargo)
(use-package coffee-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.cjsx$" . coffee-mode)))
(use-package color-theme
  :config
  (color-theme-initialize)
  (color-theme-hober))
;; (use-package column-marker
;;   :config
;;   (setq column-marker-1 100))
(use-package company
  :bind
  ("C-<tab>" . company-indent-or-complete-common)
  :config
  (global-company-mode)
  (setq company-tooltip-align-annotations t))
(use-package company-ycmd
  :config
  (company-ycmd-setup))
(use-package default-text-scale
  :config
  (default-text-scale-mode 1))
(use-package enh-ruby-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
  (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode)))
(use-package eldoc
  :config
  (add-hook 'ycmd-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))
(use-package f)
(use-package go-mode
  :config
  (defun go-save-hook ()
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook 'gofmt-before-save))
  (add-hook 'go-mode-hook 'go-save-hook)
  :bind
  ("M-." . godef-jump)
  ("M-*" . pop-tag-mark))
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
(use-package inf-ruby
  :config
  (add-hook 'after-init-hook 'inf-ruby-switch-setup))
(use-package json-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode)))
(use-package linum
  :config
  (global-linum-mode))
(use-package markdown-mode)
(use-package mmm-mode)
(use-package projectile
  :config
  (projectile-global-mode)
  (add-hook 'ruby-mode-hook 'projectile-mode)
  (add-hook 'enh-ruby-mode-hook 'projectile-mode)
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
(use-package racer
  :init
  (setq racer-rust-src-path "/home/paho/.multirust/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")
  :config
  (add-hook 'rust-mode-hook #'racer-mode))
(use-package rainbow-mode)
(use-package rjsx-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))
  :config
  (setq js-indent-level 2)
  (setq tab-width 2)
  (setq js2-basic-offset 2))
(use-package rbenv
  :config
  (global-rbenv-mode))
(use-package robe
  :config
  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'enh-ruby-mode-hook 'robe-mode)
  (eval-after-load 'company
    '(push 'company-robe company-backends))
  (add-hook 'robe-mode-hook 'ac-robe-setup))
(use-package rspec-mode
  :defer t
  :config
  (progn
    (defun rspec-ruby-mode-hook ()
      (tester-init-test-run #'rspec-run-single-file "_spec.rb$")
      (tester-init-test-suite-run #'rake-test))
    (add-hook 'enh-ruby-mode-hook 'rspec-ruby-mode-hook))
  (add-hook 'ruby-mode-hook 'rspec-mode))
(use-package rust-mode
  :init
  (setq rust-format-on-save t))
(use-package scss-mode)
(use-package tex-site
  :ensure auctex)
(use-package terraform-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.tf$" . terraform-mode)))
(use-package toml-mode)
(use-package typescript-mode)
(use-package yaml-mode)
(use-package ycmd
  :config
  (add-hook 'python-mode-hook 'ycmd-mode)
  (add-hook 'c-mode-hook 'ycmd-mode)
  (add-hook 'c++-mode-hook 'ycmd-mode)
  (set-variable 'ycmd-server-command
                '("python2" "/usr/share/vim/vimfiles/third_party/ycmd/ycmd"))
  (set-variable 'ycmd-global-config "~/.ycmd_settings.json"))

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
(add-to-list 'default-frame-alist '(font . "Hack-10"))

(setq line-number-mode t)
(setq column-number-mode t)

(setq completions-format 'vertical)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

(setq ruby-insert-encoding-magic-comment nil)
(setq enh-ruby-add-encoding-comment-on-save nil)

(set-frame-parameter (selected-frame) 'alpha '(85 85))
(add-to-list 'default-frame-alist '(alpha 85 85))
(global-auto-revert-mode)

(setq-default
 whitespace-line-column 100
 whitespace-style '(face lines-tall))
(add-hook 'prog-mode-hook #'whitespace-mode)

;; make emacs play nicely with system clipboard
(setq x-select-enable-clipboard t)

;; highlight parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)
(require 'paren)
(set-face-background 'show-paren-match-face "#333333")
(set-face-foreground 'show-paren-match-face "#ffffff")
(set-face-attribute 'show-paren-match-face nil :weight 'black)

;; show offscreen parenthesis in minibuffer
(defadvice show-paren-function
    (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
         (matching-text (and cb
                             (char-equal (char-syntax cb) ?\) )
                             (blink-matching-open))))
    (when matching-text (message matching-text))))

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

;; (require 'nix-mode)
;; (require 'tramp)
;; (add-to-list 'tramp-remote-path "/run/current-system/sw/bin")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (lua-mode rbenv yari yaml-mode use-package toml-mode terraform-mode scss-mode rspec-mode robe rjsx-mode rainbow-mode racer projectile-rails mmm-mode markdown-mode json-mode helm-projectile haskell-mode graphviz-dot-mode go-mode fill-column-indicator exec-path-from-shell enh-ruby-mode diminish default-text-scale company-ycmd column-marker column-enforce-mode color-theme coffee-mode cargo auto-complete auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
