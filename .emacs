(set-background-color "black")

;; -------------------------------------------------------------------------------------
;; Packages
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

(package-refresh-contents)
(package-initialize)

(setq use-package-always-ensure t)

(use-package color-theme
  :config
  (color-theme-initialize)
  (color-theme-hober))
(use-package relative-line-numbers
  :config
  (global-relative-line-numbers-mode t))
(use-package recentf
  :config
  (recentf-mode 1))
(use-package rust-mode)
(use-package cargo)

(use-package f)
(use-package company
  :bind
  ("C-<tab>" . company-indent-or-complete-common)
  :config
  (global-company-mode)
  (setq company-tooltip-align-annotations t))
(use-package company-ycmd
  :config
  (company-ycmd-setup))
(use-package ycmd
  :config
  (add-hook 'python-mode-hook 'ycmd-mode)
  (add-hook 'c-mode-hook 'ycmd-mode)
  (add-hook 'c++-mode-hook 'ycmd-mode)
  (set-variable 'ycmd-server-command '("python2" "/usr/share/vim/vimfiles/third_party/ycmd/ycmd"))
  (set-variable 'ycmd-global-config "~/.ycmd_settings.json"))

(use-package racer
  :init
  (setq racer-rust-src-path "/home/paho/.multirust/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")
  :config
  (add-hook 'rust-mode-hook #'racer-mode))

(use-package eldoc
 :config
 (add-hook 'ycmd-mode-hook #'eldoc-mode)
 (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package markdown-mode)
(use-package mmm-mode)
(use-package rainbow-mode)
(use-package toml-mode)
(use-package yaml-mode)
(use-package haskell-mode)
(use-package scss-mode)
(use-package tex-site
  :ensure auctex)

;; For jsx
(use-package rjsx-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))
  )

;; -------------------------------------------------------------------------------------
;; Config options
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
(global-unset-key (kbd "C-z"))

(blink-cursor-mode 0)
(setq visible-bell t)
(setq-default fill-column 99)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default show-trailing-whitespace t)
(setq-default major-mode 'text-mode)

(setq indent-line-function 'insert-tab)
(set-face-attribute 'default nil :height 95)

(setq line-number-mode t)
(setq column-number-mode t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

(set-frame-parameter (selected-frame) 'alpha '(85 85))
(add-to-list 'default-frame-alist '(alpha 85 85))
(global-auto-revert-mode)

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


;; -------------------------------------------------------------------------------------
;; Backup directory
(setq
 backup-by-copying t ; don't clobber symlinks
 backup-directory-alist '(("." . "~/.emacs.d/backups")) ; don't litter!
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

;; -------------------------------------------------------------------------------------
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

;; -------------------------------------------------------------------------------------
;; Other stuff

;; (require 'nix-mode)
;; (require 'tramp)
;; (add-to-list 'tramp-remote-path "/run/current-system/sw/bin")
