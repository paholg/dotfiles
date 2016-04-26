(set-background-color "black")

;; -------------------------------------------------------------------------------------
;; Packages
(require 'package)
(add-to-list 'package-archives
    '("melpa" . "http://melpa.org/packages/") t)
(package-refresh-contents)
(package-initialize)

(use-package color-theme
  :config
  (color-theme-initialize)
  (color-theme-hober)
  :ensure t)
(use-package relative-line-numbers
  :config (global-relative-line-numbers-mode t)
  :ensure t)

(use-package rust-mode
  :ensure t)
(use-package cargo
  :ensure t)

(use-package ycmd
  :config
  (add-hook 'after-init-hook #'global-ycmd-mode)
  (set-variable 'ycmd-server-command '("python" )
  :ensure t)
(use-package company-ycmd
  :config
  (global-company-mode)
  (global-set-key (kbd "<C-tab>") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t)
  :ensure t)
(use-package flycheck-ycmd
  :ensure t)
;; (use-package racer
;;   :config
;;   (add-hook 'rust-mode-hook #'racer-mode)
;;   (add-hook 'racer-mode-hook #'eldoc-mode)
;;   :ensure t)

(use-package flycheck
  :ensure t)
(use-package markdown-mode
  :ensure t)
(use-package mmm-mode
  :ensure t)
(use-package rainbow-mode
  :ensure t)
(use-package toml
  :ensure t)
(use-package yaml-mode
  :ensure t)
(use-package haskell-mode
  :ensure t)

;; -------------------------------------------------------------------------------------
;; Config options
(global-set-key "\C-c;" 'comment-or-uncomment-region)

(blink-cursor-mode 0)
(setq visible-bell t)
(setq-default fill-column 99)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
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
(require 'tramp)
(add-to-list 'tramp-remote-path "/run/current-system/sw/bin")

;; make emacs remember recent files across sessions
(require 'recentf)
(recentf-mode 1)

;; make emacs play nicely with system clipboard
(setq x-select-enable-clipboard t)
