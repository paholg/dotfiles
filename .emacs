;; -------------------------------------------------------------------------------------
(setq visible-bell t) ;; disable beep
(global-linum-mode t) ;; display line numbers
(setq linum-format "%d ")
(global-set-key "\C-c;" 'comment-or-uncomment-region) ;; comment region
(blink-cursor-mode 0)

;; marmalade
(require 'package)
(add-to-list 'package-archives
    '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)


(add-to-list 'load-path "~/.emacs.d/plugins/*")

;; highlighting parens, changing color based on depth
;; (add-hook 'prog-mode-hook 'rainbow-delimeters-mode)


;; -------------------------------------------------------------------------------------
;; YouCompleteMe
;; (require 'ycmd)
;; (add-hook 'after-init-hook #'global-ycmd-mode)


;; -------------------------------------------------------------------------------------
;; Color systemd service files
(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.timer\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.target\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.mount\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.automount\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.slice\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.socket\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.path\\'" . conf-unix-mode))

;; (require 'nix-mode)
(require 'tramp)
(add-to-list 'tramp-remote-path "/run/current-system/sw/bin")

;; rust racer
;; (setq racer-rust-src-path "/home/paho/git/rust/src")
;; (eval-after-load "rust-mode" '(require 'racer))

;; theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; load auctex
(load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)


(setq-default fill-column 88)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default show-trailing-whitespace t)
(setq indent-line-function 'insert-tab)

(setq-default major-mode 'text-mode)

(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))
(add-to-list 'auto-mode-alist '("\\.sage\\'" . python-mode))


;; ;; highlight numbers - fixme: actually does nothing
;; (font-lock-add-keywords nil '(("\\_<[0-9]+\\_>" . font-lock-warning-face)))

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

;; transparancy
(set-frame-parameter (selected-frame) 'alpha '(85 85))
(add-to-list 'default-frame-alist '(alpha 85 85))
(global-auto-revert-mode)

;; Haskell Mode
(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; ;; Zsh Hook
;; (add-hook 'zsh-mode-hook
;;           (function (lambda ()
;; 											(setq indent-tabs-mode nil
;; 														tab-width 2))))

;; ;; Nix Hook?
;; (add-hook 'nix-mode-hook
;;           (function (lambda ()
;; 											(setq indent-tabs-mode nil
;; 														tab-width 2))))

;; make emacs remember recent files across sessions
(require 'recentf)
(recentf-mode 1)

;; change display settings
(set-face-attribute 'default nil :height 95) ;; font size
(setq line-number-mode t)
(setq column-number-mode t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; make emacs play nicely with system clipboard
(setq x-select-enable-clipboard t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (hober)))
 '(custom-safe-themes
   (quote
    ("31772cd378fd8267d6427cec2d02d599eee14a1b60e9b2b894dd5487bd30978e" "dbcd13340c37f5cf83c2301f399f98853524043a0cf06fb77e3bffc4023006b0" "7aed88a9449f87e2a7523330923180ebcf616e3ed5d03a2bf234721782480794" "83e8a51bfe125a6b77922bb006c2c271299c942784fdf973a03b102bb3f0ae0f" "2b5a6102b64578f0992539bdd0cdf4c1e1cbc8dcf8a8dc906e65451383fb6d9d" "e7855d995dfae8419ae937395d22f164b81cda808fb9decdd216eddbfe6177f7" "2b9896d7bec05dfa406f732cd87bafba081cd765229b62cf5bf7ca9dbe93aada" default)))
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
