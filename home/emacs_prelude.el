;;; .emacs --- Emacs beginning of init file
;;; Commentary:
;;; What more is there to say?
;;; Code:

;; -----------------------------------------------------------------------------
;; Config options

(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-unset-key (kbd "C-z"))

(blink-cursor-mode 0)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(global-subword-mode 1)

(setq-default fill-column 80
              indent-tabs-mode nil
              tab-width 2
              show-trailing-whitespace t
              major-mode 'text-mode
              whitespace-line-column 80
              whitespace-style '(face lines-tall))

(setq visible-bell t
      indent-line-function 'insert-tab
      line-number-mode t
      column-number-mode t)

(setq completions-format 'vertical)
(setq inhibit-startup-message t)

;; Because of daemon mode, all graphical settings must go here
(set-frame-font "FiraCode Nerd Font Mono-130" nil t)

(global-auto-revert-mode)
(add-hook 'prog-mode-hook #'whitespace-mode)
;; highlight parentheses
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

;; Was not working with use-package, so including directly
(add-to-list 'load-path "~/dotfiles/home/spacemacs-theme/")
(add-to-list 'custom-theme-load-path "~/dotfiles/home/spacemacs-theme/")
(load-theme 'spacemacs-dark t)

;;; emacs_prelude.el ends here
