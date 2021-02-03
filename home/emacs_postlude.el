;;; .emacs --- Emacs end of init file
;;; Commentary:
;;; What more is there to say?
;;; Code:

;; lsp-ui tries to steal TAB. Rebinding it after all other package settings seems to work.
(global-set-key (kbd "TAB") 'indent-for-tab-command)

;; -----------------------------------------------------------------------------
;; Per-host settings
(let ((host (substring (shell-command-to-string "hostname") 0 -1)))
  (let ((host_file ( concat "~/dotfiles/" host "/emacs.el")))
    (if (file-exists-p host_file) (load host_file))))

;;; emacs_postlude.el ends here
