;;; tramp-config.el

(require-or-install 'tramp)

;; keep in mind known issues with zsh - see emacs wiki
(setq tramp-default-method "ssh")

(provide 'tramp-config)
