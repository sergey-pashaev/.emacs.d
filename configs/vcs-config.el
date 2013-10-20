;;; vcs-config.el

(require-or-install 'magit)

(global-set-key (kbd "C-x g") 'magit-status)

(provide 'vcs-config)
