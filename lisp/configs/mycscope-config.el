;;; mycscope-config.el

(require-or-install 'xcscope)
;(require-or-install 'ascope)
(cscope-setup)

(setq cscope-do-not-update-database t)

(provide 'mycscope-config)
