;;; ace-config.el

(require-or-install 'ace-jump-mode)

(define-key global-map (kbd "C-;") 'ace-jump-mode)

(provide 'ace-config)
