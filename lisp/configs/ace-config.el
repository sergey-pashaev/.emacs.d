;;; ace-config.el

(require-or-install 'ace-jump-mode)

(define-key global-map (kbd "C-;") 'ace-jump-word-mode)

(provide 'ace-config)
