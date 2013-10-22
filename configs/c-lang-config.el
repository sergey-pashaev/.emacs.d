;;; c-lang-config.el

(require-or-install 'c-eldoc)

(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

(provide 'c-lang-config)
