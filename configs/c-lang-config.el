;;; c-lang-config.el

(require-or-install 'c-eldoc)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
(add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)

(setq c-default-style "k&r"
      c-basic-offset 4)

(provide 'c-lang-config)
