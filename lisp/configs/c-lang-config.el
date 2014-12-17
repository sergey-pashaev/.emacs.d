;;; c-lang-config.el

;; (require-or-install 'c-eldoc)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
;; (add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)

(setq c-default-style "k&r"
      c-basic-offset 4)

(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

(provide 'c-lang-config)
