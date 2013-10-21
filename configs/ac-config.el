;;; ac-config.el

(require-or-install 'auto-complete)
(require 'auto-complete-config)

(global-auto-complete-mode t)
(setq ac-auto-start 4)
(setq ac-ignore-case 'smart)
(setq ac-use-fuzzy t)

(setq ac-modes
      '(emacs-lisp-mode
        lisp-interaction-mode
        lisp-mode
        c-mode cc-mode c++-mode
        makefile-mode sh-mode
        org-mode
        go-mode))

(ac-config-default)

(provide 'ac-config)
