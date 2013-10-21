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

(require-or-install 'ac-c-headers)

(add-hook 'c-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-c-headers)
            (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))

(provide 'ac-config)
