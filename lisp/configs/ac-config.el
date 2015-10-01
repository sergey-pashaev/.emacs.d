;;; ac-config.el

(require-or-install 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

(setq ac-auto-start nil)
(setq ac-auto-show-menu nil)
(setq ac-menu-height 20)
(setq ac-use-fuzzy t)
(setq ac-ignore-case 'smart)

(define-key ac-mode-map [(meta return)] 'auto-complete)

(setq ac-comphist-file (concat tempfiles-dir "ac-comphist.dat"))

(provide 'ac-config)
