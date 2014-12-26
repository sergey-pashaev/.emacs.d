;;; ac-config.el

(require-or-install 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

(setq-default ac-sources '(ac-source-semantic-raw))
(setq ac-auto-start nil)
(setq ac-auto-show-menu nil)
(setq ac-menu-height 20)

(define-key ac-mode-map [(meta return)] 'auto-complete)

(setq ac-comphist-file (concat tempfiles-dir "ac-comphist.dat"))
;;(define-key ac-mode-map [(meta return)] 'ac-complete)

;; (global-auto-complete-mode t)
;; (setq ac-auto-start 4)
;; (setq ac-ignore-case 'smart)
;; (setq ac-use-fuzzy t)

;; (setq ac-modes
;;       '(emacs-lisp-mode
;;         lisp-interaction-mode
;;         lisp-mode
;;         c-mode cc-mode c++-mode
;;         makefile-mode sh-mode
;;         org-mode
;;         go-mode))

;; (ac-config-default)

;; (require-or-install 'ac-c-headers)

;; (add-hook 'c-mode-hook
;;           (lambda ()
;;             (add-to-list 'ac-sources 'ac-source-c-headers)
;;             (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))

;; (add-hook 'c++-mode-hook
;;           (lambda ()
;;             (add-to-list 'ac-sources 'ac-source-c-headers)
;;             (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))

;; ;(setq ac-auto-show-menu nil)
;; (global-set-key (kbd "M-/") 'auto-complete)

(provide 'ac-config)
