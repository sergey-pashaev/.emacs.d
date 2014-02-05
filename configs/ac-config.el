;;; ac-config.el

(require-or-install 'auto-complete)
(require 'auto-complete-config)

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

(require-or-install 'ac-c-headers)

(require 'auto-complete-clang-async)

(defun ac-cc-mode-setup ()
  (setq ac-clang-complete-executable "~/github/emacs-clang-complete-async/clang-complete")
  (setq ac-sources '(ac-source-clang-async))
  (ac-clang-launch-completion-process)
)

(defun my-ac-config ()
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

(ac-cc-mode-setup)
(my-ac-config)

;; (add-hook 'c-mode-hook
;;           (lambda ()
;;             (add-to-list 'ac-sources 'ac-source-c-headers)
;;             (add-to-list 'ac-sources 'ac-source-c-header-symbols t)
;;             (add-to-list 'ac-sources 'ac-source-clang-async)
;;             (ac-clang-launch-completion-process)))

;; (add-hook 'c++-mode-hook
;;           (lambda ()
;;             (add-to-list 'ac-sources 'ac-source-c-headers)
;;             (add-to-list 'ac-sources 'ac-source-c-header-symbols t)
;;             (add-to-list 'ac-sources 'ac-source-clang-async)
;;             (ac-clang-launch-completion-process)))

(provide 'ac-config)
