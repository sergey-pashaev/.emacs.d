;; elisp settings

(defun psv/remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'psv/remove-elc-on-save)
(add-hook 'emacs-lisp-mode-hook (lambda()
                                  (setq mode-name "el")))

(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(bind-key "TAB" 'lisp-complete-symbol read-expression-map)
(bind-key "M-." 'find-function-at-point emacs-lisp-mode-map)

(provide 'lang-elisp)
