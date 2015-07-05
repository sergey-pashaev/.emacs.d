;;; bindings-config.el

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; duplicate the current line or region
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; rename buffer & visited file
(global-set-key (kbd "C-c r") 'rename-file-and-buffer)

(global-set-key (kbd "<f8>") 'kill-this-buffer)
(global-set-key (kbd "C-<f6>") 'whitespace-mode)
(global-set-key (kbd "C-<f12>") 'toggle-truncate-lines)
(global-set-key (kbd "C-<backspace>") 'backward-kill-word)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(require-or-install 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; keyfreq stats
(require-or-install 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-/") 'dabbrev-expand)
(global-set-key (kbd "s-/") 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(global-unset-key (kbd "C-x C-c"))
(global-unset-key (kbd "C-x 5 0"))
;; unbound suspend-frame function
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(global-set-key (kbd "M--") 'psv/goto-match-paren)
(global-set-key (kbd "C-M-=") 'psv/diff-current-buffer-with-file)

(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)

(global-set-key (kbd "M-s o") 'occur)
(global-set-key (kbd "M-i") 'helm-imenu)
(global-set-key (kbd "s-`") 'smartscan-symbol-replace)

(provide 'bindings-config)
