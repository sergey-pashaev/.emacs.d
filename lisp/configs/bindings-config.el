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

(global-unset-key (kbd "C-x C-c"))
(global-unset-key (kbd "C-x 5 0"))

(provide 'bindings-config)
