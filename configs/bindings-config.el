;;; bindings-config.el

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; If you want to be able to M-x without meta
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; duplicate the current line or region
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; rename buffer & visited file
(global-set-key (kbd "C-c r") 'rename-file-and-buffer)

(global-set-key (kbd "<f8>") 'kill-this-buffer)
(global-set-key (kbd "C-<f6>") 'whitespace-mode)
(global-set-key (kbd "C-<f12>") 'toggle-truncate-lines)
(global-set-key (kbd "C-<backspace>") 'backward-kill-word)
;(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'bindings-config)
