;;; base-global-keys.el --- List of global keybindings

;;; Commentary:

;;; Code:
(require 'bind-key)

;; toggle menu-bar visibility
(bind-key "<f12>" 'menu-bar-mode)

(bind-key "C-\\" 'psv/toggle-russian-input-method)

(bind-key "C-x C-b" 'ibuffer)

;; quickly kill buffers
(bind-key "C-x k" 'kill-this-buffer)
(bind-key "<f8>" 'kill-this-buffer)

(bind-key "C-a" 'psv/beginning-of-line-dwim)

;; align your code in a pretty way
(bind-key "C-x \\" 'align-regexp)

;; duplicate the current line or region
(bind-key "C-c d" 'psv/duplicate-current-line-or-region)

;; toggle ws mode
(bind-key "C-<f6>" 'whitespace-mode)

;; toggle truncate lines mode
(bind-key "C-<f12>" 'toggle-truncate-lines)

(bind-key "C-<backspace>" 'backward-kill-word)
(bind-key "RET" 'newline-and-indent)
(bind-key "M-/" 'dabbrev-expand)
(bind-key "C-M-/" 'hippie-expand)
(bind-key "M-[" 'backward-paragraph)
(bind-key "M-]" 'forward-paragraph)

(bind-key "C-S-<up>" 'psv/move-line-up)
(bind-key "C-S-<down>" 'psv/move-line-down)

(bind-key "C-c +" 'psv/increment-number-decimal)
(bind-key "C-c -" 'psv/decrement-number-decimal)

(bind-key "M--" 'psv/goto-match-paren)
(bind-key "C-M-=" 'psv/diff-current-buffer-with-file)

(bind-key "<f5>" 'psv/work-todo)
(bind-key "<f6>" 'psv/todo)

;; unbound dangerous keys
(global-unset-key (kbd "C-x C-c"))

;; unbound "C-x 5 0 <-> O" typo
(global-unset-key (kbd "C-x 5 0"))

;; unbound suspend-frame function
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(bind-key "C-c >" 'psv/cycle-base-of-integer-at-point)

(bind-key "C-<return>" 'psv/newline-below)
(bind-key "C-S-<return>" 'psv/newline-above)

(provide 'base-global-keys)
;;; base-global-keys.el ends here
