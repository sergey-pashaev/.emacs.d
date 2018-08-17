;; Add your keys here, as such

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
(bind-key "C-c d" 'duplicate-current-line-or-region)

;; toggle ws mode
(bind-key "C-<f6>" 'whitespace-mode)

;; toggle truncate lines mode
(bind-key "C-<f12>" 'toggle-truncate-lines)

(bind-key "C-<backspace>" 'backward-kill-word)
(bind-key "RET" 'newline-and-indent)
(bind-key "M-/" 'dabbrev-expand)
(bind-key "M-[" 'backward-paragraph)
(bind-key "M-]" 'forward-paragraph)

(bind-key "C-S-<up>" 'move-line-up)
(bind-key "C-S-<down>" 'move-line-down)

(bind-key "C-c +" 'psv/increment-number-decimal)
(bind-key "C-c -" 'psv/decrement-number-decimal)

(bind-key "S-RET" 'smart-open-line)
(bind-key "C-S-RET" 'smart-open-line-above)

(bind-key "M--" 'psv/goto-match-paren)
(bind-key "C-M-=" 'psv/diff-current-buffer-with-file)

(bind-key "C-x f" 'psv/recentf-ido-find-file)

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

(bind-key "C-c ?" 'helm-dash-at-point)

(provide 'base-global-keys)
