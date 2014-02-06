;;; go-config.el

(require 'ac-config)

(require-or-install 'go-mode)
(require-or-install 'go-autocomplete)
(require-or-install 'go-eldoc)

(add-hook 'go-mode-hook 'go-eldoc-setup)

(defun my-go-local-bindings()
  (progn
    (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
    (local-set-key (kbd "C-c i") 'go-goto-imports)))

(defun go-flycheck-turn-on()
  (flycheck-mode 1))

(add-hook 'go-mode-hook 'my-go-local-bindings)
(add-hook 'go-mode-hook 'go-flycheck-turn-on)

;; lurk more about imenu

(provide 'go-config)
