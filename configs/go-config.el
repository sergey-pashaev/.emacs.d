;;; go-config.el

;; mysql driver
;;go get -u github.com/go-sql-driver/mysql

;; dev utils
;;go get -u github.com/dougm/goflymake
;;go get -u github.com/nsf/gocode
;;go get -u code.google.com/p/rog-go/exp/cmd/godef
;;go get -u github.com/sriram-srinivasan/gore

(require 'ac-config)

(require-or-install 'go-mode)
(require-or-install 'go-autocomplete)
(require-or-install 'go-eldoc)

(add-hook 'go-mode-hook 'go-eldoc-setup)

(defun my-go-local-bindings()
  (progn
    (setq tab-width 4)
    (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
    (local-set-key (kbd "C-c i") 'go-goto-imports)))

(defun go-flycheck-turn-on()
  (flycheck-mode 1))

(add-hook 'go-mode-hook 'my-go-local-bindings)
(add-hook 'go-mode-hook 'go-flycheck-turn-on)

;; lurk more about imenu

(provide 'go-config)
