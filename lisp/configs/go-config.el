;;; go-config.el

;; mysql driver
;;go get -u github.com/go-sql-driver/mysql

;; dev utils
;;go get -u github.com/dougm/goflymake
;;go get -u github.com/sergey-pashaev/goflymake
;;go get -u github.com/nsf/gocode
;;go get -u code.google.com/p/rog-go/exp/cmd/godef
;;go get -u github.com/sriram-srinivasan/gore
;;go get -u github.com/golang/lint/golint

(setenv "GOPATH" (concat (expand-file-name "~/workspace/go/")))

(require-or-install 'go-mode)
(require-or-install 'go-autocomplete)
(require-or-install 'go-eldoc)

(add-hook 'go-mode-hook 'go-eldoc-setup)

(require 'go-mode)
(require 'go-eldoc)
(require 'go-autocomplete)

(defun psv/go-local-bindings()
  (progn
    (setq tab-width 4)
    (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
    (local-set-key (kbd "C-c i") 'go-goto-imports)))

(defun psv/go-flycheck-turn-on()
  (flycheck-mode 1))

(add-hook 'go-mode-hook 'psv/go-local-bindings)
(add-hook 'go-mode-hook 'psv/go-flycheck-turn-on)

(provide 'go-config)
