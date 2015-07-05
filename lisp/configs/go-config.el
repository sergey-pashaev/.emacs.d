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
    (add-hook 'before-save-hook 'gofmt-before-save)
    (local-set-key (kbd "M-.") 'godef-jump)
    (local-set-key (kbd "M-,") 'pop-tag-mark)
    (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
    (local-set-key (kbd "C-c i") 'go-goto-imports)))

(add-hook 'go-mode-hook 'psv/go-local-bindings)

(provide 'go-config)
