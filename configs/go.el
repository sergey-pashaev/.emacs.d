;; golang setup

;; $ sudo apt-get install mercurial
;; $ mkdir ~/workspace/golang
;; $ cd ~/workspace/golang
;; $ hg clone -u release https://code.google.com/p/go
;; $ cd go/src
;; $ ./all.bash

;; add to .bashrc:
;; export PATH=~/golang/go/bin:"$PATH"

;; $ cd ~/workspace
;; $ mkdir go

;; export GOPATH=~/workspace/go
;; export PATH=$PATH:$GOPATH/bin

;; $ go get code.google.com/p/go.tools/cmd/godoc
;; $ go get code.google.com/p/go.tools/cmd/goimports
;; $ go get code.google.com/p/go.tools/cmd/oracle
;; $ go get -u code.google.com/p/rog-go/exp/cmd/godef
;; $ go get -u github.com/nsf/gocode

(setenv "GOPATH" (concat (expand-file-name "~/workspace/go/")))

(require-package 'go-mode)
(require-package 'go-autocomplete)
(require-package 'go-eldoc)

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

;; more dev utils
;; $ go get -u github.com/go-sql-driver/mysql
;; $ go get -u github.com/dougm/goflymake
;; $ go get -u github.com/sergey-pashaev/goflymake
;; $ go get -u github.com/sriram-srinivasan/gore
;; $ go get -u github.com/golang/lint/golint

;; more packages
;; (require-or-install 'golint)
;; (require-or-install 'go-flycheck)
