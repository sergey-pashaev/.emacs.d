;;; flycheck-config.el

(add-to-list 'load-path "~/workspace/go/src/github.com/sergey-pashaev/goflymake")
(require 'go-flycheck)

(require 'helm-flycheck)
(global-set-key (kbd "C-c !") 'helm-flycheck)

(provide 'flycheck-config)
