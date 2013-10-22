;;; go-config.el

(require 'ac-config)

(require-or-install 'go-mode)
(require-or-install 'go-autocomplete)
(require-or-install 'go-eldoc)

(add-hook 'go-mode-hook 'go-eldoc-setup)

(provide 'go-config)
