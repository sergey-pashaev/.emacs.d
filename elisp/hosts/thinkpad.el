;;; thinkpad.el --- thinkpad-specific configurations

;;; Commentary:

;;; Code:
(when window-system
  (set-frame-font "Liberation Mono 12" nil t))

;;; C++
(use-package lsp-mode :ensure t :commands lsp)
(use-package lsp-ui :ensure t :commands lsp-ui-mode)
(use-package company-lsp :ensure :commands company-lsp)

(require 'company-lsp)
(push 'company-lsp company-backends)

(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))

(use-package helm-lsp :ensure t)

(provide 'thinkpad)
;;; thinkpad ends here
