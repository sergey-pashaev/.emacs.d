;;; t480.el --- t480 specific configurations

;;; Commentary:

;;; Code:
(when window-system
  (set-frame-font "Liberation Mono 12" nil t))

;;; C++
(defun psv/lsp-mode-hook ()
  "My lsp-mode hook."
  (require 'ccls)
  (lsp))

(use-package ccls
  :ensure t
  :hook
  ((c-mode c++-mode objc-mode) . psv/lsp-mode-hook))

(use-package lsp-mode
  :ensure t
  :diminish lsp-mode
  :commands lsp
  :config
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-prefer-flymake nil))

(defun psv/lsp-ui-mode-hook ()
  "My lsp-ui-mode hooj."
  (lsp-ui-flycheck-enable 1))

(use-package lsp-ui
  :ensure t
  :hook
  (lsp-mode . lsp-ui-mode)
  :config
  (require 'lsp-ui-flycheck)
  (add-hook 'lsp-after-open-hook 'psv/lsp-ui-mode-hook)
  (setq lsp-ui-sideline-show-hover nil))

(use-package company-lsp
  :ensure t
  :commands company-lsp
  :after (:all lsp company)
  :config
  (push 'company-lsp company-backends))

(require 'lsp-ui)
(bind-key "C-c h" 'ccls-inheritance-hierarchy lsp-ui-mode-map)
(bind-key "C-c m" 'ccls-member-hierarchy lsp-ui-mode-map)
(bind-key "C-c c" 'ccls-call-hierarchy lsp-ui-mode-map)
(bind-key "C-c <" 'lsp-find-references lsp-ui-mode-map)
(bind-key "C-c r" 'lsp-rename lsp-ui-mode-map)

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol
  :bind
  ("C-c o" . helm-lsp-workspace-symbol))

(provide 't480)
;;; t480 ends here