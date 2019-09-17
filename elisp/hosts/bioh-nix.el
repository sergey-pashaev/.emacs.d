;;; bioh-nix.el --- bioh-nix specific configurations

;;; Commentary:

;;; Code:
(defconst psv/cc-style
  '("cc-mode"
    (c-offsets-alist . ((innamespace . [0])))
    (c-basic-offset . 2)
    (indent-tabs-mode . nil)))

(c-add-style "psv/cc-mode" psv/cc-style)

(setq c-default-style "linux"
      c-basic-offset 2)

(when window-system
  (set-frame-font "Liberation Mono 12" nil t))

(when window-system
  (use-package solarized-theme
    :ensure t
    :config
    (load-theme 'solarized-light t)))

;;; C++
(defun psv/lsp-mode-hook ()
  "My lsp-mode hook."
  (require 'ccls)
  (lsp)
  )

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
(bind-key "C-c i" 'lsp-ui-imenu lsp-ui-mode-map)

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol
  :bind
  ("C-c o" . helm-lsp-workspace-symbol))

(use-package helm-rg
  :ensure t)

(use-package projectile-ripgrep
  :ensure t)

;; eglot

;; (defun psv/eglot-mode-hook ()
;;   "My eglot-mode hook."
;;   (eglot-ensure))

;; (use-package eglot
;;   :ensure t
;;   :config
;;   (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd-8"))
;;   :hook
;;   ((c-mode c++-mode) . psv/eglot-mode-hook))


;; fix font for all unicode characters
(set-fontset-font t 'unicode "Symbola" nil 'prepend)

;; use firefox
(setq browse-url-browser-function 'browse-url-firefox)

(provide 'bioh-nix)
;;; bioh-nix ends here
