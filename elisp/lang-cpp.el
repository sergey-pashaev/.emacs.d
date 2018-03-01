;;; cpp
;; common c settings
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defun psv/c-mode-hook ()
  (c-set-style "psv/cc-mode")
  (subword-mode 1)
  (bind-key (kbd "<C-tab>") 'ff-find-related-file c++-mode-map)
  (bind-key (kbd "<C-tab>") 'ff-find-related-file c-mode-map))

(add-hook 'c-mode-common-hook 'psv/c-mode-hook)
(add-hook 'c++-mode-hook 'psv/c-mode-hook)

(setq c-default-style "linux"
      c-basic-offset 4)

(setq gdb-many-windows t	     ; use gdb-many-windows by default
      gdb-show-main t)               ; non-nil means display source file containing the main routine at startup

(defconst psv/cc-style
  '("cc-mode"
    (c-offsets-alist . ((innamespace . [0])))))

(c-add-style "psv/cc-mode" psv/cc-style)


(use-package irony)
(use-package irony-eldoc)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-hook 'irony-mode-hook #'irony-eldoc)

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)

(use-package company
  :config
  (global-company-mode)
  :bind
  ("M-RET" . company-complete))

(use-package company-c-headers)
(use-package company-irony)
(use-package company-irony-c-headers)

(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

(use-package helm-company)

(use-package flycheck-irony)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(use-package rtags)

(use-package company-rtags)

(add-hook 'c-mode-hook 'rtags-start-process-unless-running)
(add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
(add-hook 'objc-mode-hook 'rtags-start-process-unless-running)

(setq rtags-completions-enabled t)
(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-rtags))
(setq rtags-autostart-diagnostics t)
(rtags-enable-standard-keybindings)

(use-package helm-rtags)
(setq rtags-display-result-backend 'helm)

(use-package clang-format
  :config
  (setq clang-format-style-option "Google")
  :bind
  ("C-M-|" . clang-format-region))

(defun clang-format-buffer-smart ()
  "Reformat buffer if .clang-format exists in the projectile root."
  (when (f-exists? (expand-file-name ".clang-format" (projectile-project-root)))
    (clang-format-buffer)))

(defun clang-format-buffer-smart-on-save ()
  "Add auto-save hook for clang-format-buffer-smart."
  (add-hook 'before-save-hook 'clang-format-buffer-smart nil t))

(add-hook 'c++-mode-hook 'clang-format-buffer-smart-on-save)
(add-hook 'c-mode-hook   'clang-format-buffer-smart-on-save)
(provide 'lang-cpp)
