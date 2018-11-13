;;; lang-cpp.el --- C/C++ language configuration

;;; Commentary:

;;; Code:

;; common c settings
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defconst psv/cc-style
  '("cc-mode"
    (c-offsets-alist . ((innamespace . [0])))
    (c-basic-offset . 4)
    (indent-tabs-mode . nil)))

(c-add-style "psv/cc-mode" psv/cc-style)

(setq c-default-style "linux"
      c-basic-offset 4)

(setq gdb-many-windows t ; use gdb-many-windows by default
      gdb-show-main t)   ; non-nil means display source file
                         ; containing the main routine at startup

(use-package irony)
(use-package irony-eldoc)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-hook 'irony-mode-hook #'irony-eldoc)

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

(defun psv/clang-format-buffer-smart ()
  "Reformat buffer if .clang-format exists in the projectile root."
  (when (and
         (not (string= (projectile-project-name) "-"))
         (f-exists? (expand-file-name ".clang-format" (projectile-project-root))))
    (clang-format-buffer)))

(defun psv/clang-format-buffer-smart-on-save ()
  "Add auto-save hook for clang-format-buffer-smart."
  (add-hook 'before-save-hook 'psv/clang-format-buffer-smart nil t))

(use-package flycheck-clang-tidy)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-clang-tidy-setup))

(defvar psv/cc-search-directories
  '("../src/*" "../../src/*" "../../../src/*" "../include/*" "../../include/*" "../../../include/*")
  "List of paths to search for other file (.h <-> .cpp).")

(defun psv/c-mode-hook ()
  "C/C++ mode hook."
  (c-set-style "psv/cc-mode")
  (subword-mode 1)
  (with-eval-after-load "find-file"
    (setq cc-search-directories (append cc-search-directories psv/cc-search-directories)))
  (bind-key (kbd "<C-tab>") 'ff-find-related-file c++-mode-map)
  (bind-key (kbd "<C-tab>") 'ff-find-related-file c-mode-map)
  (irony-mode 1)
  (rtags-start-process-unless-running)
  (psv/clang-format-buffer-smart-on-save))

(add-hook 'c-mode-hook 'psv/c-mode-hook)
(add-hook 'c++-mode-hook 'psv/c-mode-hook)

(provide 'lang-cpp)
;;; lang-cpp.el ends here
