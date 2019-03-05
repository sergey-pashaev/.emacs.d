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

(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-idle-delay 1)
  :bind
  ("M-RET" . company-complete))

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode))

(use-package company-c-headers :ensure t)
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers)))

(use-package rtags
  :ensure t
  :config
  (progn
    (setq rtags-completions-enabled t)
    (setq rtags-autostart-diagnostics t)
    (rtags-enable-standard-keybindings)
    (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
    (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)))

(use-package company-rtags
  :ensure t
  :config
  (eval-after-load 'company
    '(add-to-list
      'company-backends 'company-rtags)))

(use-package flycheck-rtags :ensure t)

(use-package helm-rtags
  :ensure t
  :config
  (setq rtags-display-result-backend 'helm))

(use-package clang-format
  :ensure t
  :config
  (setq clang-format-style-option "Google")
  :bind
  ("C-M-|" . clang-format-region))

(defun psv/clang-format-buffer-smart ()
  "Reformat buffer if .clang-format exists in the projectile root."
  (when (and
         (not (string= (projectile-project-name) "-"))
         (not (string= (projectile-project-name) "cpptac"))
         (not (string= (projectile-project-name) "atr"))
         (f-exists? (expand-file-name ".clang-format" (projectile-project-root))))
    (clang-format-buffer)))

(defun psv/clang-format-buffer-smart-on-save ()
  "Add auto-save hook for clang-format-buffer-smart."
  (add-hook 'before-save-hook 'psv/clang-format-buffer-smart nil t))

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
  (psv/clang-format-buffer-smart-on-save))

(add-hook 'c-mode-hook 'psv/c-mode-hook)
(add-hook 'c++-mode-hook 'psv/c-mode-hook)

(provide 'lang-cpp)
;;; lang-cpp.el ends here
