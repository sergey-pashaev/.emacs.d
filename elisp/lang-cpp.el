;;; lang-cpp.el --- C/C++ language configuration

;;; Commentary:

;;; Code:

;; common c settings
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defconst psv/cc-style
  '("cc-mode"
    (c-offsets-alist . ((innamespace . [0])))
    (c-basic-offset . 2)
    (indent-tabs-mode . nil)))

(c-add-style "psv/cc-mode" psv/cc-style)

(setq c-default-style "linux"
      c-basic-offset 2)

(setq gdb-many-windows t ; use gdb-many-windows by default
      gdb-show-main t)   ; non-nil means display source file
                         ; containing the main routine at startup

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
         (not (string= (projectile-project-name) "src"))
         (not (string= (projectile-project-name) "cpptac"))
         (not (string= (projectile-project-name) "atr"))
         (f-exists? (expand-file-name ".clang-format" (projectile-project-root))))
    (clang-format-buffer)))

(defun psv/clang-format-buffer-smart-on-save ()
  "Add auto-save hook for clang-format-buffer-smart."
  (add-hook 'before-save-hook 'psv/clang-format-buffer-smart nil t))

(defvar psv/cc-search-directories
  '("../src/*" "../../src/*" "../../../src/*"
    "../include/*" "../../include/*" "../../../include/*")
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

(use-package lsp-mode
  :ensure t
  :diminish lsp-mode
  :commands lsp
  :config
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-prefer-flymake nil))

(defun psv/lsp-mode-hook ()
  "My lsp-mode hook."
  (require 'ccls)
  ;; don't enable lsp for extra browser repos
  (when (and
         (not (string= (projectile-project-name) "src"))
         (not (string= (projectile-project-name) "browser2"))
         (not (string= (projectile-project-name) "browser3"))
         (not (string= (projectile-project-name) "browser4")))
    (lsp)))

(use-package ccls
  :ensure t
  :hook
  ((c-mode c++-mode objc-mode) . psv/lsp-mode-hook))
(defun psv/lsp-ui-mode-hook ()
  "My lsp-ui-mode hook."
  (lsp-ui-flycheck-enable 1))

(use-package lsp-ui
  :ensure t
  :hook
  (lsp-mode . lsp-ui-mode)
  :config
  (require 'lsp-ui)
  (require 'lsp-ui-flycheck)
  (add-hook 'lsp-after-open-hook 'psv/lsp-ui-mode-hook)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-include-signature nil)
  (setq lsp-ui-sideline-show-symbol nil)
  (setq lsp-ui-flycheck-live-reporting nil)
  (setq lsp-ui-peek-list-width 60)
  (setq lsp-ui-peek-always-show t))

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
(bind-key "C-c i" 'helm-semantic-or-imenu lsp-ui-mode-map)
(bind-key "C-c f" 'lsp-format-region)

;; fix font for all unicode characters
(set-fontset-font t 'unicode "Symbola" nil 'prepend)

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol
  :bind
  ("C-c o" . helm-lsp-workspace-symbol))

(use-package helm-rg
  :ensure t)

(use-package projectile-ripgrep
  :ensure t)

;; use firefox
(setq browse-url-browser-function 'browse-url-firefox)
;; https://github.com/MaskRay/ccls/wiki/lsp-mode#cross-reference-extensions
(defun psv/ccls-peek-references ()
  "Peek references."
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"))

(defun psv/ccls-peek-callee ()
  "Peek callee's."
  (interactive)
  (lsp-ui-peek-find-custom "$ccls/call" '(:callee t)))

(defun psv/ccls-peek-caller ()
  "Peek callers."
  (interactive)
  (lsp-ui-peek-find-custom "$ccls/call"))

(defun psv/ccls-peek-references-address ()
  "Peek references where we take address."
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :role 128)))

(defun psv/ccls-peek-references-read ()
  "Peek references where we read read variable."
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
    (plist-put (lsp--text-document-position-params) :role 8)))

(defun psv/ccls-peek-references-write ()
  "Peek references where we write to variable."
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :role 16)))

(defun psv/ccls-peek-references-macro ()
  "Peek references of macro expansion."
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :role 64)))

(defun psv/ccls-peek-references-not-call ()
  "Peek non-call references."
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :excludeRole 32)))

(defun psv/ccls-peek-inheritance-base ()
  "Peek base of class."
  (interactive)
  (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels 1)))

(defun psv/ccls-peek-inheritance-derived ()
  "Peek derived classes."
  (interactive)
  (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels 1 :derived t))
  (psv/ccls/derived 1))

(defun psv/ccls-tree-inheritance-base ()
  "Show base tree of type."
  (interactive)
  (ccls-inheritance-hierarchy nil))

(defun psv/ccls-tree-inheritance-derived ()
  "Show derived tree of type."
  (interactive)
  (ccls-inheritance-hierarchy t))

(defun psv/ccls-tree-caller ()
  "Show tree of callers."
  (interactive)
  (ccls-call-hierarchy nil))

(defun psv/ccls-tree-callee ()
  "Show tree of callee's."
  (interactive)
  (ccls-call-hierarchy t))

(defun psv/ccls-peek-member ()
  "Peek members of type."
  (interactive)
  (lsp-ui-peek-find-custom "$ccls/member"))

(defun psv/ccls-peek-variables ()
  "Peek variables."
  (interactive)
  (lsp-ui-peek-find-custom "$ccls/vars"))

(use-package hydra
  :ensure t)

(defhydra psv/hydra-ccls (:hint t)
  "Where?"
  ("c" psv/ccls-peek-caller "peek caller")
  ("C" psv/ccls-tree-caller "tree caller")
  ("r" psv/ccls-peek-references-read "read")
  ("w" psv/ccls-peek-references-write "write")
  ("b" psv/ccls-peek-inheritance-base "peek base")
  ("B" psv/ccls-tree-inheritance-base "tree base")
  ("d" psv/ccls-peek-inheritance-derived "peek derived")
  ("D" psv/ccls-tree-inheritance-derived "tree derived")
  ("m" psv/ccls-peek-member "member")
  ("n" psv/ccls-references-not-call "not call")
  ("a" psv/ccls-peek-references "references")
  ("v" psv/ccls-peek-variables "variables"))

(bind-key "C-c w" 'psv/hydra-ccls/body lsp-ui-mode-map)

(provide 'lang-cpp)
;;; lang-cpp.el ends here
