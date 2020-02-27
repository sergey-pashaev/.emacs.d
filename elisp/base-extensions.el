;;; base-extensions.el --- Base extenstions config file

;;; Commentary:

;;; Code:

(use-package no-littering :ensure t)

(use-package avy
  :ensure t
  :bind
  ("C-;" . avy-goto-word-1))

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

;; (use-package flycheck
;;   :ensure t
;;   :config
;;   (setq flycheck-check-syntax-automatically '(mode-enabled save))
;;   (setq flycheck-mode-line-prefix " fc")
;;   (global-flycheck-mode))

(use-package helm
  :ensure t
  :diminish
  :init
  (require 'helm-config)
  :config
  (setq helm-quick-update               t   ; do not display invisible
                                            ; candidates

        helm-split-window-inside-p    t     ; open helm buffer inside
                                            ; current window, not
                                            ; occupy whole other
                                            ; window

        helm-buffers-fuzzy-matching   t     ; fuzzy matching buffer
                                            ; names when non--nil

        helm-move-to-line-cycle-in-source t ; move to end or
                                            ; beginning of source
                                            ; when reaching top or
                                            ; bottom of source.

        helm-ff-search-library-in-sexp    t ; search for library in
                                            ; `require' and
                                            ; `declare-function' sexp.

        helm-ff-file-name-history-use-recentf t
        helm-split-window-default-side        'below
        helm-idle-delay                       0.0
        helm-input-idle-delay                 0.01
        helm-ff-skip-boring-files             t)
  (helm-mode 1)
  :bind (
         ("M-x" . helm-M-x)
         ("C-x f" . helm-recentf)))

(use-package projectile
  :ensure t
  :config
  (setq projectile-enable-caching t
        projectile-mode-line-prefix " p")
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode))

(use-package helm-projectile
  :ensure t)

(helm-projectile-on)

(use-package magit
  :ensure t
  :config
  (setq magit-refresh-status-buffer nil)
  (setq magit-refresh-verbose t)

  (setq magit-diff-refine-hunk 'all)

  ; disable vc for git
  (setq vc-handled-backends (delq 'Git vc-handled-backends))
  :bind
  ("C-x g" . magit-status)
  ("C-x =" . magit-diff-buffer-file))

(use-package recentf
  :ensure t
  :config
  (setq recentf-max-saved-items 1000
        recentf-max-menu-items 100)
  (recentf-mode 1))

;; todo: check why use-package doesn't work
(require 'windmove)
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (use-package yasnippet-snippets :ensure t)
  (yas-global-mode 1)
  (yas-reload-all))

(use-package markdown-mode
  :ensure t
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc"))

(use-package google-translate
  :ensure t
  :init
  (progn
    (require 'google-translate-default-ui)
    (setq google-translate-output-destination 'echo-area)
    (setq google-translate-default-source-language "auto")
    (setq google-translate-default-target-language "ru"))
  :bind
  ("C-c t" . google-translate-at-point))

(use-package dockerfile-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package cmake-mode :ensure t)
(use-package wgrep :ensure t)

(use-package git-timemachine :ensure t)

(use-package company
  :ensure t
  :diminish " ca"
  :config
  (global-company-mode)
  :bind
  ("M-RET" . company-complete))

(use-package rmsbolt
  :ensure t)

(use-package shell-pop
  :ensure t
  :bind (("C-`" . shell-pop))
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/bin/bash")
  (setq shell-pop-full-span t)
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

(when window-system
  (set-frame-font "Liberation Mono 12" nil t))

(when window-system
  (use-package solarized-theme
    :ensure t
    :config
    (load-theme 'solarized-light t)))


(use-package git-ps1-mode
  :ensure t
  :config
  (git-ps1-mode +1))

(use-package f :ensure t)

(use-package hydra :ensure t)

(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  ("C-s" . swiper-isearch))

(provide 'base-extensions)
;;; base-extensions.el ends here
