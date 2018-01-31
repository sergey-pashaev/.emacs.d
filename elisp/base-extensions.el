;; basic extensions

(use-package no-littering)

;; saveplace: save location in file when saving files
(setq save-place-file (concat temp-dir "/saveplace"))
(setq-default save-place t)
(require 'saveplace)

;; tramp
(require 'tramp)
(setq tramp-default-method "ssh")

;; meaningful names for buffers with the same name
(require 'uniquify)
(progn
  (setq uniquify-buffer-name-style 'forward
	uniquify-separator "/"
	;; rename after killing uniquified
	uniquify-after-kill-buffer-p t
	;; don't muck with special buffers
	uniquify-ignore-buffers-re "^\\*"))

(use-package avy
  :bind
  ("C-;" . avy-goto-word-1))

;; (use-package ace-jump-mode
;;   :bind
;;   ("C-c SPC" . ace-jump-mode))

(use-package ag
  :config
  (setq ag-highlight-search t
	ag-reuse-buffers    t))

;; (use-package company
;;   :config
;;   (add-hook 'after-init-hook 'global-company-mode))

;; (use-package dashboard
;;   :config
;;   (dashboard-setup-startup-hook))

;; (use-package ediff
;;   :config
;;   (setq ediff-window-setup-function 'ediff-setup-windows-plain)
;;   (setq-default ediff-highlight-all-diffs 'nil)
;;   (setq ediff-diff-options "-w"))

;; (use-package exec-path-from-shell
;;   :config
;;   ;; Add GOPATH to shell
;;   (when (memq window-system '(mac ns))
;;     (exec-path-from-shell-copy-env "GOPATH")
;;     (exec-path-from-shell-copy-env "PYTHONPATH")
;;     (exec-path-from-shell-initialize)))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

;; (use-package flycheck)

;; todo: fix this
;; (when (executable-find "curl")
;;   (setq helm-google-suggest-use-curl-p t))

(use-package helm
  :init
  (require 'helm-config)
  :config
  (setq helm-quick-update                     t ; do not display invisible candidates
	helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
	helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
	helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
	helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
	helm-ff-file-name-history-use-recentf t
        helm-split-window-default-side        'below
	helm-idle-delay                       0.0
	helm-input-idle-delay                 0.01
	helm-ff-skip-boring-files             t)
  (helm-mode 1)
  :bind (("M-x" . helm-M-x)))
         ;; ("C-x C-m" . helm-M-x)
         ;; ("C-x C-f" . helm-find-files)
         ;; ("C-x v" . helm-projectile)
         ;; ("C-x c o" . helm-occur)
         ;; ("C-x c p" . helm-projectile-ag)
         ;; ("C-x c k" . helm-show-kill-ring)
         ;; :map helm-map
         ;; ("<tab>" . helm-execute-persistent-action)))

(use-package helm-projectile
  :bind
  (("M-?" . helm-projectile)))

;; (use-package helm-ag)

;; (use-package helm-git-grep)

;; (use-package helm-swoop
;;   :bind
;;   ("C-x c s" . helm-swoop))


;; (use-package hlinum
;;   :config
;;   (hlinum-activate))

;; (use-package linum
;;   :config
;;   (setq linum-format " %3d ")
;;   (global-linum-mode nil))

(use-package magit
  :bind ("C-x g" . magit-status))

;; (use-package magit-popup)

;; (use-package multiple-cursors
;;   :bind
;;   ("C-S-c C-S-c" . mc/edit-lines)
;;   ("C->" . mc/mark-next-like-this)
;;   ("C-<" . mc/mark-previous-like-this)
;;   ("C-c C->" . mc/mark-all-like-this))

;; (use-package neotree
;;   :config
;;   (setq neo-theme 'arrow
;;         neotree-smart-optn t
;;         neo-window-fixed-size nil)
;;   ;; Disable linum for neotree
;;   (add-hook 'neo-after-create-hook 'disable-neotree-hook))

;; (use-package org
;;   :config
;;   (setq org-directory "~/org-files"
;;         org-default-notes-file (concat org-directory "/todo.org"))
;;   :bind
;;   ("C-c l" . org-store-link)
;;   ("C-c a" . org-agenda))

;; (use-package org-projectile
;;   :config
;;   (org-projectile-per-project)
;;   (setq org-projectile-per-project-filepath "todo.org"
;; 	org-agenda-files (append org-agenda-files (org-projectile-todo-files))))

;; (use-package org-bullets
;;   :config
;;   (setq org-hide-leading-stars t)
;;   (add-hook 'org-mode-hook
;;             (lambda ()
;;               (org-bullets-mode t))))

;; (use-package page-break-lines)

(use-package projectile
  :config
  (setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" temp-dir)
	projectile-enable-caching t
	projectile-mode-line '(:eval (if (projectile-project-p)
					 (format " p[%s]"
						 (projectile-project-name))
				       "")))

  (projectile-global-mode))

;; (use-package recentf
;;   :config
;;   (setq recentf-save-file (recentf-expand-file-name "~/.emacs.d/private/cache/recentf"))
;;   (recentf-mode 1))

;; (use-package smartparens)

;; (use-package smex)

(use-package undo-tree
  :config
  ;; Remember undo history
  (setq
   undo-tree-auto-save-history nil
   undo-tree-history-directory-alist `(("." . ,(concat temp-dir "/undo/"))))
  (global-undo-tree-mode 1)
  :bind
  ("s-/" . undo-tree-visualize))

;; (use-package which-key
;;   :config
;;   (which-key-mode))

;; windmove
(require 'windmove)
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)
;; todo: check why use-package doesn't work
;; (use-package windmove
;;   :bind
;;   :config
;;   (windmove-default-keybindings 'meta)
;;   (setq windmove-wrap-around t))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package markdown-mode)

(use-package elfeed
  :config
  (setq elfeed-feeds '("http://www.reddit.com/r/emacs/.rss" ;emacs
		       "http://planet.emacsen.org/ru/atom.xml"
		       "http://planet.emacsen.org/atom.xml"
		       "http://pragmaticemacs.com/feed/"
		       "http://habrahabr.ru/rss/all" ;it
		       "https://news.ycombinator.com/rss"
		       "https://www.linux.org.ru/section-rss.jsp?section=1"
		       "http://www.aaronsw.com/2002/feeds/pgessays.rss" ;Paul Graham
		       "http://stephenramsay.us/atom.xml"
		       )))

(use-package google-translate
  :init
  (progn
    (require 'google-translate-default-ui)
    (setq google-translate-default-source-language "auto")
    (setq google-translate-default-target-language "ru"))
  :bind
  (("C-c t" . google-translate-at-point)))

(use-package goto-chg
  :bind
  (("C-c b ," . goto-last-change)
   ("C-c b ." . goto-last-change-reverse)))

(use-package dockerfile-mode)

(use-package yaml-mode)

(use-package cmake-mode
  :ensure t)

(use-package wgrep
  :ensure t)
(use-package wgrep-ag
  :ensure t)

(use-package guru-mode
  :ensure t
  :config
  (setq guru-warn-only t)
  (guru-global-mode 1))

(use-package skeletor
  :config
  (skeletor-define-template "cpp-make"
    :requires-executables '(("make" . "http://www.gnu.org/software/make/"))
    :no-license? t))

(provide 'base-extensions)
