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

(use-package ag
  :config
  (setq ag-highlight-search t
	ag-reuse-buffers    t))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package flycheck
  :config
  (global-flycheck-mode))

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

(use-package helm-swoop
  :bind
  ("C-x c s" . helm-swoop))

(use-package magit
  :bind ("C-x g" . magit-status))

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

(use-package projectile
  :config
  (setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" temp-dir)
	projectile-enable-caching t
	projectile-mode-line '(:eval (if (projectile-project-p)
					 (format " p[%s]"
						 (projectile-project-name))
				       "")))

  (projectile-global-mode))

(use-package recentf
  :config
  (setq recentf-save-file (recentf-expand-file-name (expand-file-name "recentf" temp-dir)))
  (recentf-mode 1))

(use-package undo-tree
  :config
  ;; Remember undo history
  (setq
   undo-tree-auto-save-history nil
   undo-tree-history-directory-alist `(("." . ,(concat temp-dir "/undo/"))))
  (global-undo-tree-mode 1)
  :bind
  ("s-/" . undo-tree-visualize))

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
  (use-package yasnippet-snippets
    :ensure t)
  (yas-global-mode 1)
  (yas-reload-all))

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

(use-package skeletor
  :config
  (setq skeletor-project-directory (expand-file-name "~/workspace/cpp/"))
  (skeletor-define-template "cpp-make"
    :requires-executables '(("make" . "http://www.gnu.org/software/make/"))
    :no-license? t)
  (skeletor-define-template "appl"
    :no-license? t)
  (skeletor-define-template "cpp-cmake"
    :requires-executables '(("make" . "http://www.gnu.org/software/make/")
			    ("cmake" . "https://cmake.org/"))
    :no-license? t
    :before-git
    (lambda (dir)
      (skeletor-shell-command "chmod +x build.sh" dir))))

(use-package plantuml-mode
  :config
  (setq plantuml-jar-path "/usr/share/plantuml/plantuml.jar"))

(defun langtool-autoshow-detail-popup (overlays)
  (when (require 'popup nil t)
    ;; Do not interrupt current popup
    (unless (or popup-instances
                ;; suppress popup after type `C-g` .
                (memq last-command '(keyboard-quit)))
      (let ((msg (langtool-details-error-message overlays)))
        (popup-tip msg)))))

(use-package langtool
  :ensure t
  :config
  (setq langtool-language-tool-jar (expand-file-name "~/src/LanguageTool-4.0/languagetool-commandline.jar")
	langtool-autoshow-message-function 'langtool-autoshow-detail-popup))

(use-package string-inflection
  :ensure t
  :bind
  (("C-c s" . string-inflection-all-cycle)))

(provide 'base-extensions)
