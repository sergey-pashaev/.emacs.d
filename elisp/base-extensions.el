;;; base-extensions.el --- Base extenstions config file

;;; Commentary:

;;; Code:

;; basic extensions
(use-package no-littering :ensure t)

;; saveplace: save location in file when saving files
(require 'saveplace)
(save-place-mode 1)

;; tramp
(require 'tramp)
(setq tramp-default-method "ssh")

;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      ;; rename after killing uniquified
      uniquify-after-kill-buffer-p t
      ;; don't muck with special buffers
      uniquify-ignore-buffers-re "^\\*")

(use-package avy
  :ensure t
  :bind
  ("C-;" . avy-goto-word-1))

(use-package ag
  :ensure t
  :config
  (setq ag-highlight-search t
        ag-reuse-buffers    t))

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

(use-package flycheck
  :ensure t
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (global-flycheck-mode))

(use-package helm
  :ensure t
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

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on)
  :bind
  ("M-?" . helm-projectile))

(use-package helm-swoop
  :ensure t
  :bind
  ("C-x c s" . helm-swoop))

(use-package magit
  :ensure t
  :config
  (setq magit-diff-refine-hunk 'all)
  :bind
  ("C-x g" . magit-status))

(use-package projectile
  :ensure t
  :config
  (setq projectile-enable-caching t
        projectile-mode-line '(:eval (format " p[%s]" (projectile-project-name))))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode))

(use-package recentf
  :ensure t
  :config
  (setq recentf-max-saved-items 100
        recentf-max-menu-items 15)
  (recentf-mode 1))

;; todo: check why use-package doesn't work
(require 'windmove)
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

(use-package yasnippet
  :ensure t
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

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds '("https://www.reddit.com/.rss?feed=33a3018b0dbb339573b04a5c08c0a799e5c167f5&user=bioh" ;reddit
                       "http://planet.emacsen.org/ru/atom.xml"
                       "https://changelog.com/feed"
                       "http://planet.emacsen.org/atom.xml"
                       "http://pragmaticemacs.com/feed/"
                       "http://habrahabr.ru/rss/all" ;it
                       "https://www.linux.org.ru/section-rss.jsp?section=1"
                     ; "http://www.aaronsw.com/2002/feeds/pgessays.rss" ;Paul Graham
                       "http://stephenramsay.us/atom.xml"
                       )))

(use-package google-translate
  :ensure t
  :init
  (progn
    (require 'google-translate-default-ui)
    (setq google-translate-default-source-language "auto")
    (setq google-translate-default-target-language "ru"))
  :bind
  ("C-c t" . google-translate-at-point))

(use-package dockerfile-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package cmake-mode :ensure t)
(use-package wgrep :ensure t)
(use-package wgrep-ag :ensure t)

(use-package skeletor
  :ensure t
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
  :ensure t
  :config
  (setq plantuml-jar-path "/usr/share/plantuml/plantuml.jar"))

(defun psv/langtool-autoshow-detail-popup (overlays)
  "Show error message w/ OVERLAYS in popup tip."
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
  (setq
   langtool-default-language          "en-US"
   langtool-language-tool-jar         (expand-file-name "~/src/LanguageTool-4.0/languagetool-commandline.jar")
   langtool-autoshow-message-function 'psv/langtool-autoshow-detail-popup)
  :bind
  ("C-c g" . langtool-check))

(use-package string-inflection
  :ensure t
  :bind
  (("C-c s" . string-inflection-all-cycle)))

;; dash
(defun psv/helm-dash-cpp-doc ()
  "Enable C++ dash docset for c++ buffers."
  (interactive)
  (setq-local helm-dash-docsets '("C++")))

(defun psv/helm-dash-python-doc ()
  "Enable python2 dash docset for python buffers."
  (interactive)
  (setq-local helm-dash-docsets '("Python 2")))

(defun psv/helm-dash-ensure-docset-installed (docset-name)
  "Ensures that DOCSET-NAME is installed."
  (make-directory (expand-file-name "~/.docsets") t)
  (if (not (member docset-name (helm-dash-installed-docsets)))
      (helm-dash-install-docset (replace-regexp-in-string (rx whitespace) "_" docset-name))))

(defconst psv/helm-dash-docsets '("C++" "C" "Boost" "Python 2" "Ansible" "Docker")
  "My default docset list.")

(use-package helm-dash
  :ensure t
  :config
  (mapc 'psv/helm-dash-ensure-docset-installed psv/helm-dash-docsets)
  (setq helm-dash-common-docsets psv/helm-dash-docsets)
  (add-hook 'c++-mode-hook 'psv/helm-dash-cpp-doc)
  (add-hook 'python-mode-hook 'psv/helm-dash-python-doc)
  :bind
  ("C-c ?" . helm-dash-at-point))

(use-package git-timemachine :ensure t)

(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-idle-delay 1)
  :bind
  ("M-RET" . company-complete))

(use-package diminish :ensure t)
(diminish 'abbrev-mode)
(diminish 'yas-minor-mode)
(diminish 'helm-mode)
(diminish 'eldoc-mode)
(diminish 'company-mode " ca")

(use-package org-pomodoro
  :ensure t
  :bind
  (("C-c r" . org-pomodoro)))

(use-package ledger-mode
  :ensure t
  :mode "\\.dat\\'")

(use-package flycheck-ledger :ensure t)

(provide 'base-extensions)
;;; base-extensions.el ends here
