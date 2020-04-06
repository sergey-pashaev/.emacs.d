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

(use-package flycheck
  :ensure t
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (setq flycheck-mode-line-prefix " fc")
  (global-flycheck-mode))

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
    (setq google-translate-output-destination 'echo-area)
    (setq google-translate-default-source-language "auto")
    (setq google-translate-default-target-language "ru"))
  :bind
  ("C-c t" . google-translate-at-point))

(use-package dockerfile-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package cmake-mode :ensure t)
(use-package wgrep :ensure t)

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

(require 'popup)
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

(use-package string-inflection :ensure t)

;; dash
(defun psv/helm-dash-cpp-doc ()
  "Enable C++ dash docset for c++ buffers."
  (interactive)
  (setq-local helm-dash-docsets '("C++" "C")))

(defun psv/helm-dash-python-doc ()
  "Enable python2 dash docset for python buffers."
  (interactive)
  (setq-local helm-dash-docsets '("Python 2")))

(defun psv/helm-dash-bash-doc ()
  "Enable bash dash docset for shell buffers."
  (interactive)
  (setq-local helm-dash-docsets '("Bash")))

(defun psv/helm-dash-ensure-docset-installed (docset-name)
  "Ensures that DOCSET-NAME is installed."
  (make-directory (expand-file-name "~/.docsets") t)
  (if (not (helm-dash-docset-installed-p docset-name))
      (let ((tarball (replace-regexp-in-string (rx whitespace) "_" docset-name)))
        (helm-dash-install-docset-from-file (concat psv/helm-dash-tarballs-path tarball)))))

(defconst psv/helm-dash-tarballs-path (expand-file-name "~/docsets/")
  "My default path to downloaded docset tarballs.")

(defconst psv/helm-dash-docsets '("C++" "C" "Python 2" "CMake" "Bash")
  "My default docset list.")

(use-package helm-dash
  :ensure t
  :config
  (setq helm-dash-common-docsets psv/helm-dash-docsets)
  (add-hook 'c++-mode-hook 'psv/helm-dash-cpp-doc)
  (add-hook 'python-mode-hook 'psv/helm-dash-python-doc)
  (add-hook 'sh-mode-hook 'psv/helm-dash-bash-doc)
  :bind
  ("C-c ?" . helm-dash-at-point)
  (:map sh-mode-map
        ("C-c ?" . helm-dash-at-point)))

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
