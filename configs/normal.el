;; uniquify
(use-package uniquify
  :init
  (progn
    (setq uniquify-buffer-name-style 'forward
          uniquify-separator "/"
          ;; rename after killing uniquified
          uniquify-after-kill-buffer-p t
          ;; don't muck with special buffers
          uniquify-ignore-buffers-re "^\\*")))

;; projectile
(require-package 'projectile)
(use-package projectile
  :init
  (projectile-global-mode))

;; ace jump mode
(require-package 'ace-jump-mode)
(use-package ace-jump-mode
  :bind ("C-;" . ace-jump-mode))

;; magit
(require-package 'magit)
(use-package magit
  :bind ("C-x g" . magit-status))

;; org-mode
(defvar *psv/ditaa-path* "")
(defvar *psv/plantuml-path* "")

(defun psv/org-confirm-babel-evaluate (lang body)
  (not (or (string= lang "ditaa")
           (string= lang "dot")
           (string= lang "plantuml"))))

(defun psv/org-mode-hook ()
  (define-key org-mode-map (kbd "C-c t") 'org-time-stamp)
  (org-indent-mode t)
  (visual-line-mode t))

(use-package org-mode
  :init
  (progn
    (setq calendar-week-start-day 1
          org-confirm-babel-evaluate 'psv/org-confirm-babel-evaluate
          org-agenda-files '()
          ;; org-ditaa-jar-path *psv/ditaa-path*
          ;; org-plantuml-jar-path *psv/plantuml-path*
          )
    (add-hook 'org-mode-hook 'psv/org-mode-hook))
  :bind
  (("C-c l" . org-store-link)
   ("C-c c" . org-capture)
   ("C-c a" . org-agenda)
   ("C-c b" . org-iswitchb)
   ("C-c r" . org-remember)))

;; expand-region
(require-package 'expand-region)
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;;; helm
(require-package 'helm)
(require-package 'helm-projectile)
(require-package 'helm-flycheck)

(require 'helm)
(require 'helm-projectile)
(require 'helm-flycheck)
(require 'helm-config)

(use-package helm
  :init
  (progn
    (setq helm-quick-update                     t ; do not display invisible candidates
	  helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
	  helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
	  helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
	  helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
	  helm-ff-file-name-history-use-recentf t)

    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)	; make TAB works in terminal
    (define-key helm-map (kbd "C-z")  'helm-select-action)		; list actions using C-z

    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))

    (helm-mode 1))
    :bind
    (("M-?" . helm-projectile)
     ("C-c !" . helm-flycheck)))

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)

;; auto-complete
(require-package 'auto-complete)
(require 'auto-complete-config)

(use-package auto-complete
  :init
  (progn
    (ac-config-default)

    (setq ac-auto-start nil)
    (setq ac-auto-show-menu t)
    (setq ac-ignore-case 'smart)

    (define-key ac-mode-map [(meta return)] 'auto-complete)))
