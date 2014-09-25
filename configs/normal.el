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
  :bind
  (("M-?" . helm-projectile)
   ("C-c !" . helm-flycheck)))

(global-set-key (kbd "M-x") 'helm-M-x)
