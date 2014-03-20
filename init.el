;; set package archives
(require 'cl)
(require 'package)

(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

;;; require package if installed, otherwise install and require
(defun require-or-install (PKG)
  (if (package-installed-p PKG)
      (require PKG)
    (progn
      (package-install PKG)
      (require PKG))))

(require-or-install 'use-package)

;;; ui
(defun psv/update-cursor-color ()
  (set-cursor-color (if current-input-method
                        "red"
                      "black")))

(progn
    ;; bars
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (blink-cursor-mode -1)
    (set-scroll-bar-mode 'right)
    (scroll-bar-mode -1)

    (set-default-font "Liberation Mono 10")

    ;; cursor color
    (if window-system
        (add-hook 'post-command-hook 'psv/update-cursor-color)))

;;; uniquify
;; (require-or-install 'uniquify)
;; (use-package uniquify
;;   :init
;;   (progn
;;     (setq uniquify-buffer-name-style 'forward
;;           uniquify-separator "/"
;;           ;; rename after killing uniquified
;;           uniquify-after-kill-buffer-p t
;;           ;; don't muck with special buffers
;;           uniquify-ignore-buffers-re "^\\*")))

;;; common bindings
(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)
(global-set-key (kbd "C-c r") 'rename-file-and-buffer)
(global-set-key (kbd "<f8>") 'kill-this-buffer)
(global-set-key (kbd "C-<f6>") 'whitespace-mode)
(global-set-key (kbd "C-<f12>") 'toggle-truncate-lines)
(global-set-key (kbd "C-<backspace>") 'backward-kill-word)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<f12>") 'menu-bar-mode)

;;; misc
(defun psv/beginning-of-line-dwim ()
  "Toggles between moving point to the first non-whitespace character, and
the start of the line."
  (interactive)
  (let ((start-position (point)))
    ;; see if going to the beginning of the line changes our position
    (move-beginning-of-line nil)

    (when (= (point) start-position)
      ;; we're already at the beginning of the line, so go to the
      ;; first non-whitespace character
      (back-to-indentation))))

(global-set-key (kbd "C-a") 'psv/beginning-of-line-dwim)

(progn
    ;; common minor modes
    (line-number-mode t)
    (column-number-mode t)
    (size-indication-mode t)
    (delete-selection-mode t)

    ;; always do syntax highlighting
    (global-font-lock-mode t)

    ;; revert buffers automatically when underlying files are changed
    ;; externally
    (global-auto-revert-mode t)

    ;; enable y/n answers to yes/no
    (fset 'yes-or-no-p 'y-or-n-p)

    ;; highlight matching parens
    (show-paren-mode t)

    ;; highligh current line
    (global-hl-line-mode t)

    ;; common parameters
    (setq inhibit-startup-screen t
          ;; a proper stack trace is priceless
          debug-on-error t
          byte-compile-warnings nil

          ;; nice scrolling
          scroll-margin 2
          scroll-conservatively 100000
          scroll-preserve-screen-position t

          ;; i hate tabs
          indent-tabs-mode nil

          ;; highlight when searching and replacing
          search-highlight t
          query-replace-highlight t

          ;; unified diffs
          diff-switches "-u"

          ;; end files with newline
          require-final-newline t

          ;; gc threshold
          gc-cons-threshold (* 10 1024 1024))

    ;; common hooks
    (add-hook 'before-save-hook 'delete-trailing-whitespace))

;; TODO: saveplace and recentf

;; russian
(defun psv/toggle-russian-input-method ()
  (interactive)
  (if (string= current-input-method "russian-computer")
      (progn
        (inactivate-input-method))
    (progn
      (set-input-method "russian-computer"))))

(global-set-key (kbd "C-\\") 'psv/toggle-russian-input-method)

;; projectile
(require-or-install 'projectile)
(use-package projectile
  :init
  (projectile-global-mode))

;; personal
(setq user-mail-address "pashaev.sergey@gmail.com"
      user-full-name "Sergey Pashaev")

;; ace jump mode
(require-or-install 'ace-jump-mode)
(use-package ace-jump-mode
  :bind ("C-;" . ace-jump-mode))

;; magit
(require-or-install 'magit)
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

;;; jabber
(require-or-install 'jabber)

(defun psv/jabber-hook ()
  (define-key jabber-chat-mode-map (kbd "RET") 'jabber-chat-buffer-send)
  (define-key jabber-chat-mode-map (kbd "<C-return>") 'newline))

(use-package jabber
  :init
  (progn
    (setq
     jabber-chat-buffer-show-avatar nil
     jabber-roster-line-format " %c %-25n %u %-8s  %S"
     jabber-roster-show-bindings nil
     jabber-roster-show-title nil
     jabber-history-dir "~/.emacs.d/jabber-history"
     jabber-history-enabled t
     jabber-use-global-history nil)

    (add-hook 'jabber-chat-mode-hook 'psv/jabber-hook t))
  :bind (("C-x C-;" . jabber-activity-switch-to)
         ("<C-f10>" . jabber-switch-to-roster-buffer)))

;;; irfc
(require-or-install 'irfc)
(use-package irfc
  :init
  (setq irfc-directory "~/workspace/rfc/"
        irfc-assoc-mode t))

;;; ido
(use-package ido
  :init
  (progn
    (ido-mode t)
    (icomplete-mode t)
    (setq ido-enable-flex-matching t
          ido-use-filename-at-point 'guess
          ido-everywhere t
          ido-save-directory-list-file "~/.emacs.d/ido.last")))

;;; ibuffer
(defun psv/ibuffer-hook ()
  (ibuffer-switch-to-saved-filter-groups "default"))

(use-package ibuffer
  :init
  (progn
    (setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("org-mode" (or
                            (name . "^org$")
                            (mode . org-mode)))
               ("emacs lisp" (mode . emacs-lisp-mode))
               ("magit" (name . "^\\*magit"))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))
               ("c++ / c" (or
                           (mode . c-mode)
                           (mode . c++-mode)))
               ("cedet" (or
                         (name . "^\\*CEDET Global\\*$")
                         (name . "^\\*Semantic SymRef\\*$")
                         (name . "^\\*Symref ")))
               ("jabber / irc" (or
                          (name . "^\\*-jabber-chat-")
                          (name . "^\\*-jabber-roster-")
                          (mode . jabber-chat)
                          (mode . jabber-roster)
                          (mode . erc-mode)))
               ("go lang" (or
                           (mode . go-mode)
                           (name . "^\\*godoc")
                           (name . "^\\*godef")))))))

    (add-hook 'ibuffer-mode-hook 'psv/ibuffer-hook)))

;;; golang
;; mysql driver
;;go get -u github.com/go-sql-driver/mysql

;; dev utils
;;go get -u github.com/dougm/goflymake
;;go get -u github.com/sergey-pashaev/goflymake
;;go get -u github.com/nsf/gocode
;;go get -u code.google.com/p/rog-go/exp/cmd/godef
;;go get -u github.com/sriram-srinivasan/gore
;;go get -u github.com/golang/lint/golint
(require-or-install 'go-mode)
;(require-or-install 'go-autocomplete)
(require-or-install 'go-eldoc)
(require-or-install 'golint)
;(require-or-install 'go-flycheck)

(defun psv/go-hook ()
  (go-eldoc-setup)
  (flycheck-mode t)
  (setq tab-width 4)
  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
  (local-set-key (kbd "C-c i") 'go-goto-imports))

(use-package go-mode
  :init
  (progn
    (add-hook 'go-mode-hook 'psv/go-hook)
    (add-to-list 'load-path "~/workspace/go/src/github.com/sergey-pashaev/goflymake")))

;;; encodings
(progn
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)
    (set-selection-coding-system 'utf-8)
    (prefer-coding-system 'utf-8)
    (set-language-environment "UTF-8")
    (set-input-method nil)
    (setq read-quoted-char-radix 10
          locale-coding-system 'utf-8))

;;; elisp
(use-package emacs-lisp-mode
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
    (define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
    (define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)))

;;; erc
(require-or-install 'erc)
(use-package erc
  :init
  (progn
    (setq erc-hide-list '("JOIN" "PART" "QUIT")
          erc-prompt-for-nickserv-password nil
          erc-save-buffer-on-part t)))

;;; dired
(defun psv/dired-hook ()
  (hl-line-mode 1))

(use-package dired
  :init
  (progn
    (add-hook 'dired-mode-hook 'psv/dired-hook)))

;;; helm
(require-or-install 'helm)
(require-or-install 'helm-projectile)
(require-or-install 'helm-flycheck)
(require 'helm-config)
(use-package helm
  :bind
  (("M-?" . helm-projectile)
   ("C-c !" . helm-flycheck)))

;;; expand-region
(require-or-install 'expand-region)
(use-package expand-region
  :bind ("C-=" . er/expand-region))
