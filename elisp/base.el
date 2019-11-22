;;; base.el --- Base configuration module which doesn't depend on any external packages

;;; Commentary:

;;; Code:

;;; make sure all connection are secure according to:
;;; https://glyph.twistedmatrix.com/2015/11/editor-malware.html
(defconst psv/trustfile (replace-regexp-in-string
                         "\\\\" "/"
                         (replace-regexp-in-string
                          "\n" ""
                          (shell-command-to-string "python -m certifi")))
  "Path to file with trusted certificates from python certifi package.")

(require 'tls)
(setq tls-program
      (list
       (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
               (if (eq window-system 'w32) ".exe" "") psv/trustfile)))

(setq gnutls-verify-error t)
(setq gnutls-trustfiles (list psv/trustfile))

;;; install use-package if needed
(setq load-prefer-newer t)
(require 'package)
(unless package--initialized (package-initialize))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)

(setq package-archive-priorities
      '(("melpa-stable" . 15)
        ("org" . 10)
        ("melpa" . 5)))

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (or (package-installed-p 'use-package)
            (package-installed-p 'diminish))
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish))

(eval-when-compile
  (require 'use-package))
(require 'diminish)

;; utf-8
(set-language-environment    "UTF-8")
(set-charset-priority        'unicode)
(setq locale-coding-system   'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(set-input-method nil)            ; no funky input for normal editing
(setq read-quoted-char-radix 10)  ; use decimal, not octal

;;; Emacs customizations
(setq confirm-kill-emacs                  'y-or-n-p
      confirm-nonexistent-file-or-buffer  t
      mouse-yank-at-point                 t
      visible-bell                        t
      ring-bell-function                  'ignore
      custom-file                         (concat user-emacs-directory "custom.el")

      ;; forbid to edit minibuffer prompt
      ;; http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
      minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)

      ;; disable non selected window highlight
      cursor-in-non-selected-windows      nil
      highlight-nonselected-windows       nil

      ;; path
      exec-path                           (append exec-path '("/usr/local/bin/"))

      inhibit-startup-message             t
      inhibit-startup-screen              t
      fringes-outside-margins             t

      ;; a proper stack trace is priceless
      debug-on-error                      t
      byte-compile-warnings               nil

      ;; nice scrolling
      scroll-margin                       5
      scroll-conservatively               100000
      scroll-preserve-screen-position     t

      ;; highlight when searching and replacing
      search-highlight                    t
      query-replace-highlight             t

      ;; unified diffs
      diff-switches                       "-u"

      ;; end files with newline
      require-final-newline               t

      ;; dired
      dired-dwim-target                   t ; guess target directory
      dired-listing-switches              "-alh"
      wdired-allow-to-change-permissions  t

      ;; stop asking whether to save newly added abbrev when quitting emacs
      save-abbrevs                        nil

      ;; Save whatever’s in the current (system) clipboard before
      ;; replacing it with the Emacs’ text.
      ;; https://github.com/dakrone/eos/blob/master/eos.org
      save-interprogram-paste-before-kill t

      ;; changing the recentering order
      recenter-positions                  '(middle top bottom))

;; don't use tabls
(setq-default indent-tabs-mode nil)

;;; Bookmarks
(setq bookmark-save-flag t) ; persistent bookmarks

;;; Lockfiles
(setq create-lockfiles nil)

;;; Backups
(setq
 backup-inhibited                   nil
 backup-by-copying                  t
 delete-old-versions                t
 version-control                    t
 make-backup-files                  t)

;;; Autosave
(setq auto-save-default t)

;; yes/no -> y/n
(fset 'yes-or-no-p 'y-or-n-p)

;;; Common minor modes
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; highlight the current line
(global-hl-line-mode t)

;; always do syntax highlighting
(global-font-lock-mode t)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; subtle highlighting of matching parens
(show-paren-mode t)

;; delete the selection with a keypress
(delete-selection-mode -1)

;; delete trailing whitespace before save
(defun psv/before-save-hook ()
  "Delete trailing whitespace everywhere."
  (delete-trailing-whitespace))

(add-hook 'before-save-hook 'psv/before-save-hook)

;;; personal info
(setq user-mail-address "pashaev.sergey@gmail.com"
      user-full-name "Sergey Pashaev")

;;; UI

;; disable toolbar & menubar
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when window-system
  (blink-cursor-mode -1)
  (tooltip-mode -1))

(defun psv/update-cursor-color ()
  "Change cursor color with keyboard layout change."
  (set-cursor-color (if current-input-method
                        "red"
                      "black")))

(if window-system
    (add-hook 'post-command-hook 'psv/update-cursor-color))

;; default font
(set-frame-font "Liberation Mono 11")
(add-to-list 'default-frame-alist '(font . "Liberation Mono 11"))

;; find file
(require 'find-file)
(setq ff-always-try-to-create nil)

(setq browse-url-browser-function 'browse-url-chromium)

;; show buffer filepath at frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

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

(provide 'base)
;;; base ends here
