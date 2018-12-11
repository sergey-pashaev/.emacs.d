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

(setq tls-program
      (list
       (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
               (if (eq window-system 'w32) ".exe" "") psv/trustfile)))

(setq gnutls-verify-error t)
(setq gnutls-trustfiles (list psv/trustfile))

;;; install use-package if needed
(setq load-prefer-newer t)
(require 'package)
(unless package--initialized (package-initialize t))
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

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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

      ;; stop asking whether to save newly added abbrev when quitting emacs
      save-abbrevs                        nil

      ;; Save whatever’s in the current (system) clipboard before
      ;; replacing it with the Emacs’ text.
      ;; https://github.com/dakrone/eos/blob/master/eos.org
      save-interprogram-paste-before-kill t

      ;; changing the recentering order
      recenter-positions                  '(top middle bottom)

      use-package-always-ensure           t)

(setq-default indent-tabs-mode nil)

;;; Bookmarks
(setq
 ;; persistent bookmarks
 bookmark-save-flag                 t
 bookmark-default-file              (concat psv/temp-dir "/bookmarks"))

;;; Lockfiles
(setq create-lockfiles nil)

;;; Backups
(setq
 backup-inhibited                   nil
 backup-by-copying                  t
 delete-old-versions                t
 version-control                    t
 make-backup-files                  t
 backup-directory-alist            `(("." . ,psv/backup-root-dir)))

;; make backup to a designated dir, mirroring the full path
(defun psv/backup-file-name (file-path)
  "Return a new file path of a given FILE-PATH.
If the new path's directories does not exist, create them."
  (let* ((origin-file-path (replace-regexp-in-string "[A-Za-z]:" "" file-path)) ; remove Windows driver letter in path, for example, “C:”
         (backup-file-path (replace-regexp-in-string "//" "/" (concat psv/backup-root-dir origin-file-path "~"))))
    (make-directory (file-name-directory backup-file-path) (file-name-directory backup-file-path))
    backup-file-path))

(setq make-backup-file-name-function 'psv/backup-file-name)

;;; Autosave
(setq
 auto-save-default                  t
 auto-save-list-file-name           (concat psv/temp-dir "/autosave"))

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
(delete-selection-mode t)

;; delete trailing whitespace before save
(defun psv/before-save-hook ()
  "Delete trailing ws everywhere but at SwiftBack project."
  (when (not (string= (projectile-project-name) "SwiftBack"))
    (delete-trailing-whitespace)))

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

;; notify when compilation is done
;; (require 'notifications)

;; (defun emacs-notify-send-message (headline-string message-string)
;;   "Send message with HEADLINE-STRING title and MESSAGE-STRING to notification system."
;;   (shell-command (concat "notify-send -u critical -i emacs \""
;;                          headline-string
;;                          "\" \""
;;                          message-string
;;                          "\"")))

;; (setq compilation-finish-functions 'compile-finish-notify)

;; (defun compile-finish-notify (buffer string)
;;   (progn
;;     (notifications-notify
;;      :app-name "emacs"
;;      :body (concat "Compilation is " string))
;;     (emacs-notify-send-message "emacs" (concat "Compilation is " string))))

;; (defun psv/make-some-rest-msg ()
;;   (interactive)
;;   (message (concat "Get some rest and continue at "
;;                 (format-time-string "%H:%M" (time-add (current-time)
;;                                                       (* 2 60))))))
;; (defun psv/make-some-rest-dbus ()
;;   (interactive)
;;   (notifications-notify
;;    :app-name "emacs"
;;    :body (concat "Get some rest and continue at "
;;               (format-time-string "%H:%M" (time-add (current-time)
;;                                                     (* 2 60))))))

;; (if (dbus-ping :session "org.freedesktop.Notifications")
;;   (run-with-timer (* 20 60) (* 20 60) 'psv/make-some-rest-dbus)
;;   (run-with-timer (* 20 60) (* 20 60) 'psv/make-some-rest-msg))


;; default font
(set-frame-font "Liberation Mono 11")

;; find file
(setq ff-always-try-to-create nil)

(provide 'base)
;;; base ends here
