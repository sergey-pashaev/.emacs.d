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
; (setq gnutls-trustfiles (list psv/trustfile))
; (add-to-list 'gnutls-trustfiles psv/trustfile)
;;; install use-package if needed
(setq load-prefer-newer t)
(require 'package)
(unless package--initialized (package-initialize t))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; auto compile emacs lisp files
(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

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

      ;; don't indent with tabs
      indent-tabs-mode                    nil

      inhibit-startup-message             t
      inhibit-startup-screen              t
      fringes-outside-margins             t
      x-select-enable-clipboard           t

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

;;; Bookmarks
(setq
 ;; persistent bookmarks
 bookmark-save-flag                      t
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
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; show time in mode line
(display-time-mode 1)
(setq display-time-format "%H:%M")

;;; ido
(ido-mode t)
(icomplete-mode t)

(setq ido-enable-flex-matching t
      ido-use-filename-at-point 'guess
      ido-everywhere t
      ido-save-directory-list-file (concat psv/temp-dir "/ido.last"))

;;; personal info
(setq user-mail-address "pashaev.sergey@gmail.com"
      user-full-name "Sergey Pashaev")

;;; ibuffer
(defun psv/ibuffer-hook ()
  "Make ibuffer Use \"psv\" filter group."
  (ibuffer-switch-to-saved-filter-groups "psv"))

(setq ibuffer-saved-filter-groups
      (quote (("psv"
	       ("dired" (mode . dired-mode))
	       ("elisp" (mode . emacs-lisp-mode))
	       ("org-mode" (or
			    (name . "^org$")
			    (mode . org-mode)))
	       ("vcs" (or (name . "^\\*magit")
			  (name . "^\\*vc")))
	       ("search" (or (name . "^\\*ag")
			     (name . "^\\*grep")
			     (name . "^\\*Occur")))
	       ("emacs" (or
			 (name . "^\\*scratch\\*$")
			 (name . "^\\*Messages\\*$")
			 (name . "^\\*Warnings\\*$")
			 (name . "^\\*helm")
			 (name . "^\\*elfeed")))
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

(add-hook 'ibuffer-mode-hook 'psv/ibuffer-hook)

;;; dired
(defun psv/dired-hook ()
  "Custom dired hook."
  (progn
    ;; (bind-key "C-o" 'dired-omit-mode dired-mode-map)
    ;; (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.")
    ;; todo: move listing switches to emacs customization part
    ))

(add-hook 'dired-mode-hook 'psv/dired-hook)

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

;; change cursor color with keyboard layout change
(defun psv/update-cursor-color ()
  (set-cursor-color (if current-input-method
                        "red"
                      "black")))

(if window-system
    (add-hook 'post-command-hook 'psv/update-cursor-color))

;; notify when compilation is done
(require 'notifications)

(defun emacs-notify-send-message (headline-string message-string)
  """Send message to notification"""
  (shell-command (concat "notify-send -u critical -i emacs \""
                         headline-string
                         "\" \""
                         message-string
                         "\"")))

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
;; 		   (format-time-string "%H:%M" (time-add (current-time)
;; 							 (* 2 60))))))
;; (defun psv/make-some-rest-dbus ()
;;   (interactive)
;;   (notifications-notify
;;    :app-name "emacs"
;;    :body (concat "Get some rest and continue at "
;; 		 (format-time-string "%H:%M" (time-add (current-time)
;; 						       (* 2 60))))))

;; (if (dbus-ping :session "org.freedesktop.Notifications")
;;   (run-with-timer (* 20 60) (* 20 60) 'psv/make-some-rest-dbus)
;;   (run-with-timer (* 20 60) (* 20 60) 'psv/make-some-rest-msg))


;; default font
(set-frame-font "Liberation Mono 11")

(provide 'base)
;;; base ends here
