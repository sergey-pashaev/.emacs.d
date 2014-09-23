;;;; minimal config - no external dependencies here

;;; personal
(setq user-mail-address "pashaev.sergey@gmail.com"
      user-full-name "Sergey Pashaev")

;;; common parameters
(setq inhibit-startup-screen t
      debug-on-error t
      byte-compile-warnings nil

      scroll-margin 2
      scroll-conservatively 100000
      scroll-preserve-screen-position t

      indent-tabs-mode nil

      search-highlight t
      query-replace-highlight t

      ;; unified diffs
      diff-switches "-u"

      ;; end files with newline
      require-final-newline t

      gc-cons-threshold (* 10 1024 1024))

;;; common hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; enable y/n answers to yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;;; encodings
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-input-method nil)
(setq read-quoted-char-radix 10
      locale-coding-system 'utf-8)

;;; ui
(defun psv/update-cursor-color ()
  (set-cursor-color (if current-input-method
                        "red"
                      "black")))

(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(set-scroll-bar-mode 'right)
(scroll-bar-mode -1)
(set-default-font "Liberation Mono 10")
(if window-system
    (add-hook 'post-command-hook 'psv/update-cursor-color))

;;; common minor modes
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(delete-selection-mode t)
(global-font-lock-mode t)
(global-auto-revert-mode t)
(show-paren-mode t)
(global-hl-line-mode t)

(defun psv/toggle-russian-input-method ()
  (interactive)
  (if (string= current-input-method "russian-computer")
      (progn
        (inactivate-input-method))
    (progn
      (set-input-method "russian-computer"))))

;;; ido
(defun psv/recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(ido-mode t)
(icomplete-mode t)
(setq ido-enable-flex-matching t
      ido-use-filename-at-point 'guess
      ido-everywhere t
      ido-save-directory-list-file "~/.emacs.d/ido.last")

;;; ibuffer
(defun psv/ibuffer-hook ()
  (ibuffer-switch-to-saved-filter-groups "default"))

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

(add-hook 'ibuffer-mode-hook 'psv/ibuffer-hook)

;;; dired
(defun psv/dired-hook ()
  (hl-line-mode 1))

(add-hook 'dired-mode-hook 'psv/dired-hook)

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

;;; common bindings
(setq psv/common-bindings-list
      '(("C-x \\" . align-regexp)
	 ("<f8>" . kill-this-buffer)
	 ("C-<f6>" . whitespace-mode)
	 ("C-<f12>" . toggle-truncate-lines)
	 ("C-<backspace>" . backward-kill-word)
	 ("<f12>" . menu-bar-mode)
	 ("C-\\" . psv/toggle-russian-input-method)
	 ("C-x C-b" . ibuffer)
	 ;("C-a" . psv/beginning-of-line-dwim)
	 ("C-x f" . psv/recentf-ido-find-file)
	 ))

(mapc (lambda (p) (global-set-key (kbd (car p)) (cdr p)))
      psv/common-bindings-list)

;; elisp
(defun psv/remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))


(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'psv/remove-elc-on-save)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; backups - store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . "~/.emacs.d/backups")))

(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/autosave" t)))

;; saveplace: save location in file when saving files
(setq save-place-file (concat user-emacs-directory "saveplace"))
(setq-default save-place t)
(require 'saveplace)

;; save recent files
(require 'recentf)
(setq recentf-save-file (concat user-emacs-directory "recentf")
      recentf-max-saved-items 100
      recentf-max-menu-items 15)
(recentf-mode t)

;; tramp
(require 'tramp)
(setq tramp-default-method "ssh")
