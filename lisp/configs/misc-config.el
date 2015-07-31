;;; misc-config.el

;; disable startup screen
(setq inhibit-startup-screen t)

;; a proper stack trace is priceless
(setq debug-on-error t)
(setq byte-compile-warnings nil)

;; nice scrolling
(setq scroll-margin 2
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)                     ; show line number
(column-number-mode t)                   ; show column number
(size-indication-mode t)                 ; show file size (Emacs 22+)

;; general settings
(setq-default indent-tabs-mode nil)      ; I hate tabs!

(delete-selection-mode t)                ; delete the selection with a keypress

(unless (= emacs-major-version 24)
  (setq x-select-enable-clipboard t       ; copy-paste should work ...
        interprogram-paste-function       ; ...with...
        'x-cut-buffer-or-selection-value)); ...other X clients

(setq search-highlight t                 ; highlight when searching...
      query-replace-highlight t)         ; ...and replacing

(fset 'yes-or-no-p 'y-or-n-p)            ; enable y/n answers to yes/no

(global-font-lock-mode t)                ; always do syntax highlighting
(setq require-final-newline t)           ; end files with a newline

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; saveplace: save location in file when saving files
(setq save-place-file (concat tempfiles-dir "saveplace"))
(setq-default save-place t)            ;; activate it for all buffers
(require-or-install 'saveplace)                   ;; get the package

;; save recent files
(require-or-install 'recentf)
(setq recentf-save-file (concat tempfiles-dir "recentf") ;; keep ~/ clean
      recentf-max-saved-items 100          ;; max save 100
      recentf-max-menu-items 15)         ;; max 15 in menu
(recentf-mode t)                  ;; turn it on

;; show-paren-mode: subtle highlighting of matching parens
(show-paren-mode t)

;; Default to unified diffs
(setq diff-switches "-u")

(when (fboundp 'winner-mode)
      (winner-mode 1))

;; bookmarks
(setq
 bookmark-default-file (concat dotfiles-dir "bookmarks") ;; keep my ~/ clean
 bookmark-save-flag 1)                        ;; autosave each change

;; highlight the current line; set a custom face, so we can
;; recognize from the normal marking (selection)
(global-hl-line-mode t) ; turn it on for all modes by default

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun beginning-of-line-dwim ()
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

(global-set-key (kbd "C-a") 'beginning-of-line-dwim)

(setq gc-cons-threshold (* 10 1024 1024))

;; log stuff
;; (add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))
(add-to-list 'auto-mode-alist '("\\.log\\'" . read-only-mode))


(define-abbrev-table 'global-abbrev-table '(
    ;; email
    ("psv/mail" "pashaev.sergey@gmail.com")
    ("psv/name" "Sergey Pashaev")))

;; stop asking whether to save newly added abbrev when quitting emacs
(setq save-abbrevs nil)

;; show time in mode line
(display-time-mode 1)
(setq display-time-format "%H:%M")

;; recenter after jump to occurence
(defun psv/occur-find-occurence-hook ()
  (recenter-top-bottom))

(add-hook 'occur-mode-find-occurrence-hook 'psv/occur-find-occurence-hook)

;; use wgrep
(require-or-install 'wgrep)
(require-or-install 'wgrep-ag)

(provide 'misc-config)
