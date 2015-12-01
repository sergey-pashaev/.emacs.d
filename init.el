;; inspiring emacs configs:
;; - http://doc.rix.si/org/fsem.html
;; - http://pages.sachachua.com/.emacs.d/Sacha.html
;; - https://github.com/thomasf/dotfiles-thomasf-emacs/
;; - https://github.com/danielmai/.emacs.d
;; - https://github.com/bradwright/emacs-d
;; - https://github.com/magnars/.emacs.d

;; Turn off mouse interface early in startup to avoid momentary display
(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode -1)
  (tooltip-mode -1))

;;; Set up package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

;;; Bootstrap use-package
;; Install use-package if it's not already installed.
;; use-package is used to configure the rest of the packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; set an explicit file to customization created via the UI
(setq custom-file (concat user-emacs-directory "custom.el"))

;; add everything to load-path
(defun psv/add-subfolders-to-load-path (parent-dir)
  "Adds all first level `parent-dir' subdirs to the
Emacs load path."
  (dolist (f (directory-files parent-dir))
    (let ((name (concat parent-dir f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

(mapc (lambda (p) (push p load-path))
      '("~/.emacs.d/configs/"))

;;;; minimal config - no external dependencies here

;;; personal
(setq user-mail-address "pashaev.sergey@gmail.com"
      user-full-name "Sergey Pashaev")

;;; common parameters
(setq inhibit-startup-screen t
      inhibit-startup-message t

      ;; a proper stack trace is priceless
      debug-on-error t
      byte-compile-warnings nil

      ;; nice scrolling
      scroll-margin 2
      scroll-conservatively 100000
      scroll-preserve-screen-position t

      indent-tabs-mode nil

      ;; highlight when searching and replacing
      search-highlight t
      query-replace-highlight t

      ;; unified diffs
      diff-switches "-u"

      ;; end files with newline
      require-final-newline t

      ;; garbage collector limit to 10mb
      gc-cons-threshold (* 10 1024 1024))

;;; common hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; yes/no -> y/n
(fset 'yes-or-no-p 'y-or-n-p)

;;; encodings
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-input-method nil)			; no funky input for normal editing;
(setq read-quoted-char-radix 10		; use decimal, not octal
      locale-coding-system 'utf-8)

;;; ui
(defun psv/update-cursor-color ()
  (set-cursor-color (if current-input-method
                        "red"
                      "black")))

(setq visible-bell t)

(if window-system
    (add-hook 'post-command-hook 'psv/update-cursor-color))

(set-default-font "Liberation Mono 10")

;; toggle menu-bar visibility
(bind-key "<f12>" 'menu-bar-mode)

;;; common minor modes
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; always do syntax highlighting
(global-font-lock-mode t)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; subtle highlighting of matching parens
(show-paren-mode t)

;; highlight the current line
(global-hl-line-mode t)

;;; russian input
(defun psv/toggle-russian-input-method ()
  (interactive)
  (if (string= current-input-method "russian-computer")
      (progn
        (inactivate-input-method))
    (progn
      (set-input-method "russian-computer"))))

(bind-key "C-\\" 'psv/toggle-russian-input-method)

;;; ido
(defun psv/recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))

(ido-mode t)
(icomplete-mode t)

(setq ido-enable-flex-matching t
      ido-use-filename-at-point 'guess
      ido-everywhere t
      ido-save-directory-list-file (concat user-emacs-directory "ido.last"))

;; jump to a definition in the current file
(bind-key "M-i" 'ido-goto-symbol)

;; File finding
(bind-key "C-x f" 'psv/recentf-ido-find-file)

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

(bind-key "C-x C-b" 'ibuffer)

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

(bind-key "C-a" 'psv/beginning-of-line-dwim)

;;; common bindings

;; Align your code in a pretty way.
(bind-key "C-x \\" 'align-regexp)

;; duplicate the current line or region
(bind-key "C-c d" 'duplicate-current-line-or-region)

;; rename buffer & visited file
(bind-key "C-c r" 'rename-file-and-buffer)

(bind-key "<f8>" 'kill-this-buffer)
(bind-key "C-<f6>" 'whitespace-mode)
(bind-key "C-<f12>" 'toggle-truncate-lines)
(bind-key "C-<backspace>" 'backward-kill-word)
(bind-key "RET" 'newline-and-indent)
(bind-key "M-/" 'dabbrev-expand)

;; unbound dangerous keys
(global-unset-key (kbd "C-x C-c"))

;; unbound "C-x 5 0 <-> O" typo
(global-unset-key (kbd "C-x 5 0"))

;; unbound suspend-frame function
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(bind-key "M-[" 'backward-paragraph)
(bind-key "M-]" 'forward-paragraph)

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

(bind-key "TAB" 'lisp-complete-symbol read-expression-map)
(bind-key "M-." 'find-function-at-point emacs-lisp-mode-map)

;; backups - store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "backups"))))

(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "autosave") t)))

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

;; windmove
(require 'windmove)
(windmove-default-keybindings 'meta)

;;;; minimal.el ends here

;;;; normal.el
;; meaningful names for buffers with the same name
(require 'uniquify)
(progn
  (setq uniquify-buffer-name-style 'forward
	uniquify-separator "/"
	;; rename after killing uniquified
	uniquify-after-kill-buffer-p t
	;; don't muck with special buffers
	uniquify-ignore-buffers-re "^\\*"))

;; ace jump mode
(use-package ace-jump-mode
  :ensure t
  :bind ("C-;" . ace-jump-mode))

;; magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; org-mode
(defvar *psv/ditaa-path* (expand-file-name "~/bin/ditaa.jar"))
(defvar *psv/plantuml-path* (expand-file-name "~/bin/plantuml.jar"))

(defun psv/org-confirm-babel-evaluate (lang body)
  (not (or (string= lang "ditaa")
           (string= lang "dot")
           (string= lang "plantuml"))))

(defun psv/org-mode-hook ()
  (bind-key "C-c t" 'org-time-stamp org-mode-map)
  (org-indent-mode t)
  (visual-line-mode t))

(use-package org-mode
  :init
  (progn
    (setq calendar-week-start-day 1
          org-confirm-babel-evaluate 'psv/org-confirm-babel-evaluate
          org-agenda-files '()
	  org-refile-targets '((org-agenda-files . (:maxlevel . 3)))
	  org-refile-use-outline-path 'file
	  org-return-follows-link t	; follow links by ret
          org-ditaa-jar-path *psv/ditaa-path*
          org-plantuml-jar-path *psv/plantuml-path*
	  org-directory "~/Dropbox/org"
	  org-default-notes-file "~/Dropbox/org/notes.org"
	  org-capture-templates
	  (quote (("t" "Todo" entry (file "~/Dropbox/org/todo.org")  "* TODO %U %?\n")
		  ("n" "Note" entry (file "~/Dropbox/org/notes.org") "* %U %?\n")
		  ("l" "Link" entry (file "~/Dropbox/org/links.org") "* %U %?\n")
		  ("o" "Todo (OPS)" entry (file "~/Dropbox/org/job/ops/todo.org.gpg")
		   "* TODO %U %?\n  %i\n  %a"))))
    (add-hook 'org-mode-hook 'psv/org-mode-hook)

    ;; save the clock history across emacs sessions
    (setq org-clock-persist 'history)
    (org-clock-persistence-insinuate)

    ;; enable markdown export
    (eval-after-load "org"
      '(require 'ox-md nil t))

    (eval-after-load "org"
      '(require 'ox-gfm nil t))

    (eval-after-load "org"
      '(require 'ox-rst nil t))

    (org-babel-do-load-languages 'org-babel-load-languages
				 '((emacs-lisp . t)
				   (sh . t)
				   (ditaa . t)
				   (plantuml . t)
				   (dot . t)
				   (gnuplot . t)
				   (org . t))))
  :bind
  (("C-c l" . org-store-link)
   ("C-c c" . org-capture)
   ("C-c a" . org-agenda)
   ("C-c b" . org-iswitchb)
   ("C-c r" . org-remember)
   ("C-c C-x C-j" . org-clock-goto)))

;; expand-region
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; projectile
(use-package projectile
  :ensure t
  :init
  (projectile-global-mode)
  (setq projectile-enable-caching t))

;;; helm
(use-package helm
  :ensure t
  :init
  (progn
    (require 'helm-config)
    (setq helm-quick-update                     t ; do not display invisible candidates
	  helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
	  helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
	  helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
	  helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
	  helm-ff-file-name-history-use-recentf t)

    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))

    (helm-mode 1))
    :bind
    (("M-x" . helm-M-x)))

(use-package helm-projectile
  :ensure t
  :bind
  (("M-?" . helm-projectile)))

;; auto-complete
(use-package auto-complete
  :ensure t
  :init
  (progn
    (require 'auto-complete-config)
    (ac-config-default)

    (setq ac-auto-start nil)
    (setq ac-auto-show-menu t)
    (setq ac-ignore-case 'smart)

    (setq ac-menu-height 30)
    (setq ac-use-fuzzy t)

    (bind-key "M-RET" 'auto-complete ac-mode-map)))

;; eshell
(use-package exec-path-from-shell
  :ensure t
  :init
  (progn
    (exec-path-from-shell-initialize)

    (setq eshell-cmpl-cycle-completions nil
	  eshell-directory-name (concat user-emacs-directory "eshell/")
	  eshell-save-history-on-exit t
	  eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

    (defalias 'e 'find-file)
    (defalias 'E 'find-file-other-window)))

;;; jabber
(defun psv/jabber-hook ()
  (bind-key "RET" 'jabber-chat-buffer-send jabber-chat-mode-map)
  (bind-key "<C-return>" 'newline jabber-chat-mode-map))

(use-package jabber
  :ensure t
  :init
  (progn
    (setq
     jabber-chat-buffer-show-avatar nil
     jabber-roster-line-format " %c %-25n %u %-8s  %S"
     jabber-roster-show-bindings nil
     jabber-roster-show-title nil
     jabber-history-dir (concat user-emacs-directory "jabber-history")
     jabber-history-enabled t
     jabber-use-global-history nil)

    (add-hook 'jabber-chat-mode-hook 'psv/jabber-hook t))
  :bind (("C-x C-;" . jabber-activity-switch-to)
         ("<C-f10>" . jabber-switch-to-roster-buffer)))

;;; erc
(use-package erc
  :ensure t
  :init
  (progn
    (setq erc-hide-list '("JOIN" "PART" "QUIT")
          erc-prompt-for-nickserv-password nil
          erc-save-buffer-on-part t
	  erc-autojoin-channels-alist '(("irc.freenode.net" "#emacs" "#lisp")))

    (require 'erc-log)
    (setq erc-log-channels-directory "~/.irc/logs/")
    (setq erc-save-buffer-on-part t)

    ;; ;; load file with password
    ;; (load "~/.ercpass")
    ;; (setq erc-nickserv-passwords
    ;; 	  `((freenode     (("bioh" . ,freenode-bioh-pass)))))

    (require'erc-services)
    (erc-services-mode 1)
    (setq erc-prompt-for-nickserv-password nil)))

;;; ag
(use-package ag
  :ensure t
  :init
  (progn
    (setq ag-highlight-search t)
    (setq ag-reuse-window t)
    (setq ag-reuse-buffers t)))

;;; keyfreq
(use-package keyfreq
  :ensure t
  :init
  (progn
    (keyfreq-mode 1)
    (keyfreq-autosave-mode 1)))

;;; hippie expand
(use-package hippie-exp
  :ensure t
  :init
  (progn
    (setq hippie-expand-try-functions-list
	  '(try-complete-file-name-partially
	    try-complete-file-name
	    try-expand-all-abbrevs
	    try-expand-list
	    try-expand-line
	    try-expand-dabbrev
	    try-expand-dabbrev-all-buffers
	    try-expand-dabbrev-from-kill
	    try-complete-lisp-symbol-partially
	    try-complete-lisp-symbol)))
  :bind
  (("s-/" . hippie-expand)))

;;; occur
;; recenter after jump to occurence
(defun psv/occur-find-occurence-hook ()
  (recenter-top-bottom))

(defun noccur-dired (regexp &optional nlines)
  "Perform `multi-occur' with REGEXP in all dired marked files.
When called with a prefix argument NLINES, display NLINES lines before and after."
  (interactive (occur-read-primary-args))
  (multi-occur (mapcar #'find-file (dired-get-marked-files)) regexp nlines))

(defun noccur-project (regexp &optional nlines)
  "Perform `multi-occur' in the current project files."
  (interactive (occur-read-primary-args))
  (let* ((directory (read-directory-name "Search in directory: "))
         (files (if (and directory (not (string= directory (projectile-project-root))))
                    (projectile-files-in-project-directory directory)
                  (projectile-current-project-files)))
         (buffers (mapcar #'find-file
                          (mapcar #'(lambda (file)
                                      (expand-file-name file (projectile-project-root)))
                                  files))))
    (multi-occur buffers regexp nlines)))

(use-package occur
  :init
  (progn
    (add-hook 'occur-mode-find-occurrence-hook 'psv/occur-find-occurence-hook))
  :bind
  (("M-s o" . occur)))

;;; cpp
;; common c settings
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(setq c-default-style "k&r"
      c-basic-offset 4)

(setq gdb-many-windows t	     ; use gdb-many-windows by default
      gdb-show-main t)               ; non-nil means display source file containing the main routine at startup


;;; elfeed
(use-package elfeed
  :ensure t
  :init
  (progn
    (setq elfeed-feeds '("http://www.reddit.com/r/emacs/.rss"
                     "http://planet.emacsen.org/ru/atom.xml"
                     "http://planet.emacsen.org/atom.xml"
                     "https://news.ycombinator.com/rss"
                     "https://meduza.io/rss/all"))))

;;; irfc
(use-package irfc
  :ensure t
  :init
  (progn
    (setq irfc-directory "~/workspace/rfc/"
	  irfc-assoc-mode t)))


;;; utils
(defun psv/sort-lines (beg end)
  (interactive "*r")
  (save-excursion
    (sort-lines nil beg end)))

(defun psv/diff-current-buffer-with-file ()
  "Diff current buffer with associated file"
  (interactive)
  (diff-buffer-with-file (current-buffer)))

(defun psv/flush-lines-like-at-the-point ()
  (interactive)
  (let ((line (buffer-substring-no-properties (line-beginning-position)
                                            (line-end-position))))
       (save-excursion
         (cond ((= 0 (length line))     ; empty string check
                (message "Trying to delete empty lines. Be careful."))
               ((string-match "[ \t]+$" line) ; whitespace string check
                (when (yes-or-no-p "Do you really want to flush whitespace strings?")
                  (beginning-of-buffer)
                  (flush-lines (regexp-quote line))))
               (t                       ; common case
                (beginning-of-buffer)
                (flush-lines (regexp-quote line)))))))

(defun randomize-region (beg end)
  (interactive "r")
  (if (> beg end)
      (let (mid) (setq mid end end beg beg mid)))
  (save-excursion
    ;; put beg at the start of a line and end and the end of one --
    ;; the largest possible region which fits this criteria
    (goto-char beg)
    (or (bolp) (forward-line 1))
    (setq beg (point))
    (goto-char end)
    ;; the test for bolp is for those times when end is on an empty
    ;; line; it is probably not the case that the line should be
    ;; included in the reversal; it isn't difficult to add it
    ;; afterward.
    (or (and (eolp) (not (bolp)))
        (progn (forward-line -1) (end-of-line)))
    (setq end (point-marker))
    (let ((strs (shuffle-list
                 (split-string (buffer-substring-no-properties beg end)
                             "\n"))))
      (delete-region beg end)
      (dolist (str strs)
        (insert (concat str "\n"))))))

(defun shuffle-list (list)
  "Randomly permute the elements of LIST.
All permutations equally likely."
  (let ((i 0)
  j
  temp
  (len (length list)))
    (while (< i len)
      (setq j (+ i (random (- len i))))
      (setq temp (nth i list))
      (setcar (nthcdr i list) (nth j list))
      (setcar (nthcdr j list) temp)
      (setq i (1+ i))))
  list)

(defun psv/align-by-spaces ()
  "Align selection by spaces."
  (interactive)
  (align-regexp (region-beginning) (region-end) "\\(\\s-*\\) " -1 0 t))

(defun psv/goto-match-paren (arg)
  "Go to the matching  if on (){}[], similar to vi style of % "
  (interactive "p")
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc
  (cond ((looking-at "[\[\(\{]") (forward-sexp))
        ((looking-back "[\]\)\}]" 1) (backward-sexp))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (backward-sexp))
        ((looking-back "[\[\(\{]" 1) (backward-char) (forward-sexp))
        (t nil)))

(defun uniquify-region-lines (beg end)
  "Remove duplicate adjacent lines in region."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))

(defun uniquify-buffer-lines ()
  "Remove duplicate adjacent lines in the current buffer."
  (interactive)
  (uniquify-region-lines (point-min) (point-max)))

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(bind-key "C-S-<up>" 'move-line-up)
(bind-key "C-S-<down>" 'move-line-down)

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer ()
  "Indents a region if selected, otherwise the whole buffer"
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region"))
      (progn
        (indent-buffer)
        (message "Indented buffer")))))

(defun copy-file-name-to-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun delete-file-and-buffer ()
  "Kills the current buffer and deletes the file it is visiting"
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (delete-file filename)
      (message "Deleted file %s" filename)))
  (kill-buffer))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

;; increment/decrement number at point
(defun psv/increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

(defun psv/decrement-number-decimal (&optional arg)
  (interactive "p*")
  (psv/increment-number-decimal (if arg (- arg) -1)))

(bind-key "C-c +" 'psv/increment-number-decimal)
(bind-key "C-c -" 'psv/decrement-number-decimal)

(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(bind-key "S-RET" 'smart-open-line)
(bind-key "C-S-RET" 'smart-open-line-above)

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;; other
(bind-key "M--" 'psv/goto-match-paren)
(bind-key "C-M-=" 'psv/diff-current-buffer-with-file)

;;; misc

;; stop asking whether to save newly added abbrev when quitting emacs
(setq save-abbrevs nil)

;; show time in mode line
(display-time-mode 1)
(setq display-time-format "%H:%M")

;;; wgrep / wgrep-ag
(use-package wgrep :ensure t)
(use-package wgrep-ag :ensure t)

;;; python
;; pip install virtualenv
(use-package jedi
  :ensure t
  :init
  (progn
    (add-hook 'python-mode-hook 'jedi:setup)
    (setq jedi:complete-on-dot t)
    (setq jedi:use-shortcuts t)))

;; (jedi:install-server)

;;; golang
;; look at ~/.emacs.d/sh/go/sh

(use-package go-mode
  :ensure t
  :init
  (progn
    (setenv "GOPATH" (concat (expand-file-name "~/workspace/go/")))
    (add-hook 'go-mode-hook 'go-eldoc-setup)
    (add-hook 'go-mode-hook 'psv/go-local-bindings)
    (setq gofmt-command "goimports")))

(use-package go-autocomplete
  :ensure t)

(use-package go-eldoc
  :ensure t)

(use-package golint
  :ensure t)

(defun psv/go-local-bindings()
  (progn
    (setq tab-width 4)
    (local-set-key (kbd "M-.") 'godef-jump)
    (local-set-key (kbd "M-,") 'pop-tag-mark)
    (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
    (local-set-key (kbd "C-c i") 'go-goto-imports)))


;;; Environment setup
(defun psv/fs ()
  (make-directory "~/Books" t)
  (make-directory "~/Distributives" t)
  (make-directory "~/Downloads" t)
  (make-directory "~/Dropbox" t)
  (make-directory "~/bin" t)
  (make-directory "~/github" t)
  (make-directory "~/src" t)
  (make-directory "~/workspace" t))

(defun psv/personal-fs ()
  (make-directory "~/Auidioworks" t)
  (make-directory "~/Documents" t)
  (make-directory "~/Music" t)
  (make-directory "~/Music/heap" t)
  (make-directory "~/Music/lossy" t)
  (make-directory "~/Music/lossless" t)
  (make-directory "~/Music/soundtracks" t)
  (make-directory "~/Videos" t)
  (make-directory "~/Videos/audio" t)
  (make-directory "~/Videos/emacs" t)
  (make-directory "~/Videos/guitar" t)
  (make-directory "~/Videos/movies" t)
  (make-directory "~/Videos/programming" t))

(defvar psv/executables '("bash" "git" "ag" "go" "global" "wget"
			  "gocode" "godef" "goimports" "golint" ; some golang tools
			  "plantuml.jar" "ditaa.jar" "dot"
			  "exercism"))

(defun psv/check-executable (program)
  (unless (executable-find program)
    (warn "'%s' is not in $PATH or not installed, try ~/.emacs.d/sh/%s.sh" program program)))

(defun psv/check-executables ()
  (mapcar 'psv/check-executable psv/executables))

(psv/fs)
(psv/check-executables)
