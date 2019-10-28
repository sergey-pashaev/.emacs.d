;;; base-functions.el --- Base functions file

;;; Commentary:

;;; Code:
(defun psv/server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;; russian input
(defun psv/toggle-russian-input-method ()
  "Toggle internal input method between default and russian."
  (interactive)
  (if (string= current-input-method "russian-computer")
      (deactivate-input-method)
      (set-input-method "russian-computer")))

(defun psv/beginning-of-line-dwim ()
  "Cycle point position between first non-ws character and start of the line."
  (interactive)
  (let ((start-position (point)))
    ;; see if going to the beginning of the line changes our position
    (move-beginning-of-line nil)
    (when (= (point) start-position)
      ;; we're already at the beginning of the line, so go to the
      ;; first non-whitespace character
      (back-to-indentation))))

(defun psv/diff-current-buffer-with-file ()
  "Diff current buffer with associated file."
  (interactive)
  (diff-buffer-with-file (current-buffer)))

(defun psv/flush-lines-like-at-the-point ()
  "Flush lines like at the point from buffer."
  (interactive)
  (let ((line (buffer-substring-no-properties (line-beginning-position)
                                              (line-end-position))))
    (save-excursion
      (cond ((= 0 (length line))     ; empty string check
             (message "Trying to delete empty lines. Be careful."))
            ((string-match "[ \t]+$" line) ; whitespace string check
             (when (yes-or-no-p "Do you really want to flush whitespace strings?")
               (goto-char (point-min))
               (flush-lines (regexp-quote line))))
            (t                       ; common case
             (goto-char (point-min))
             (flush-lines (regexp-quote line)))))))

(defun psv/shuffle-region (beg end)
  "Shuffle lines in region from BEG to END."
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

(defun psv/goto-match-paren ()
  "Go to matching bracket if on (){}[], similar to vi-style of %."
  (interactive)
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc
  (cond ((looking-at "[\[\(\{]") (forward-sexp))
        ((looking-back "[\]\)\}]" 1) (backward-sexp))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (backward-sexp))
        ((looking-back "[\[\(\{]" 1) (backward-char) (forward-sexp))
        (t nil)))

(defun psv/uniquify-region-lines (beg end)
  "Remove duplicate adjacent lines in region from BEG to END."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))

(defun psv/uniquify-buffer-lines ()
  "Remove duplicate adjacent lines in current buffer."
  (interactive)
  (psv/uniquify-region-lines (point-min) (point-max)))

(defun psv/move-line-up ()
  "Move current line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun psv/move-line-down ()
  "Move current line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun psv/indent-buffer ()
  "Indent current buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun psv/indent-region-or-buffer ()
  "Indent region if selected, otherwise indent buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (indent-region (region-beginning) (region-end))
      (psv/indent-buffer))))

(defun psv/gn-refs ()
  "Run gn refs for current file."
  (interactive)
  (when (projectile-project-root)
    (let ((file (psv/buffer-file-path))
          (dir (concat (projectile-project-root) "src/")))
      (let ((default-directory dir)
            (program "/home/bioh/workspace/ya/depot_tools/gn")
            (cmd (format " refs out/Debug/ --all %s" file)))
        (start-process "psv/gn-ref-proc" "*psv/gn-ref*" program "refs" "out/Debug/" "-all" file)
        (message "gn refs started...")
        (switch-to-buffer-other-window "*psv/gn-ref*")))))

(defconst *psv/chromium-project-root-path*
  "~/workspace/ya/chromium/src/"
  "Chromium project root path.")

(defun psv/put-to-clipboard (str)
  "Put STR into clipboard."
  (when str
    (with-temp-buffer
      (insert str)
      (clipboard-kill-region (point-min) (point-max)))))

(defun psv/buffer-file-path ()
  "Return current buffer filename."
  (interactive)
  (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name)))

(defun psv/projectile-buffer-relative-path ()
  "Return project-relative path."
  (interactive)
  (let ((path (substring (psv/buffer-file-path)
                         (length (projectile-project-root)))))
    (if (string= (projectile-project-root) (expand-file-name *psv/chromium-project-root-path*))
        (concat "src/" path)
      path)))

(defun psv/make-include-statement ()
  "Generate include statement for current file."
  (let ((path (if (string= (projectile-project-root)
                           (expand-file-name *psv/chromium-project-root-path*))
                  (substring (psv/projectile-buffer-relative-path)
                             (length "src/"))
                (psv/projectile-buffer-relative-path))))
    (if (s-starts-with? "src/" path)
        (format "#include \"%s\"" (substring path (length "src/")))
      (format "#include \"%s\"" path))))

(defun psv/copy-include-statement-to-clipboard ()
  "Put the current file include statement name to clipboard."
  (interactive)
  (let ((include (psv/make-include-statement)))
    (psv/put-to-clipboard include)
    (message include)))

(defun psv/visit-file-in-other-project ()
  "Visit file in other project with same relative path as current buffer.
With passed unversal argument it visits file in other
window."
  (interactive)
  (let ((projects (projectile-relevant-known-projects))
        (position (point))
        (path (psv/projectile-buffer-relative-path)))
    (if projects
        (projectile-completing-read
         "Switch to file in project: " projects
         :action (lambda (project)
                   (let ((filepath (if (string= project *psv/chromium-project-root-path*)
                                       (concat "~/workspace/ya/chromium/" path)
                                     (concat project path))))
                     (if (f-exists? filepath)
                         (if current-prefix-arg
                             (progn
                               (delete-other-windows)
                               (split-window-right)
                               (other-window 1)
                               (find-file filepath)
                               (goto-char position)
                               (other-window 1))
                           (progn
                             (find-file filepath)
                             (goto-char position)))
                       (user-error (format "path:%s doesn't exist" path))))))
      (user-error "There are no open projects"))))

(defun psv/diff-with-same-file-in-other-project ()
  "Diff current file with file on same path in other project."
  (interactive)
  (let ((projects (projectile-relevant-known-projects))
        (project (projectile-project-root))
        (filepath (psv/projectile-buffer-relative-path)))
    (if projects
        (projectile-completing-read
         "Diff with file in project: " projects
         :action (lambda (other-project)
                   (let ((other-filepath (if (string= other-project *psv/chromium-project-root-path*)
                                             (concat (expant-file-name *psv/chromium-project-root-path*)
                                                     (substring filepath (length "src/")))
                                           (concat other-project filepath))))
                     (if (f-exists? other-filepath)
                         (ediff (concat project filepath) other-filepath)
                       (user-error (format "path:%s doesn't exist" other-filepath))))))
      (user-error "There are no open projects"))))

(defun psv/copy-file-name-to-clipboard ()
  "Put the current file name to clipboard."
  (interactive)
  (let ((filename (psv/buffer-file-path)))
    (psv/put-to-clipboard filename)
    (message filename)))

(defun psv/duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
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

(defun psv/delete-file-and-buffer ()
  "Kill current buffer and delete associated file."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (delete-file filename)
      (message "%s is deleted." filename)))
  (kill-buffer))

(defun psv/untabify-buffer ()
  "Untabify current buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun psv/cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (psv/untabify-buffer)
  (psv/indent-buffer)
  (delete-trailing-whitespace))

(defun psv/increment-number-decimal (&optional arg)
  "Increment number forward from point by ARG."
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
  "Decrement number forward from point by ARG."
  (interactive "p*")
  (psv/increment-number-decimal (if arg (- arg) -1)))

(defconst psv/integer-bounds-rx "[^0-9A-Fa-fxX]")

(defun psv/bounds-of-integer-at-point ()
  "Return bounds (begin . end) of number at point or nil."
  (let (beg end)
    (save-excursion
      ;; set point to buffer begin/end if search fails
      (condition-case nil
          (progn
            (re-search-backward psv/integer-bounds-rx)
            (forward-char)
            (setq beg (point)))
        (error (setq beg (point-min))))
      (condition-case nil
          (progn
            (re-search-forward psv/integer-bounds-rx)
            (backward-char)
            (setq end (point)))
        (error (setq end (point-max)))))
    ;; check bound region is not empty
    (when (< beg end)
      (cons beg end))))

(defconst psv/integer-bases '((16 . "^0[xX]\\([0-9A-Fa-f]+\\)$")
                              (8  . "^0\\([0-9]+\\)$")
                              (2  . "^0[bB]\\([01]+\\)$")
                              (10 . "^\\([0-9]+\\)$"))
  "List of integer BASE to REGEX (w/ submatch group 1) mappings.")

(defun psv/integer-n-base-from-bounds (bounds)
  "Return parsed integer & base (num . base) from given BOUNDS."
  (let (str base)
    (setq str (buffer-substring-no-properties (car bounds) (cdr bounds)))
    (setq base (seq-find (lambda (e)
                           (string-match (cdr e) str))
                         psv/integer-bases))
    (when base
      (setq base (car base)) ; get base number, drop rx
      (cons (string-to-number (match-string 1 str) base) base))))

(defun psv/next-integer-base (base)
  "Return next interger base for BASE in psv/integer-bases."
  (let (pos)
    ;; find given BASE in psv/integer-bases
    (setq pos (seq-position psv/integer-bases
                            base
                            (lambda (x y) (= (car x) y)))) ; get base number, drop rx
    (when pos
      ;; get next base in list in cyclic manner
      (car (seq-elt psv/integer-bases
                    (mod (+ pos 1)
                         (seq-length psv/integer-bases)))))))

(defun psv/integer-to-binary-string (i)
  "Convert an integer I into it's binary representation in string format."
  (let ((res ""))
    (while (not (= i 0))
      (setq res (concat (if (= 1 (logand i 1)) "1" "0") res))
      (setq i (lsh i -1)))
    (if (string= res "")
        (setq res "0"))
    res))

(defun psv/format-integer-in-base (num base)
  "Return string representation of NUM in BASE base."
  (cond
   ((= base 2)  (format "0b%s" (psv/integer-to-binary-string num)))
   ((= base 8)  (format "0%o"  num))
   ((= base 10) (format "%d"   num))
   ((= base 16) (format "0x%x" num))))

(defun psv/cycle-base-of-integer-at-point ()
  "Cycle through different integer bases for number at point."
  (interactive)
  (let (bounds num-base)
    (setq bounds (psv/bounds-of-integer-at-point))
    (when bounds
      (setq num-base (psv/integer-n-base-from-bounds bounds))
      (when num-base
        (delete-region (car bounds) (cdr bounds))
        (insert (psv/format-integer-in-base (car num-base)
                                            (psv/next-integer-base (cdr num-base))))))))

(defun psv/sudo-edit (&optional arg)
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

;; http://pragmaticemacs.com/emacs/aligning-text/
;; https://github.com/WaYdotNET/.emacs.d/blob/master/function.el#L128
(defun psv/align-whitespace (start end)
  "Align columns by whitespace in region from START to END."
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))

(defun psv/align-& (start end)
  "Align columns by ampersand in region from START to END."
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)&" 1 1 t))

(defun psv/line-reference ()
  "Copy to `kill-ring` reference to current line."
  (interactive)
  (kill-new (format "%s:%d:%d:%s"
                    (buffer-name)
                    (line-number-at-pos (point))
                    (current-column)
                    (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position)))
            nil))

(defun psv/disable-final-newline ()
  "Disable final newline for current buffer."
  (interactive)
  (set (make-local-variable 'require-final-newline) nil))

(defun psv/enable-final-newline ()
  "Enable final newline for current buffer."
  (interactive)
  (set (make-local-variable 'require-final-newline) t))

;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first.  Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

(defun psv/todo ()
  "Go to end of personal todo list."
  (interactive)
  (find-file (expand-file-name "~/Dropbox/org/gtd/tasks.org"))
  (goto-char (point-max)))

(defun psv/work-todo-open (filepath)
  "Open todo list at FILEPATH."
  (if (f-exists? filepath)
      (progn
        (find-file filepath)
        (goto-char (point-max)))
    (message "%s not exist" filepath)))

(defun psv/work-todo ()
  "Go to end of work todo list."
  (interactive)
  (let ((global-todo (expand-file-name "~/Dropbox/org/work/tasks.org"))
        (project-todo (expand-file-name "todo.org" (projectile-project-root)))
        (in-project (not (string= (projectile-project-name) "-"))))
    (if (and in-project (f-exists? project-todo))
        (psv/work-todo-open project-todo)
      (progn
        (psv/work-todo-open global-todo)
        (message "%s not exist. Open global todo list instead." project-todo)))))

(defun psv/unfill-paragraph (&optional region)
  "Take a REGION and transform it into single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(use-package popup :ensure t)

(defvar psv/cdecl-program "cdecl"
  "The program to run.")

(defun psv/cdecl-explain (expr)
  "Run cdecl 'explain' on EXPR."
  (interactive "sExpr: \n")
  (let ((cmd (concat
              "echo 'explain " expr "' |"
              psv/cdecl-program)))
    (popup-tip (shell-command-to-string cmd))))

(defun psv/cdecl-explain-region (beg end)
  "Run cdecl 'explain' in region from BEG to END."
  (interactive "r")
  (psv/cdecl-explain (buffer-substring-no-properties beg end)))

(defun psv/ssh-hosts ()
  "Return lists of hosts from ~/.ssh/config."
  (split-string (shell-command-to-string
                 "cat ~/.ssh/config | grep 'Host ' | awk -F' ' '{ print $2 }'")))

(defun psv/dired-ssh ()
  "Run dired at selected host."
  (interactive)
  (dired (format "/ssh:%s:/"
                 (ido-completing-read "Host? " (psv/ssh-hosts)))))

(defun psv/dired-appl()
  "Run dired at selected appl directory."
  (interactive)
  (let* ((appl-dir (expand-file-name "~/workspace/Swift/appl/"))
         (contents (directory-files appl-dir)))
    (dired (concat appl-dir
                   (ido-completing-read "APPL? "
                                        (remove-if-not (lambda (name) (file-directory-p (concat appl-dir name)))
                                                       contents))))))
(defun psv/newline-below()
  "Insert & indent new line below current line."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun psv/newline-above()
  "Insert & indent new line above current line."
  (interactive)
  (beginning-of-line)
  (newline-and-indent)
  (forward-line -1) ; previous line
  (indent-according-to-mode))

(defconst psv/chromium-code-search-format "https://cs.chromium.org/search/?q=%s&sq=package:chromium&type=cs"
  "Chromium code search format, %s = SEARCH TERM.")

(defun psv/chromium-code-search-at-point ()
  "Search symbol at point at chromium sources."
  (interactive
   (let ((term (read-string "Search term: " (thing-at-point 'symbol))))
     (browse-url-default-browser (format psv/chromium-code-search-format term)))))

(provide 'base-functions)
;;; base-functions.el ends here
