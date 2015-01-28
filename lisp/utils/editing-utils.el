;;; editing-utils.el

;; insert an empty line after the current
;; line and position the cursor on its beginning,
;; according to the current mode
(defun insert-empty-line ()
  (interactive)
  (move-end-of-line nil)
  (open-line 1)
  (next-line 1)
  (indent-according-to-mode))

;; mimic popular IDEs binding, note that it doesn't work in a terminal session
(global-set-key [(shift return)] 'insert-empty-line)

;; moving lines
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(global-set-key [(control shift up)] 'move-line-up)

(global-set-key [(control shift down)] 'move-line-down)

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

(defun annotate-todo ()
  "put fringe marker on TODO: lines in the curent buffer"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "TODO:" nil t)
      (let ((overlay (make-overlay (- (point) 5) (point))))
        (overlay-put overlay
                     'before-string
                     (propertize (format "A")
                                 'display '(left-fringe right-triangle)))))))

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

;; TODO doesn't work with uniquify
(defun rename-file-and-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (message "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file name new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)))))))

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

(defun insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

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

(defun psv/replace-and-add-namespace (symbol namespace)
  (interactive (list (read-string (format "Symbol (%s): " (thing-at-point 'symbol))
                                  nil nil (thing-at-point 'symbol))
                     (read-string "Namespace: ")))
  (backward-char (length symbol))
  (query-replace symbol (concat namespace "::" symbol)))

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

(global-set-key (kbd "C-c +") 'psv/increment-number-decimal)
(global-set-key (kbd "C-c -") 'psv/decrement-number-decimal)

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

(provide 'editing-utils)
