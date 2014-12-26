;;; cedet-config.el

;; If using cedet-devel, srecode-map.el can be
;; moved by setting srecode-map-save-file variable, but it has to be
;; before loading cedet-devel-load.el.
(setq srecode-map-save-file (concat tempfiles-dir "srecode-map.el"))

(load-file (expand-file-name "~/github/cedet-bzr/cedet-devel-load.el"))
(add-to-list 'load-path (expand-file-name "~/github/cedet-bzr/contrib/"))

(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)

(setq semanticdb-default-save-directory (concat tempfiles-dir ".semanticdb"))
(setq ede-project-placeholder-cache-file (concat tempfiles-dir "ede-projects.el"))

(ede-enable-generic-projects)

(semantic-mode 1)
(global-ede-mode 1)

(defconst my-cc-style
  '("cc-mode"
    (c-offsets-alist . ((innamespace . [0])))))

(c-add-style "my-cc-mode" my-cc-style)

(require 'eassist)

(defvar semantic-tags-location-ring (make-ring 80))

(defun psv/semantic-goto-definition (point)
  "Goto definition using semantic-ia-fast-jump
save the pointer marker if tag is found"
  (interactive "d")
  (condition-case err
      (progn
        (ring-insert semantic-tags-location-ring (point-marker))
        (semantic-ia-fast-jump point))
    (error
     ;;if not found remove the tag saved in the ring
     (set-marker (ring-remove semantic-tags-location-ring 0) nil nil)
     (signal (car err) (cdr err)))))

(defun psv/semantic-pop-tag-mark ()
  "popup the tag save by semantic-goto-definition"
  (interactive)
  (if (ring-empty-p semantic-tags-location-ring)
      (message "%s" "No more tags available")
    (let* ((marker (ring-remove semantic-tags-location-ring 0))
              (buff (marker-buffer marker))
                 (pos (marker-position marker)))
      (if (not buff)
            (message "Buffer has been deleted")
        (switch-to-buffer buff)
        (goto-char pos))
      (set-marker marker nil nil))))

(defun psv/cedet-hook ()
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\M-." 'psv/semantic-goto-definition)
  (local-set-key "\M-," 'psv/semantic-pop-tag-mark)
  (local-set-key "\C-ct" 'eassist-switch-h-cpp)
  (local-set-key "\C-ce" 'eassist-list-methods)
  (local-set-key "\C-c\C-r" 'semantic-symref)
  (local-set-key "\C-cs" 'semantic-ia-show-summary))

(add-hook 'c-mode-common-hook 'psv/cedet-hook)

(require 'nda)

(provide 'cedet-config)
