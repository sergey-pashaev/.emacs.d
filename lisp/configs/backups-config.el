;;; backups-config.el

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . "~/.emacs.d/tmp/backups")))

(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/tmp/autosave" t)))


(setq auto-save-list-file-prefix "~/.emacs.d/tmp/auto-save-list/.saves-")

(provide 'backups-config)
