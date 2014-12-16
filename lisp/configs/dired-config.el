;;; dired-config.el

(add-hook 'dired-mode-hook
          (lambda ()
            (hl-line-mode 1)))

(provide 'dired-config)
