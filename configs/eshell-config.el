;;; eshell-config.el

(setq eshell-cmpl-cycle-completions nil
      eshell-directory-name (concat dotfiles-dir "eshell/")
      eshell-save-history-on-exit t
      eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

(defalias 'e 'find-file)
(defalias 'E 'find-file-other-window)

(provide 'eshell-config)
