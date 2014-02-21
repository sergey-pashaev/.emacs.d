;;; eshell-config.el

(setq eshell-cmpl-cycle-completions nil
      eshell-directory-name (concat dotfiles-dir "eshell/")
      eshell-save-history-on-exit t
      eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

(defalias 'e 'find-file)
(defalias 'E 'find-file-other-window)

(setenv "GOPATH" (expand-file-name "~/workspace/go/"))
(setenv "PATH" (concat (getenv "PATH") ":" (getenv "GOPATH") "bin"))

(require-or-install 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(provide 'eshell-config)
