;;; eshell-config.el

(setq eshell-cmpl-cycle-completions nil
      eshell-directory-name (concat dotfiles-dir "eshell/")
      eshell-save-history-on-exit t
      eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

(defalias 'e 'find-file)
(defalias 'E 'find-file-other-window)

;; (require-or-install 'exec-path-from-shell)
;; (exec-path-from-shell-initialize)

;; from ~/.bashrc
;; export MYGOROOT=$HOME/workspace/go
;; export STDGOROOT=$HOME/workspace/go-snapshot/go
;; export GOPATH=$MYGOROOT:$STDGOROOT
;; export PATH=$PATH:$MYGOROOT/bin

;; (setenv "GOPATH" (concat (expand-file-name "~/workspace/go/")
;;                          ":"
;;                          (expand-file-name "~/workspace/go-snapshot/go/")))

(provide 'eshell-config)
