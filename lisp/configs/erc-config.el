;;; erc-config.el

(require-or-install 'erc)

(setq erc-autojoin-channels-alist
      '(("irc.freenode.net" "#emacs" "#lisp")))

(require 'erc-log)

(setq erc-log-channels-directory "~/.irc/logs/")
(setq erc-save-buffer-on-part t)

;; load file with password
(load "~/.ercpass")

(require'erc-services)
(erc-services-mode 1)
(setq erc-prompt-for-nickserv-password nil)

(setq erc-nickserv-passwords
      `((freenode     (("bioh" . ,freenode-bioh-pass)))))

(setq erc-hide-list '("JOIN" "PART" "QUIT"))

(provide 'erc-config)
