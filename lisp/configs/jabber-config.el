;;; jabber-config.el

(require-or-install 'jabber)

(setq jabber-chat-buffer-show-avatar nil)
(setq jabber-roster-line-format " %c %-25n %u %-8s  %S")
(setq jabber-roster-show-bindings nil)
(setq jabber-roster-show-title nil)
(setq jabber-history-dir "~/.emacs.d/tmp/jabber-history")
(setq jabber-history-enabled t)
(setq jabber-use-global-history nil)

(add-hook 'jabber-chat-mode-hook
          (lambda ()
            (define-key jabber-chat-mode-map (kbd "RET") 'jabber-chat-buffer-send)
            (define-key jabber-chat-mode-map (kbd "<C-return>") 'newline))
          t)

(global-set-key (kbd "C-x C-;") 'jabber-activity-switch-to)
(global-set-key (kbd "<C-f10>") 'jabber-switch-to-roster-buffer)

(provide 'jabber-config)
