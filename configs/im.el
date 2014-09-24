;;; jabber
(require-package 'jabber)
(require 'jabber)

(defun psv/jabber-hook ()
  (define-key jabber-chat-mode-map (kbd "RET") 'jabber-chat-buffer-send)
  (define-key jabber-chat-mode-map (kbd "<C-return>") 'newline))

(use-package jabber
  :init
  (progn
    (setq
     jabber-chat-buffer-show-avatar nil
     jabber-roster-line-format " %c %-25n %u %-8s  %S"
     jabber-roster-show-bindings nil
     jabber-roster-show-title nil
     jabber-history-dir "~/.emacs.d/jabber-history"
     jabber-history-enabled t
     jabber-use-global-history nil)

    (add-hook 'jabber-chat-mode-hook 'psv/jabber-hook t))
  :bind (("C-x C-;" . jabber-activity-switch-to)
         ("<C-f10>" . jabber-switch-to-roster-buffer)))

;;; erc
(require-package 'erc)
(require 'erc)

(use-package erc
  :init
  (progn
    (setq erc-hide-list '("JOIN" "PART" "QUIT")
          erc-prompt-for-nickserv-password nil
          erc-save-buffer-on-part t)))
